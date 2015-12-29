{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Offset.Splices where

import Control.Monad.State
import           Control.Applicative     ((<|>))
import           Control.Lens            hiding (children)
import           Control.Monad           (void, sequence)
import           Control.Monad.Trans     (lift, liftIO)
import           Data.Aeson              hiding (decode, encode)
import qualified Data.Attoparsec.Text    as A
import           Data.Char               (toUpper)
import qualified Data.HashMap.Strict     as M
import           Data.IntSet             (IntSet)
import qualified Data.IntSet             as IntSet
import           Data.Map.Syntax
import           Data.Maybe              (fromJust, fromMaybe)
import           Data.Monoid
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import qualified Text.XmlHtml            as X

import           Web.Offset.Field
import           Web.Offset.Posts
import           Web.Offset.Queries
import           Web.Offset.Types
import           Web.Offset.Utils

wordpressSplices :: (MonadState s m, MonadIO m) =>
                    Wordpress b
                 -> [Field m]
                 -> m Text
                 -> WPLens b s m
                 -> Splices (Splice m)
wordpressSplices wp extraFields getURI wpLens =
  do "wpPosts" ## wpPostsSplice wp extraFields wpLens
     "wpPostByPermalink" ## wpPostByPermalinkSplice extraFields getURI wpLens
     "wpPage" ## wpPageSplice wpLens
     "wpNoPostDuplicates" ## wpNoPostDuplicatesSplice wpLens
     "wp" ## wpPrefetch wp

wpPostsSplice :: (MonadState s m, MonadIO m) =>
                 Wordpress b
              -> [Field m]
              -> WPLens b s m
              -> Splice m
wpPostsSplice wp extraFields wpLens =
  do promise <- newEmptyPromise
     outputChildren <- manyWithSplices runChildren (postSplices extraFields)
                                                   (getPromise promise)
     n <- getParamNode
     attrs' <- runAttributesRaw (X.elementAttrs n)
     tagDict <- lift $ lookupTaxDict (TaxDictKey "post_tag") wp
     catDict <- lift $ lookupTaxDict (TaxDictKey "category") wp
     return $ yieldRuntime $
       do attrs <- attrs'
          let postsQuery = parseQueryNode attrs
          let wpKey = mkWPKey tagDict catDict postsQuery
          res <- liftIO $ cachingGetRetry wp wpKey
          case decode res of
            Just posts -> do let postsW = extractPostIds posts
                             Wordpress{..} <- lift (use wpLens)
                             let postsND = take (qlimit postsQuery) . noDuplicates requestPostSet $ postsW
                             lift $ addPostIds wpLens (map fst postsND)
                             putPromise promise (map snd postsND)
                             codeGen outputChildren
            Nothing -> codeGen (yieldPureText "")
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just postSet) = filter (\(i,_) -> IntSet.notMember i postSet)

wpPostByPermalinkSplice :: (MonadState s m, MonadIO m) =>
                           [Field m]
                        -> m Text
                        -> WPLens b s m
                     -> Splice m
wpPostByPermalinkSplice extraFields getURI wpLens =
  do promise <- newEmptyPromise
     outputChildren <- withSplices runChildren (postSplices extraFields) (getPromise promise)
     return $ yieldRuntime $
       do mperma <- parsePermalink <$> lift getURI
          case mperma of
            Nothing -> codeGen (yieldPureText "")
            Just (year, month, slug) ->
              do res <- lift $ wpGetPost wpLens (PostByPermalinkKey year month slug)
                 case res of
                   Just post -> do lift $ addPostIds wpLens [fst (extractPostId post)]
                                   putPromise promise post
                                   codeGen outputChildren
                   _ -> codeGen (yieldPureText "")

wpNoPostDuplicatesSplice :: (MonadState s m, MonadIO m) =>
                            WPLens b s m
                         -> Splice m
wpNoPostDuplicatesSplice wpLens =
  return $ yieldRuntime $
    do w@Wordpress{..} <- lift $ use wpLens
       case requestPostSet of
         Nothing -> lift $ assign wpLens
                                  w{requestPostSet = (Just IntSet.empty)}
         Just _ -> return ()
       codeGen $ yieldPureText ""

wpPageSplice :: (MonadState s m, MonadIO m) =>
                WPLens b s m
             -> Splice m
wpPageSplice wpLens =
  do n <- getParamNode
     case X.getAttribute "name" n of
       Nothing -> return (yieldPureText "")
       Just slug ->
         return $ yieldRuntimeText $ do res <- lift $ wpGetPost wpLens (PageKey slug)
                                        case res of
                                          Just page -> case M.lookup "content" page of
                                                         Just (String c) -> return c
                                                         _ -> return ""
                                          _ -> return ""

postSplices :: (Functor m, Monad m) => [Field m] -> Splices (RuntimeSplice m Object -> Splice m)
postSplices extra = mconcat (map buildSplice (mergeFields postFields extra))
  where buildSplice (F n) =
          transformName n ## pureSplice . textSplice $ getText n
        buildSplice (P n splice) =
          transformName n ## \o -> splice (getText n <$> o)
        buildSplice (N n fs) = transformName n ## \o ->
                                 withSplices runChildren
                                                (mconcat $ map buildSplice fs)
                                                (unObj . M.lookup n <$> o)
        buildSplice (C n path) =
          transformName n ## bindLater $ \o -> return (textSplice (getText (last path) . traverseObject (init path)) o)
        buildSplice (CN n path fs) =
          transformName n ## \o -> withSplices runChildren
                                               (mconcat $ map buildSplice fs)
                                               (traverseObject path <$> o)
        buildSplice (M n fs) = transformName n ## \o ->
                                 manyWithSplices runChildren
                                                    (mconcat $ map buildSplice fs)
                                                    (unArray . M.lookup n <$> o)
        unObj (Just (Object o)) = o
        unObj _ = M.empty
        unArray (Just (Array v)) = map (unObj . Just) $ V.toList v
        unArray _ = []
        traverseObject [] o = o
        traverseObject (x:xs) o = traverseObject xs (unObj . M.lookup x $ o)
        getText n o = case M.lookup n o of
                        Just (String t) -> t
                        Just (Number i) -> T.pack $ show i
                        _ -> ""

-- * -- Internal -- * --

parseQueryNode :: [(Text, Text)] -> WPQuery
parseQueryNode attrs =
  mkPostsQuery (readSafe =<< lookup "limit" attrs)
               (readSafe =<< lookup "num" attrs)
               (readSafe =<< lookup "offset" attrs)
               (readSafe =<< lookup "page" attrs)
               (readSafe =<< lookup "tags" attrs)
               (readSafe =<< lookup "categories" attrs)
               (lookup "user" attrs)

mkPostsQuery :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
       -> Maybe (TaxSpecList TagType) -> Maybe (TaxSpecList CatType)
       -> Maybe Text
       -> WPQuery
mkPostsQuery l n o p ts cs us =
  WPPostsQuery{ qlimit = fromMaybe 20 l
              , qnum = fromMaybe 20 n
              , qoffset = fromMaybe 0 o
              , qpage = fromMaybe 1 p
              , qtags = fromMaybe (TaxSpecList []) ts
              , qcats = fromMaybe (TaxSpecList []) cs
              , quser = us
              }


wpPrefetch :: (MonadState s m, MonadIO m) =>
              Wordpress b
           -> Splice m
wpPrefetch wp =
  do n <- getParamNode
     childrenRes <- runChildren
     tagDict <- lift $ lookupTaxDict (TaxDictKey "post_tag") wp
     catDict <- lift $ lookupTaxDict (TaxDictKey "category") wp
     wpKeys' <- findPrefetchables tagDict catDict n
     return $ yieldRuntime $
       do wpKeys <- sequence wpKeys'
          void $ liftIO $ concurrently $ map (cachingGet wp) wpKeys
          codeGen childrenRes

findPrefetchables :: (MonadState s m, MonadIO m) =>
       (TaxSpec TagType -> TaxSpecId TagType)
    -> (TaxSpec CatType -> TaxSpecId CatType)
    -> X.Node
    -> HeistT m IO [RuntimeSplice m WPKey]
findPrefetchables tdict cdict e@(X.Element "wpPosts" attrs' children) =
  do attrs <- runAttributesRaw attrs'
     rest <- mapM (findPrefetchables tdict cdict) children
     return $ [mkWPKey tdict cdict . parseQueryNode <$> attrs] <> concat rest
findPrefetchables tdict cdict e@(X.Element "wpPage" attrs' _) =
  case lookup "name" attrs' of
    Nothing -> return []
    Just _ -> do attrs <- runAttributesRaw attrs'
                 return [PageKey . fromJust . lookup "name" <$> attrs]
findPrefetchables tdict cdict (X.Element _ _ children) =
  do rest <- mapM (findPrefetchables tdict cdict) children
     return $ concat rest
findPrefetchables _ _ _ = return []

mkWPKey :: (TaxSpec TagType -> TaxSpecId TagType)
    -> (TaxSpec CatType -> TaxSpecId CatType)
    -> WPQuery
    -> WPKey
mkWPKey tagDict catDict WPPostsQuery{..} =
  let page = if qpage < 1 then 1 else qpage
      offset = qnum * (page - 1) + qoffset
      tags = map tagDict $ unTaxSpecList qtags
      cats = map catDict $ unTaxSpecList qcats
  in PostsKey (Set.fromList $ [ NumFilter qnum , OffsetFilter offset]
               ++ map TagFilter tags ++ map CatFilter cats ++ userFilter quser)
  where userFilter Nothing = []
        userFilter (Just u) = [UserFilter u]

parsePermalink :: Text -> Maybe (Text, Text, Text)
parsePermalink = either (const Nothing) Just . A.parseOnly parser . T.reverse
  where parser = do _ <- A.option ' ' (A.char '/')
                    guls <- A.many1 (A.letter <|> A.char '-')
                    _ <- A.char '/'
                    htnom <- A.count 2 A.digit
                    _ <- A.char '/'
                    raey <- A.count 4 A.digit
                    _ <- A.char '/'
                    return (T.reverse $ T.pack raey
                           ,T.reverse $ T.pack htnom
                           ,T.reverse $ T.pack guls)

wpGetPost :: (MonadState s m, MonadIO m) => WPLens b s m -> WPKey -> m (Maybe Object)
wpGetPost wpLens wpKey =
  do wp <- use wpLens
     liftIO $ getPost wp wpKey

getPost :: Wordpress b -> WPKey -> IO (Maybe Object)
getPost Wordpress{..} wpKey = do decodePost <$> cachingGetRetry wpKey
  where decodePost :: Text -> Maybe Object
        decodePost t =
          do post' <- decodeJson t
             case post' of
              Just (post:_) -> Just post
              _ -> Nothing

transformName :: Text -> Text
transformName = T.append "wp" . snd . T.foldl f (True, "")
  where f (True, rest) next = (False, T.snoc rest (toUpper next))
        f (False, rest) '_' = (True, rest)
        f (False, rest) '-' = (True, rest)
        f (False, rest) next = (False, T.snoc rest next)

-- Move this into Init.hs (should retrieve from Wordpress data structure)
addPostIds :: (MonadState s m, MonadIO m) => WPLens b s m -> [Int] -> m ()
addPostIds wpLens ids =
  do w@Wordpress{..} <- use wpLens
     assign wpLens
            w{requestPostSet = ((`IntSet.union` (IntSet.fromList ids)) <$> requestPostSet) }
