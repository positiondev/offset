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
import           Heist.Interpreted
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
  do n <- getParamNode
     attrs <- runAttributes (X.elementAttrs n)
     tagDict <- liftIO $ lookupTaxDict (TaxDictKey "post_tag") wp
     catDict <- liftIO $ lookupTaxDict (TaxDictKey "category") wp
     let postsQuery = parseQueryNode attrs
     let wpKey = mkWPKey tagDict catDict postsQuery
     res <- liftIO $ cachingGetRetry wp wpKey
     case decode res of
       Just posts -> do let postsW = extractPostIds posts
                        Wordpress{..} <- lift (use wpLens)
                        let postsND = take (qlimit postsQuery) . noDuplicates requestPostSet $ postsW
                        lift $ addPostIds wpLens (map fst postsND)
                        mapSplices (runChildrenWith . postSplices extraFields)
                                   (map snd postsND)
       Nothing -> return []
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just postSet) = filter (\(i,_) -> IntSet.notMember i postSet)

wpPostByPermalinkSplice :: (MonadState s m, MonadIO m) =>
                           [Field m]
                        -> m Text
                        -> WPLens b s m
                     -> Splice m
wpPostByPermalinkSplice extraFields getURI wpLens =
  do mperma <- parsePermalink <$> lift getURI
     case mperma of
       Nothing -> return []
       Just (year, month, slug) ->
         do res <- lift $ wpGetPost wpLens (PostByPermalinkKey year month slug)
            case res of
              Just post -> do lift $ addPostIds wpLens [fst (extractPostId post)]
                              runChildrenWith (postSplices extraFields post)
              _ -> return []

wpNoPostDuplicatesSplice :: (MonadState s m, MonadIO m) =>
                            WPLens b s m
                         -> Splice m
wpNoPostDuplicatesSplice wpLens =
  do w@Wordpress{..} <- lift $ use wpLens
     case requestPostSet of
       Nothing -> lift $ assign wpLens
                                w{requestPostSet = (Just IntSet.empty)}
       Just _ -> return ()
     return []

wpPageSplice :: (MonadState s m, MonadIO m) =>
                WPLens b s m
             -> Splice m
wpPageSplice wpLens =
  do n <- getParamNode
     case X.getAttribute "name" n of
       Nothing -> return []
       Just slug ->
         do res <- lift $ wpGetPost wpLens (PageKey slug)
            return [X.TextNode $ case res of
                                   Just page -> case M.lookup "content" page of
                                                  Just (String c) -> c
                                                  _ ->  ""
                                   _ -> ""]

postSplices :: (Functor m, Monad m) => [Field m] -> Object -> Splices (Splice m)
postSplices extra object = mconcat (map (buildSplice object) (mergeFields postFields extra))
  where buildSplice o (F n) =
          transformName n ## textSplice $ getText n o
        buildSplice o (P n splice) =
          transformName n ## splice $ getText n o
        buildSplice o (N n fs) =
          transformName n ## runChildrenWith
                                (mconcat $ map (buildSplice (unObj . M.lookup n $ o)) fs)
        buildSplice o (C n path) =
          transformName n ## textSplice (getText (last path) . traverseObject (init path) $ o)
        buildSplice o (CN n path fs) =
          transformName n ## runChildrenWith
                               (mconcat $ map (buildSplice (traverseObject path o)) fs)
        buildSplice o (M n fs) =
          transformName n ##
            mapSplices (runChildrenWith . (\oinner -> mconcat . map (buildSplice oinner) $ fs))
                       (unArray . M.lookup n $ o)

        unObj (Just (Object o)) = o
        unObj _ = M.empty
        unArray (Just (Array v)) = map (unObj . Just) $ V.toList v
        unArray _ = []
        traverseObject pth o = foldl (\o x -> unObj . M.lookup x $ o) o pth
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
     tagDict <- liftIO $ lookupTaxDict (TaxDictKey "post_tag") wp
     catDict <- liftIO $ lookupTaxDict (TaxDictKey "category") wp
     wpKeys <- findPrefetchables tagDict catDict n
     void $ liftIO $ concurrently $ map (cachingGet wp) wpKeys
     runChildren

findPrefetchables :: (MonadState s m, MonadIO m) =>
       (TaxSpec TagType -> TaxSpecId TagType)
    -> (TaxSpec CatType -> TaxSpecId CatType)
    -> X.Node
    -> HeistT m m [WPKey]
findPrefetchables tdict cdict e@(X.Element "wpPosts" attrs' children) =
  do attrs <- runAttributes attrs'
     rest <- mapM (findPrefetchables tdict cdict) children
     return $ [mkWPKey tdict cdict . parseQueryNode $ attrs] <> concat rest
findPrefetchables tdict cdict e@(X.Element "wpPage" attrs' _) =
  case lookup "name" attrs' of
    Nothing -> return []
    Just _ -> do attrs <- runAttributes attrs'
                 return [PageKey . fromJust . lookup "name" $ attrs]
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
getPost Wordpress{..} wpKey = decodePost <$> cachingGetRetry wpKey
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
