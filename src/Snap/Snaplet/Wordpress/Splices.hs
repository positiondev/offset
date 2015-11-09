{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Snaplet.Wordpress.Splices where

import           Control.Applicative            ((<|>))
import           Control.Lens                   hiding (children)
import           Control.Monad                  (void)
import           Control.Monad.Trans            (lift, liftIO)
import           Data.Aeson                     hiding (decode, encode)
import qualified Data.Attoparsec.Text           as A
import           Data.Char                      (toUpper)
import qualified Data.HashMap.Strict            as M
import           Data.IntSet                    (IntSet)
import qualified Data.IntSet                    as IntSet
import           Data.Map.Syntax
import           Data.Maybe                     (fromJust, fromMaybe)
import           Data.Monoid
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import           Snap                           hiding (path, rqURI)
import qualified Text.XmlHtml                   as X

import           Snap.Snaplet.Wordpress.Field
import           Snap.Snaplet.Wordpress.Posts
import           Snap.Snaplet.Wordpress.Queries
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

wordpressSplices :: Wordpress b
                 -> [Field (Handler b b)]
                 -> WPLens b
                 -> Splices (Splice (Handler b b))
wordpressSplices wp extraFields wpLens =
  do "wpPosts" ## wpPostsSplice wp extraFields wpLens
     "wpPostByPermalink" ## wpPostByPermalinkSplice extraFields wpLens
     "wpNoPostDuplicates" ## wpNoPostDuplicatesSplice wpLens
     "wp" ## wpPrefetch wp

wpPostsSplice :: Wordpress b
              -> [Field (Handler b b)]
              -> WPLens b
              -> Splice (Handler b b)
wpPostsSplice wp extraFields wpLens =
  do promise <- newEmptyPromise
     outputChildren <- manyWithSplices runChildren (postSplices extraFields)
                                                   (getPromise promise)
     postsQuery <- parseQueryNode <$> getParamNode
     tagDict <- lift $ lookupTaxDict (TaxDictKey "tag") wp
     catDict <- lift $ lookupTaxDict (TaxDictKey "category") wp
     let wpKey = mkWPKey tagDict catDict postsQuery
     return $ yieldRuntime $
       do res <- liftIO $ cachingGetRetry wp wpKey
          case (decode res) of
            Just posts -> do let postsW = extractPostIds posts
                             Wordpress{..} <- lift (use (wpLens . snapletValue))
                             let postsND = take (qlimit postsQuery) . noDuplicates requestPostSet $ postsW
                             lift $ addPostIds wpLens (map fst postsND)
                             putPromise promise (map snd postsND)
                             codeGen outputChildren
            Nothing -> codeGen (yieldPureText "")
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just postSet) = filter (\(i,_) -> IntSet.notMember i postSet)

wpPostByPermalinkSplice :: [Field (Handler b b)]
                        -> WPLens b
                        -> Splice (Handler b b)
wpPostByPermalinkSplice extraFields wpLens =
  do promise <- newEmptyPromise
     outputChildren <- withSplices runChildren (postSplices extraFields) (getPromise promise)
     return $ yieldRuntime $
       do mperma <- (parsePermalink . rqURI) <$> lift getRequest
          case mperma of
            Nothing -> codeGen (yieldPureText "")
            Just (year, month, slug) ->
              do res <- lift $ with wpLens $ wpGetPost (PostByPermalinkKey year month slug)
                 case res of
                   Just post -> do putPromise promise post
                                   codeGen outputChildren
                   _ -> codeGen (yieldPureText "")

wpNoPostDuplicatesSplice :: WPLens b
                         -> Splice (Handler b b)
wpNoPostDuplicatesSplice wpLens =
  return $ yieldRuntime $
    do w@Wordpress{..} <- lift $ use (wpLens . snapletValue)
       case requestPostSet of
         Nothing -> lift $ assign (wpLens . snapletValue)
                                  w{requestPostSet = (Just IntSet.empty)}
         Just _ -> return ()
       codeGen $ yieldPureText ""

postSplices :: (Functor m, Monad m) => [Field m] -> Splices (RuntimeSplice m Object -> Splice m)
postSplices extra = mconcat (map buildSplice (mergeFields postFields extra))
  where buildSplice (F n) =
          transformName n ## pureSplice . textSplice $ getText n
        buildSplice (P n splice) =
          transformName n ## \o -> splice (getText n <$> o)
        buildSplice (N n fs) = transformName n ## \o ->
                                 withSplices runChildren
                                                (mconcat $ map buildSplice fs)
                                                (unObj . fromJust . M.lookup n <$> o)
        buildSplice (C n path) =
          transformName n ## bindLater $ \o -> return (textSplice (getText (last path) . traverseObject (init path)) o)
        buildSplice (CN n path fs) =
          transformName n ## \o -> withSplices runChildren
                                               (mconcat $ map buildSplice fs)
                                               (traverseObject path <$> o)
        buildSplice (M n fs) = transformName n ## \o ->
                                 manyWithSplices runChildren
                                                    (mconcat $ map buildSplice fs)
                                                    (unArray . fromJust . M.lookup n <$> o)
        unObj (Object o) = o
        unArray (Array v) = map unObj $ V.toList v
        traverseObject [] o = o
        traverseObject (x:xs) o = traverseObject xs (unObj . fromJust . M.lookup x $ o)
        getText n o = case M.lookup n o of
                        Just (String t) -> t
                        Just (Number i) -> T.pack $ show i
                        _ -> ""

-- * -- Internal -- * --

parseQueryNode :: X.Node -> WPQuery
parseQueryNode n =
  mkPostsQuery (readSafe =<< X.getAttribute "limit" n)
               (readSafe =<< X.getAttribute "num" n)
               (readSafe =<< X.getAttribute "offset" n)
               (readSafe =<< X.getAttribute "page" n)
               (readSafe =<< X.getAttribute "tags" n)
               (readSafe =<< X.getAttribute "categories" n)

mkPostsQuery :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
       -> Maybe (TaxSpecList TagType) -> Maybe (TaxSpecList CatType)
       -> WPQuery
mkPostsQuery l n o p ts cs =
  WPPostsQuery{ qlimit = fromMaybe 20 l
              , qnum = fromMaybe 20 n
              , qoffset = fromMaybe 0 o
              , qpage = fromMaybe 1 p
              , qtags = fromMaybe (TaxSpecList []) ts
              , qcats = fromMaybe (TaxSpecList []) cs
              }


wpPrefetch :: Wordpress b
           -> Splice (Handler b b)
wpPrefetch wp =
  do n <- getParamNode
     childrenRes <- runChildren
     tagDict <- lift $ lookupTaxDict (TaxDictKey "tag") wp
     catDict <- lift $ lookupTaxDict (TaxDictKey "category") wp
     let wpKeys = findPrefetchables tagDict catDict n
     return $ yieldRuntime $
       do void $ liftIO $ concurrently $ map (cachingGet wp) wpKeys
          codeGen childrenRes

findPrefetchables :: (TaxSpec TagType -> TaxSpecId TagType)
    -> (TaxSpec CatType -> TaxSpecId CatType)
    -> X.Node
    -> [WPKey]
findPrefetchables tdict cdict e@(X.Element "wpPosts" _ children) =
  concat (map (findPrefetchables tdict cdict) children) <>
    [mkWPKey tdict cdict $ parseQueryNode e]
findPrefetchables tdict cdict (X.Element _ _ children) =
  concat (map (findPrefetchables tdict cdict) children)
findPrefetchables _ _ _ = []

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
               ++ map TagFilter tags ++ map CatFilter cats)

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

wpGetPost :: WPKey -> Handler b (Wordpress b) (Maybe Object)
wpGetPost wpKey =
  do wp <- view snapletValue <$> getSnapletState
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
addPostIds :: WPLens b -> [Int] -> Handler b b ()
addPostIds wpLens ids =
  do w@Wordpress{..} <- use (wpLens . snapletValue)
     assign (wpLens . snapletValue)
            w{requestPostSet = ((`IntSet.union` (IntSet.fromList ids)) <$> requestPostSet) }
