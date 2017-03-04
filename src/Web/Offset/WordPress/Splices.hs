{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Offset.WordPress.Splices where

import           Control.Monad.State
import           Control.Applicative     ((<|>))
import           Control.Lens            hiding (children)
import           Control.Concurrent.MVar
import           Data.Aeson              hiding (decode, encode, json, object)
import qualified Data.Attoparsec.Text    as A
import           Data.Char               (toUpper)
import qualified Data.HashMap.Strict     as M
import qualified Data.Map as Map
import           Data.IntSet             (IntSet)
import qualified Data.IntSet             as IntSet
import           Data.Maybe              (fromJust, fromMaybe, catMaybes)
import           Data.Monoid
import           Data.Scientific         (floatingOrInteger)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Web.Larceny

import           Web.Offset.Field
import           Web.Offset.Posts
import           Web.Offset.Queries
import           Web.Offset.Types
import           Web.Offset.Utils
import Web.Offset.Splices
import           Web.Offset.WordPress.Field

wordPressSubs ::   CMS b
                 -> [Field s]
                 -> StateT s IO Text
                 -> CMSLens b s
                 -> Substitutions s
wordPressSubs wp extraFields getURI cmsLens =
  subs [ ("wpPosts", wpPostsFill wp extraFields cmsLens)
       , ("wpPostByPermalink", wpPostByPermalinkFill extraFields getURI cmsLens)
       , ("wpPage", wpPageFill cmsLens)
       , ("wpNoPostDuplicates", wpNoPostDuplicatesFill cmsLens)
       , ("wp", wpPrefetch wp extraFields getURI cmsLens)]

wpPostsFill :: CMS b
            -> [Field s]
            -> CMSLens b s
            -> Fill s
wpPostsFill wp extraFields cmsLens = Fill $ \attrs tpl lib ->
  do let postsQuery = parseQueryNode (Map.toList attrs)
     filters <- liftIO $ mkFilters wp (qtaxes postsQuery)
     let wpKey = mkWPKey filters postsQuery
     res <- liftIO $ cachingGetRetry wp (toCMSKey wpKey)
     case fmap decode res of
       Right (Just posts) -> do
         let postsW = extractPostIds posts
         wp' <- use cmsLens
         let postsND = take (qlimit postsQuery)
                       . noDuplicates (requestPostSet wp') $ postsW
         addPostIds cmsLens (map fst postsND)
         unFill (wpPostsHelper extraFields (map snd postsND)) mempty tpl lib
       Right Nothing -> return ""
       Left code -> do
         let notification = "Encountered status code " <> tshow code
                            <> " when querying wpPosts."
         liftIO $ cmsLogger wp notification
         return $ "<!-- " <> notification <> " -->"
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just wpPostIdSet) = filter (\(wpId,_) -> IntSet.notMember wpId wpPostIdSet)

mkFilters :: CMS b -> [TaxSpecList] -> IO [Filter]
mkFilters wp specLists =
  concat <$> mapM (\(TaxSpecList tName list) -> catMaybes <$> mapM (toFilter tName) list) specLists
  where toFilter :: TaxonomyName -> TaxSpec -> IO (Maybe Filter)
        toFilter tName tSpec = do
          mTSpecId <- lookupSpecId wp tName tSpec
          case mTSpecId of
            Just tSpecId -> return $ Just (TaxFilter tName tSpecId)
            Nothing -> return Nothing

wpPostsHelper :: [Field s]
              -> [Object]
              -> Fill s
wpPostsHelper extraFields postsND = mapSubs (fieldSubs (mergeFields postFields extraFields)) postsND

wpPostByPermalinkFill :: [Field s]
                      -> StateT s IO Text
                      -> CMSLens b s
                      -> Fill s
wpPostByPermalinkFill extraFields getURI cmsLens = maybeFillChildrenWith' $
  do uri <- getURI
     let mperma = parsePermalink uri
     case mperma of
       Nothing -> return Nothing
       Just (year, month, slug) ->
         do res <- wpGetPost cmsLens (toCMSKey $ PostByPermalinkKey year month slug)
            case res of
              Just post -> do addPostIds cmsLens [fst (extractPostId post)]
                              return $ Just (fieldSubs (mergeFields postFields extraFields) post)
              _ -> return Nothing

wpNoPostDuplicatesFill :: CMSLens b s -> Fill s
wpNoPostDuplicatesFill cmsLens = textFill' $
  do w@CMS{..} <- use cmsLens
     case requestPostSet of
       Nothing -> assign cmsLens
                    w{requestPostSet = Just IntSet.empty}
       Just _ -> return ()
     return ""

wpPageFill :: CMSLens b s -> Fill s
wpPageFill cmsLens =
  useAttrs (a "name") pageFill
  where pageFill Nothing = textFill ""
        pageFill (Just slug) = textFill' $
         do res <- wpGetPost cmsLens (toCMSKey $ PageKey slug)
            return $ case res of
                       Just page -> case M.lookup "content" page of
                                      Just (Object o) -> case M.lookup "rendered" o of
                                        Just (String r) -> r
                                        _ -> ""
                                      _ -> ""
                       _ -> ""

-- * -- Internal -- * --

parseQueryNode :: [(Text, Text)] -> WPQuery
parseQueryNode attrs =
  mkPostsQuery (readSafe =<< lookup "limit" attrs)
               (readSafe =<< lookup "num" attrs)
               (readSafe =<< lookup "offset" attrs)
               (readSafe =<< lookup "page" attrs)
               (filterTaxonomies attrs)
               (lookup "user" attrs)

filterTaxonomies :: [(Text, Text)] -> [TaxSpecList]
filterTaxonomies attrs =
  let reservedTerms = ["limit", "num", "offset", "page", "user"]
      taxAttrs = filter (\(k, _) -> (k `notElem` reservedTerms)) attrs in
  map attrToTaxSpecList taxAttrs

taxDictKeys :: [TaxSpecList] -> [CMSKey]
taxDictKeys = map (\(TaxSpecList tName _) -> toCMSKey $ TaxDictKey tName)

mkPostsQuery :: Maybe Int
             -> Maybe Int
             -> Maybe Int
             -> Maybe Int
             -> [TaxSpecList]
             -> Maybe Text
             -> WPQuery
mkPostsQuery l n o p ts us =
  WPPostsQuery{ qlimit = fromMaybe 20 l
              , qnum = fromMaybe 20 n
              , qoffset = fromMaybe 0 o
              , qpage = fromMaybe 1 p
              , qtaxes = ts
              , quser = us
              }

wpPrefetch :: CMS b
           -> [Field s]
           -> StateT s IO Text
           -> CMSLens b s
           -> Fill s
wpPrefetch wp extra uri cmsLens = Fill $ \ _m (p, tpl) l -> do
    CMS{..} <- use cmsLens
    mKeys <- liftIO $ newMVar []
    void $ runTemplate tpl p (prefetchSubs wp mKeys) l
    wpKeys <- liftIO $ readMVar mKeys
    void $ liftIO $ concurrently $ map cachingGet (map toCMSKey wpKeys)
    runTemplate tpl p (wordPressSubs wp extra uri cmsLens) l

prefetchSubs :: CMS b -> MVar [WPKey] -> Substitutions s
prefetchSubs wp mkeys =
  subs [ ("wpPosts", wpPostsPrefetch wp mkeys)
       , ("wpPage", useAttrs (a"name") $ wpPagePrefetch mkeys) ]

wpPostsPrefetch :: CMS b
                -> MVar [WPKey]
                -> Fill s
wpPostsPrefetch wp mKeys = Fill $ \attrs _ _ ->
  do let postsQuery = parseQueryNode (Map.toList attrs)
     filters <- liftIO $ mkFilters wp (qtaxes postsQuery)
     let key = mkWPKey filters postsQuery
     liftIO $ modifyMVar_ mKeys (\keys -> return $ key : keys)
     return ""

wpPagePrefetch :: MVar [WPKey]
               -> Text
               -> Fill s
wpPagePrefetch mKeys name = textFill' $
  do let key = PageKey name
     liftIO $ modifyMVar_ mKeys (\keys -> return $ key : keys)
     return ""

mkWPKey :: [Filter]
        -> WPQuery
        -> WPKey
mkWPKey taxFilters WPPostsQuery{..} =
  let page = if qpage < 1 then 1 else qpage
      offset = qnum * (page - 1) + qoffset
  in PostsKey (Set.fromList $ [ NumFilter qnum , OffsetFilter offset]
               ++ taxFilters ++ userFilter quser)
  where userFilter Nothing = []
        userFilter (Just u) = [UserFilter u]

findDict :: [(TaxonomyName, TaxSpec -> TaxSpecId)] -> TaxSpecList -> [Filter]
findDict dicts (TaxSpecList tName tList) =
  case lookup tName dicts of
    Just dict -> map (TaxFilter tName . dict) tList
    Nothing -> []

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

addPostIds :: (MonadState s m, MonadIO m) => CMSLens b s -> [Int] -> m ()
addPostIds cmsLens ids =
  do cms@CMS{..} <- use cmsLens
     assign cmsLens
       cms{requestPostSet = (`IntSet.union` IntSet.fromList ids) <$> requestPostSet }

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
