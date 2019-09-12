{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Offset.Splices where

import           Control.Monad.State
import           Control.Applicative     ((<|>))
import           Control.Lens            hiding (children)
import           Control.Concurrent.MVar
import           Data.Aeson              hiding (decode, encode, json, object)
import qualified Data.Attoparsec.Text    as A
import           Data.Char               (toUpper)
import qualified Data.HashMap.Strict     as M
import           Data.List               (lookup)
import qualified Data.Map as Map
import           Data.IntSet             (IntSet)
import qualified Data.IntSet             as IntSet
import           Data.Maybe              (fromJust, fromMaybe, catMaybes)
import           Data.Monoid
import           Data.Scientific         (floatingOrInteger)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Vector             as V
import           Web.Larceny

import           Web.Offset.Field
import           Web.Offset.Posts
import           Web.Offset.Queries
import           Web.Offset.Date
import           Web.Offset.Types
import           Web.Offset.Utils

wordpressSubs ::   Wordpress b
              -> [Field s]
              -> StateT s IO Text
              -> WPLens b s
              -> Substitutions s
wordpressSubs wp extraFields getURI wpLens =
  subs [ ("wpPosts", wpPostsFill wp extraFields wpLens)
       , ("wpPostsAggregate", wpPostsAggregateFill wp extraFields wpLens)
       , ("wpPostByPermalink", wpPostByPermalinkFill extraFields getURI wpLens)
       , ("wpPage", wpPageFill wpLens)
       , ("wpNoPostDuplicates", wpNoPostDuplicatesFill wpLens)
       , ("wp", wpPrefetch wp extraFields getURI wpLens)
       , ("wpCustom", wpCustomFill wp)
       , ("wpCustomAggregate", wpCustomAggregateFill wp)
       , ("wpCustomDate", wpCustomDateFill)
       , ("stripHtml", stripHtmlFill)]

stripTags :: Text -> Text
stripTags ""         = ""
stripTags str = case (T.take 1 str) of
  "<" -> stripTags $ T.drop 1 $ T.dropWhile (/= '>') str
  _ -> (T.take 1 str) <> stripTags (T.drop 1 str)

stripHtmlFill :: Fill s
stripHtmlFill = Fill $ \attrs (path, tpl) lib -> do
  text <- runTemplate tpl path mempty lib
  return $ stripTags text

wpCustomDateFill :: Fill s
wpCustomDateFill =
  useAttrs (a "wp_format" % a "date") customDateFill
  where customDateFill mWPFormat date =
          let wpFormat = fromMaybe "%Y-%m-%d %H:%M:%S" mWPFormat in
          case parseWPDate wpFormat date of
              Just d -> fillChildrenWith $ datePartSubs d
              Nothing -> rawTextFill $ "<!-- Unable to parse date: " <> date <> " -->"

wpCustomFill :: Wordpress b -> Fill s
wpCustomFill wp =
  useAttrs (a "endpoint") (\e -> customFill wp (EndpointKey e []))

customFill :: Wordpress b -> WPKey -> Fill s
customFill Wordpress{..} key = Fill $ \attrs (path, tpl) lib ->
  do res <- liftIO $ (cachingGetRetry key :: IO (Either StatusCode WPResponse))
     case (fmap decodeWPResponseBody res :: Either StatusCode (Maybe Value)) of
       Left code -> do
         let notification = "Encountered status code " <> tshow code
                         <> " when querying \"" <> tshow key <> "\"."
         liftIO $ wpLogger notification
         return $ "<!-- " <> notification <> " -->"
       Right (Just json) ->
        unFill (jsonToFill json) attrs (path, tpl) lib
       Right Nothing -> do
         let notification = "Unable to decode JSON for endpoint \"" <> tshow key
         liftIO $ wpLogger $ notification <> ": " <> tshow res
         return $ "<!-- " <> notification <> "-->"

jsonToFill :: Value -> Fill s
jsonToFill (Object o) =
  Fill $ \_ (path, tpl) lib -> runTemplate tpl path objectSubstitutions lib
  where objectSubstitutions =
          subs $ map (\k -> (transformName k,
                             jsonToFill (fromJust (M.lookup k o))))
                     (M.keys o)
jsonToFill (Array v) =
  Fill $ \attrs (path, tpl) lib ->
           V.foldr mappend "" <$> V.mapM (\e -> unFill (jsonToFill e) attrs (path, tpl) lib) v
jsonToFill (String s) = rawTextFill s
jsonToFill (Number n) = case floatingOrInteger n of
                          Left r -> rawTextFill $ tshow (r :: Double)
                          Right i -> rawTextFill $ tshow (i :: Integer)
jsonToFill (Bool True) = rawTextFill $ tshow True
jsonToFill (Bool False) = rawTextFill "<!-- JSON field found, but value is false. -->"
jsonToFill (Null) = rawTextFill "<!-- JSON field found, but value is null. -->"

wpCustomAggregateFill :: Wordpress b -> Fill s
wpCustomAggregateFill wp =
  useAttrs (a "endpoint") (customAggregateFill wp)

customAggregateFill :: Wordpress b -> Text -> Fill s
customAggregateFill Wordpress{..} endpoint = Fill $ \attrs (path, tpl) lib ->
  do let key = EndpointKey endpoint []
     res <- liftIO $ (cachingGetRetry key :: IO (Either StatusCode WPResponse))
     case (fmap decodeWPResponseBody res :: Either StatusCode (Maybe Value)) of
       Left code -> do
         let notification = "Encountered status code " <> tshow code
                         <> " when querying \"" <> endpoint <> "\"."
         liftIO $ wpLogger notification
         return $ "<!-- " <> notification <> " -->"
       Right (Just json) ->
        unFill (fillChildrenWith $
                    subs [ ("wpCustomItem", jsonToFill json)
                         , ("wpCustomMeta", useAttrs (a "page") (wpAggregateMetaFill res)) ])
               attrs (path, tpl) lib
       Right Nothing -> do
         let notification = "Unable to decode JSON for endpoint \"" <> endpoint
         liftIO $ wpLogger $ notification <> ": " <> tshow res
         return $ "<!-- " <> notification <> "-->"

wpPostsFill :: Wordpress b
            -> [Field s]
            -> WPLens b s
            -> Fill s
wpPostsFill wp extraFields wpLens = Fill $ \attrs tpl lib ->
  do (postsQuery, wpKey) <- mkPostsQueryAndKey wp attrs
     res <- liftIO $ cachingGetRetry wp wpKey
     case fmap decodeWPResponseBody res of
       Right (Just posts) -> do
         postsND <- postsWithoutDuplicates wpLens postsQuery posts
         addPostIds wpLens (map fst postsND)
         unFill (wpPostsHelper wp extraFields (map snd postsND)) mempty tpl lib
       Right Nothing -> return ""
       Left code     -> liftIO $ logStatusCode wp code

postsWithoutDuplicates :: WPLens b s
                       -> WPQuery
                       -> [Object]
                       -> StateT s IO [(Int, Object)]
postsWithoutDuplicates wpLens postsQuery posts = do
  wp <- use wpLens
  let postsW = extractPostIds posts
  return $ take (qlimit postsQuery) . removeDupes (requestPostSet wp) $ postsW
  where removeDupes :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        removeDupes Nothing = id
        removeDupes (Just wpPostIdSet) =
          filter (\(wpId,_) -> IntSet.notMember wpId wpPostIdSet)

mkPostsQueryAndKey :: Wordpress b
                   -> Attributes
                   -> StateT s IO (WPQuery, WPKey)
mkPostsQueryAndKey wp attrs = do
  let postsQuery = parseQueryNode (Map.toList attrs)
  filters <- liftIO $ mkFilters wp (qtaxes postsQuery)
  let wpKey = mkWPKey filters postsQuery
  return (postsQuery, wpKey)

logStatusCode :: Wordpress b -> Int -> IO Text
logStatusCode wp code = do
  let notification = "Encountered status code " <> tshow code
                     <> " when querying wpPosts."
  wpLogger wp notification
  return $ "<!-- " <> notification <> " -->"

wpPostsAggregateFill :: Wordpress b
            -> [Field s]
            -> WPLens b s
            -> Fill s
wpPostsAggregateFill wp extraFields wpLens = Fill $ \attrs tpl lib ->
  do (postsQuery, wpKey) <- mkPostsQueryAndKey wp attrs
     res <- liftIO $ cachingGetRetry wp wpKey
     case fmap decodeWPResponseBody res of
       Right (Just posts) -> do
          postsND' <- postsWithoutDuplicates wpLens postsQuery posts
          addPostIds wpLens (map fst postsND')
          unFill (fillChildrenWith $
                    subs [ ("wpPostsItem", wpPostsHelper wp extraFields (map snd postsND'))
                         , ("wpPostsMeta", wpAggregateMetaFill res (Just $ qpage postsQuery)) ])
                 mempty tpl lib
       Right Nothing -> return ""
       Left code -> liftIO $ logStatusCode wp code

wpAggregateMetaFill :: Either StatusCode WPResponse -> Maybe Int -> Fill s
wpAggregateMetaFill (Right (WPResponse headers _)) mCurrentPage = do
  let totalPagesText = maybe "" T.decodeUtf8
                          (lookup "x-wp-totalpages" headers)
      totalPages = fromMaybe 1 (readSafe totalPagesText) :: Int
      currentPage = fromMaybe 1 mCurrentPage
  fillChildrenWith $
    subs [ ("wpTotalPages", textFill totalPagesText )
         , ("wpHasMorePages",
               if currentPage < totalPages
                then fillChildren
                else textFill "")
         , ("wpNoMorePages",
               if currentPage < totalPages
                then textFill ""
                else fillChildren)]
wpPostsMetaFill _ _ = textFill ""

mkFilters :: Wordpress b -> [TaxSpecList] -> IO [Filter]
mkFilters wp specLists =
  concat <$> mapM (\(TaxSpecList tName list) -> catMaybes <$> mapM (toFilter tName) list) specLists
  where toFilter :: TaxonomyName -> TaxSpec -> IO (Maybe Filter)
        toFilter tName tSpec = do
          mTSpecId <- lookupSpecId wp tName tSpec
          case mTSpecId of
            Just tSpecId -> return $ Just (TaxFilter tName tSpecId)
            Nothing -> return Nothing

wpPostsHelper :: Wordpress b
              -> [Field s]
              -> [Object]
              -> Fill s
wpPostsHelper wp extraFields postsND =
  mapSubs (postSubs wp extraFields) postsND

wpPostByPermalinkFill :: [Field s]
                      -> StateT s IO Text
                      -> WPLens b s
                      -> Fill s
wpPostByPermalinkFill extraFields getURI wpLens = maybeFillChildrenWith' $
  do uri <- getURI
     let mperma = parsePermalink uri
     case mperma of
       Nothing -> do
         w@Wordpress{..} <- use wpLens
         liftIO $ wpLogger $ "unable to parse URI: " <> uri
         return Nothing
       Just (year, month, slug) ->
         do res <- wpGetPost wpLens (PostByPermalinkKey year month slug)
            case res of
              Just post -> do addPostIds wpLens [fst (extractPostId post)]
                              wp <- use wpLens
                              return $ Just (postSubs wp extraFields post)
              _ -> return Nothing


feedSubs :: [Field s] -> WPLens b s -> Object -> Substitutions s
feedSubs fields lens obj=
  subs $ [("wpPost", wpPostFromObjectFill fields lens obj)]

wpPostFromObjectFill :: [Field s]
                      -> WPLens b s
                      -> Object
                      -> Fill s
wpPostFromObjectFill extraFields wpLens postObj = maybeFillChildrenWith' $
  do  addPostIds wpLens [fst (extractPostId postObj)]
      wp <- use wpLens
      return $ Just (postSubs wp extraFields postObj)

wpNoPostDuplicatesFill :: WPLens b s -> Fill s
wpNoPostDuplicatesFill wpLens = rawTextFill' $
  do w@Wordpress{..} <- use wpLens
     case requestPostSet of
       Nothing -> assign wpLens
                    w{requestPostSet = Just IntSet.empty}
       Just _ -> return ()
     return ""

wpPageFill :: WPLens b s -> Fill s
wpPageFill wpLens =
  useAttrs (a "name") pageFill
  where pageFill Nothing = rawTextFill ""
        pageFill (Just slug) = rawTextFill' $
         do res <- wpGetPost wpLens (PageKey slug)
            return $ case res of
                       Just page -> case M.lookup "content" page of
                                      Just (Object o) -> case M.lookup "rendered" o of
                                        Just (String r) -> r
                                        _ -> ""
                                      _ -> ""
                       _ -> ""

postSubs :: Wordpress b -> [Field s] -> Object -> Substitutions s
postSubs wp extra object = subs (map (buildSplice object) (mergeFields postFields extra))
  where buildSplice o (F n) =
          (transformName n, rawTextFill $ getText n o)
        buildSplice o (B n) =
          (transformName n, textFill $ getBool n o)
        buildSplice o (Q n endpoint) =
          (transformName n, customFill wp (idToEndpoint endpoint $ getText n o))
        buildSplice o (QM n endpoint) =
          (transformName n, customFill wp (idsToEndpoint endpoint (unArray' . M.lookup n $ o)))
        buildSplice o (P n fill') =
          (transformName n, fill' $ getText n o)
        buildSplice o (PV n fill') =
          (transformName n, fill' (M.lookup n $ o))
        buildSplice o (PN n fill') =
          (transformName n, fill' (unObj . M.lookup n $ o))
        buildSplice o (PM n fill') =
          (transformName n, fill' (unArray . M.lookup n $ o))
        buildSplice o (N n fs) =
          (transformName n, fillChildrenWith $ subs
                            (map (buildSplice (unObj . M.lookup n $ o)) fs))
        buildSplice o (C n path) =
          (transformName n, rawTextFill (getText (last path) . traverseObject (init path) $ o))
        buildSplice o (CB n path) =
          (transformName n, rawTextFill (getBool (last path) . traverseObject (init path) $ o))
        buildSplice o (CN n path fs) =
          (transformName n, fillChildrenWith $ subs
                            (map (buildSplice (traverseObject path o)) fs))
        buildSplice o (M n fs) =
          (transformName n,
            mapSubs (\(i, oinner) -> subs $ map (buildSplice oinner) fs
                                         <> [(transformName n <> "Index", textFill (tshow i))])
                    (zip [1..] (unArray . M.lookup n $ o)))
        unValue (String t) = t
        unValue (Number i) = either (tshow :: Double -> Text)
                                    (tshow :: Integer -> Text) (floatingOrInteger i)
        unValue v = ""
        unObj (Just (Object o)) = o
        unObj _ = M.empty
        unArray (Just (Array v)) = map (unObj . Just) $ V.toList v
        unArray _ = []
        unArray' (Just (Array v)) = map unValue $ V.toList v
        unArray' _ = []
        traverseObject pth o = foldl (\o' x -> unObj . M.lookup x $ o') o pth
        getText n o = maybe "" unValue (M.lookup n o)
        getBool n o = case M.lookup n o of
                        Just (Bool b) -> tshow b
                        _ -> ""

-- * -- Internal -- * --

parseQueryNode :: [(Text, Text)] -> WPQuery
parseQueryNode attrs =
  WPPostsQuery  { qlimit   = fromMaybe 20 $ readLookup "limit" attrs
                , qnum     = fromMaybe 20 $ readLookup "num" attrs
                , qoffset  = fromMaybe 0  $ readLookup "offset" attrs
                , qpage    = fromMaybe 1  $ readLookup "page" attrs
                , qperpage = fromMaybe 20 $ readLookup "perpage" attrs
                , qorder   = toWPOrdering $ readLookup "order" attrs
                , qorderby = lookup "order_by" attrs
                , qsearch  = lookup "search" attrs
                , qbefore  = readLookup "before" attrs
                , qafter   = readLookup "after" attrs
                , qstatus  = toWPStatus $ lookup "status" attrs
                , qsticky  = readLookup "sticky" attrs
                , quser    = lookup "user" attrs
                , qtaxes   = filterTaxonomies attrs }
  where readLookup n attrs = readSafe =<< lookup n attrs

toWPOrdering :: Maybe Text -> Maybe WPOrdering
toWPOrdering (Just "asc") = Just Asc
toWPOrdering (Just "desc") = Just Desc
toWPOrdering _ = Nothing

toWPStatus :: Maybe Text -> Maybe WPPostStatus
toWPStatus (Just status) = readSafe (T.toTitle status)
toWPStatus _ = Nothing

listOfFilters = ["limit"
                , "num"
                , "offset"
                , "page"
                , "per_page"
                , "user"
                , "orderby"
                , "context"
                , "search"
                , "after"
                , "before"
                , "slug"
                , "status"
                , "sticky"]

filterTaxonomies :: [(Text, Text)] -> [TaxSpecList]
filterTaxonomies attrs =
  let taxAttrs = filter (\(k, _) -> (k `notElem` listOfFilters)) attrs in
  map attrToTaxSpecList taxAttrs

taxDictKeys :: [TaxSpecList] -> [WPKey]
taxDictKeys = map (\(TaxSpecList tName _) -> TaxDictKey tName)

wpPrefetch :: Wordpress b
           -> [Field s]
           -> StateT s IO Text
           -> WPLens b s
           -> Fill s
wpPrefetch wp extra uri wpLens = Fill $ \ _m (p, tpl) l -> do
    Wordpress{..} <- use wpLens
    mKeys <- liftIO $ newMVar []
    void $ runTemplate tpl p (prefetchSubs wp mKeys) l
    wpKeys <- liftIO $ readMVar mKeys
    void $ liftIO $ concurrently $ map cachingGet wpKeys
    runTemplate tpl p (wordpressSubs wp extra uri wpLens) l

prefetchSubs :: Wordpress b -> MVar [WPKey] -> Substitutions s
prefetchSubs wp mkeys =
  subs [ ("wpPosts", wpPostsPrefetch wp mkeys)
       , ("wpPage", useAttrs (a"name") $ wpPagePrefetch mkeys) ]

wpPostsPrefetch :: Wordpress b
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
wpPagePrefetch mKeys name = rawTextFill' $
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
                    guls <- A.many1 (A.letter <|> A.char '-' <|> A.digit)
                    _ <- A.char '/'
                    segment2 <- A.many1 (A.letter <|> A.char '-' <|> A.digit)
                    _ <- A.char '/'
                    segment1 <- A.many1 (A.letter <|> A.char '-' <|> A.digit)
                    _ <- A.char '/'
                    return (T.reverse $ T.pack segment1
                           ,T.reverse $ T.pack segment2
                           ,T.reverse $ T.pack guls)

wpGetPost :: (MonadState s m, MonadIO m) => WPLens b s -> WPKey -> m (Maybe Object)
wpGetPost wpLens wpKey =
  do wp <- use wpLens
     liftIO $ getPost wp wpKey

getPost :: Wordpress b -> WPKey -> IO (Maybe Object)
getPost Wordpress{..} wpKey = decodePost <$> cachingGetRetry wpKey
  where decodePost :: Either StatusCode WPResponse -> Maybe Object
        decodePost (Right t) =
          do post' <- decodeWPResponseBody t
             case post' of
              Just (post:_) -> Just post
              _ -> Nothing
        decodePost (Left _) = Nothing


transformName :: Text -> Text
transformName = T.append "wp" . snd . T.foldl f (True, "")
  where f (True, rest) next = (False, T.snoc rest (toUpper next))
        f (False, rest) '_' = (True, rest)
        f (False, rest) '-' = (True, rest)
        f (False, rest) next = (False, T.snoc rest next)

-- Move this into Init.hs (should retrieve from Wordpress data structure)
addPostIds :: (MonadState s m, MonadIO m) => WPLens b s -> [Int] -> m ()
addPostIds wpLens ids =
  do w@Wordpress{..} <- use wpLens
     assign wpLens
            w{requestPostSet = (`IntSet.union` IntSet.fromList ids) <$> requestPostSet }

idToEndpoint :: IdToEndpoint -> Text -> WPKey
idToEndpoint (UseId endpoint) id = EndpointKey (endpoint <> id) []
idToEndpoint (UseSlug endpoint) slug = EndpointKey (endpoint) [("slug", slug)]

idsToEndpoint :: IdsToEndpoint -> [Text] -> WPKey
idsToEndpoint (UseInclude endpoint) ids = EndpointKey endpoint (map (\id -> ("include[]", id)) ids)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
