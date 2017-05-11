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

wordpressSubs ::   Wordpress b
              -> [Field s]
              -> StateT s IO Text
              -> WPLens b s
              -> Substitutions s
wordpressSubs wp extraFields getURI wpLens =
  subs [ ("wpPosts", wpPostsFill wp extraFields wpLens)
       , ("wpPostByPermalink", wpPostByPermalinkFill extraFields getURI wpLens)
       , ("wpPage", wpPageFill wpLens)
       , ("wpNoPostDuplicates", wpNoPostDuplicatesFill wpLens)
       , ("wp", wpPrefetch wp extraFields getURI wpLens)
       , ("wpCustom", wpCustomFill wp)
       , ("wpCustomDate", wpCustomDateFill) ]

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
  useAttrs (a "endpoint") (customFill wp)

customFill :: Wordpress b -> Text -> Fill s
customFill Wordpress{..} endpoint = Fill $ \attrs (path, tpl) lib ->
  do let key = EndpointKey endpoint
     res <- liftIO $ cachingGetRetry key
     case fmap decode res of
       Left code -> do
         let notification = "Encountered status code " <> tshow code
                         <> " when querying \"" <> endpoint <> "\"."
         liftIO $ wpLogger notification
         return $ "<!-- " <> notification <> " -->"
       Right (Just (json :: Value)) ->
         unFill (jsonToFill json) attrs (path, tpl) lib
       Right Nothing -> do
         let notification = "Unable to decode JSON for endpoint \"" <> endpoint
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
jsonToFill (Bool b) = rawTextFill $ tshow b
jsonToFill (Null) = rawTextFill "<!-- JSON field found, but value is null. -->"


wpPostsFill :: Wordpress b
            -> [Field s]
            -> WPLens b s
            -> Fill s
wpPostsFill wp extraFields wpLens = Fill $ \attrs tpl lib ->
  do let postsQuery = parseQueryNode (Map.toList attrs)
     filters <- liftIO $ mkFilters wp (qtaxes postsQuery)
     let wpKey = mkWPKey filters postsQuery
     res <- liftIO $ cachingGetRetry wp wpKey
     case fmap decode res of
       Right (Just posts) -> do
         let postsW = extractPostIds posts
         wp' <- use wpLens
         let postsND = take (qlimit postsQuery)
                       . noDuplicates (requestPostSet wp') $ postsW
         addPostIds wpLens (map fst postsND)
         unFill (wpPostsHelper wp extraFields (map snd postsND)) mempty tpl lib
       Right Nothing -> return ""
       Left code -> do
         let notification = "Encountered status code " <> tshow code
                            <> " when querying wpPosts."
         liftIO $ wpLogger wp notification
         return $ "<!-- " <> notification <> " -->"
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just wpPostIdSet) = filter (\(wpId,_) -> IntSet.notMember wpId wpPostIdSet)

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
        buildSplice o (Q n endpoint) =
          (transformName n, customFill wp (toEndpoint endpoint $ getText n o))
        buildSplice o (P n fill') =
          (transformName n, fill' $ getText n o)
        buildSplice o (PN n fill') =
          (transformName n, fill' (unObj . M.lookup n $ o))
        buildSplice o (PM n fill') =
          (transformName n, fill' (unArray . M.lookup n $ o))
        buildSplice o (N n fs) =
          (transformName n, fillChildrenWith $ subs
                            (map (buildSplice (unObj . M.lookup n $ o)) fs))
        buildSplice o (C n path) =
          (transformName n, rawTextFill (getText (last path) . traverseObject (init path) $ o))
        buildSplice o (CN n path fs) =
          (transformName n, fillChildrenWith $ subs
                            (map (buildSplice (traverseObject path o)) fs))
        buildSplice o (M n fs) =
          (transformName n,
            mapSubs (\oinner -> subs $ map (buildSplice oinner) fs)
                    (unArray . M.lookup n $ o))

        unObj (Just (Object o)) = o
        unObj _ = M.empty
        unArray (Just (Array v)) = map (unObj . Just) $ V.toList v
        unArray _ = []
        traverseObject pth o = foldl (\o' x -> unObj . M.lookup x $ o') o pth
        getText n o = case M.lookup n o of
                        Just (String t) -> t
                        Just (Number i) -> either (tshow :: Double -> Text)
                                                  (tshow :: Integer -> Text) (floatingOrInteger i)
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

taxDictKeys :: [TaxSpecList] -> [WPKey]
taxDictKeys = map (\(TaxSpecList tName _) -> TaxDictKey tName)

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
                    htnom <- A.count 2 A.digit
                    _ <- A.char '/'
                    raey <- A.count 4 A.digit
                    _ <- A.char '/'
                    return (T.reverse $ T.pack raey
                           ,T.reverse $ T.pack htnom
                           ,T.reverse $ T.pack guls)

wpGetPost :: (MonadState s m, MonadIO m) => WPLens b s -> WPKey -> m (Maybe Object)
wpGetPost wpLens wpKey =
  do wp <- use wpLens
     liftIO $ getPost wp wpKey

getPost :: Wordpress b -> WPKey -> IO (Maybe Object)
getPost Wordpress{..} wpKey = decodePost <$> cachingGetRetry wpKey
  where decodePost :: Either StatusCode Text -> Maybe Object
        decodePost (Right t) =
          do post' <- decodeJson t
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

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
