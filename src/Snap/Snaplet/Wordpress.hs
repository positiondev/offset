{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Snap.Snaplet.Wordpress (
   Wordpress(..)
 , WordpressConfig(..)
 , Requester(..)
 , CacheBehavior(..)
 , initWordpress
 , initWordpress'
 , getPost
 , WPKey(..)
 , Filter(..)
 , wpCacheGet
 , wpCacheSet
 , wpCacheGet'
 , wpCacheSet'
 , wpExpirePost
 , wpExpireAggregates
 , wpExpirePost'
 , wpExpireAggregates'

 , transformName
 , TaxSpec(..)
 , TagType
 , CatType
 , TaxSpecList(..)
 , Field(..)
 , mergeFields
 ) where


import           Blaze.ByteString.Builder     (Builder)
import           Control.Applicative
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.MVar
import           Control.Lens
import           Data.Aeson
import qualified Data.Attoparsec.Text         as A
import           Data.ByteString              (ByteString)
import           Data.Char                    (toUpper)
import qualified Data.Configurator            as C
import           Data.Default
import qualified Data.HashMap.Strict          as M
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet
import           Data.List                    (intercalate)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Map.Syntax
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe,
                                               isJust, listToMaybe, mapMaybe)
import           Data.Monoid
import           Data.Ratio
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TL
import           Data.Time.Clock
import qualified Data.Vector                  as V
import           Database.Redis               (Redis)
import qualified Database.Redis               as R
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import qualified Network.Wreq                 as W
import           Snap
import           Snap.Snaplet.Heist           (Heist, addConfig)
import           Snap.Snaplet.RedisDB         (RedisDB)
import qualified Snap.Snaplet.RedisDB         as R
import qualified Text.XmlHtml                 as X

import           Snap.Snaplet.Wordpress.Cache
import           Snap.Snaplet.Wordpress.Posts
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

newtype Requester = Requester { unRequester :: forall a. Text -> [(Text, Text)] -> (Text -> a) -> IO a}

data WordpressConfig m =
     WordpressConfig { endpoint      :: Text
                     , requester     :: Maybe Requester
                     , cacheBehavior :: CacheBehavior
                     , extraFields   :: [Field m]
                     , logger        :: Maybe (Text -> IO ())
                     }

instance Default (WordpressConfig m) where
  def = WordpressConfig "http://127.0.0.1/wp-json" Nothing (CacheSeconds 600) [] Nothing


data Wordpress b =
     Wordpress { runRedis       :: forall a. Redis a -> IO a
               , runHTTP        :: Requester
               , activeMV       :: MVar (Map WPKey UTCTime)
               , requestPostSet :: Maybe IntSet
               , conf           :: WordpressConfig (Handler b b)
               }

initWordpress :: Snaplet (Heist b)
              -> Snaplet RedisDB
              -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
              -> SnapletInit b (Wordpress b)
initWordpress = initWordpress' def

initWordpress' :: WordpressConfig (Handler b b)
               -> Snaplet (Heist b)
               -> Snaplet RedisDB
               -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
               -> SnapletInit b (Wordpress b)
initWordpress' wpconf heist redis wpLens =
  makeSnaplet "wordpress" "" Nothing $
    do conf <- getSnapletUserConfig
       req <- case requester wpconf of
                Nothing -> do u <- liftIO $ C.require conf "username"
                              p <- liftIO $ C.require conf "password"
                              return $ wreqRequester wpconf u p
                Just r -> return r
       active <- liftIO $ newMVar Map.empty
       let wp = Wordpress (x redis) req active Nothing wpconf
       addConfig heist $ set scCompiledSplices (wordpressSplices wp wpconf wpLens) mempty
       return wp

x snaplet action = do
  let connection = view (snapletValue . R.redisConnection) snaplet
  R.runRedis connection action


wordpressSplices :: Wordpress b
                 -> WordpressConfig (Handler b b)
                 -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                 -> Splices (Splice (Handler b b))
wordpressSplices wp conf wpLens =
  do "wpPosts" ## wpPostsSplice wp conf wpLens
     "wpPostByPermalink" ## wpPostByPermalinkSplice conf wpLens
     "wpNoPostDuplicates" ## wpNoPostDuplicatesSplice wpLens
     "wp" ## wpPrefetch wp wpLens conf


wpPrefetch :: Wordpress b
           -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
           -> WordpressConfig (Handler b b)
           -> Splice (Handler b b)
wpPrefetch wp wpLens conf =
  do n <- getParamNode
     childrenRes <- runChildren
     tagDict <- lift $ lookupTaxDict "post_tag" wp
     catDict <- lift $ lookupTaxDict "category" wp
     let wpKeys = findPrefetchables tagDict catDict n
     return $ yieldRuntime $
       do lift $ mapM_ (getPosts wpLens conf False) wpKeys
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


wpNoPostDuplicatesSplice :: Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                         -> Splice (Handler b b)
wpNoPostDuplicatesSplice wpLens =
  return $ yieldRuntime $
    do w@Wordpress{..} <- lift $ use (wpLens . snapletValue)
       case requestPostSet of
         Nothing -> lift $ assign (wpLens . snapletValue)
                                  w{requestPostSet = (Just IntSet.empty)}
         Just _ -> return ()
       codeGen $ yieldPureText ""

getWordpress :: Handler b v v
getWordpress = view snapletValue <$> getSnapletState

startWpQueryMutex :: Wordpress b -> WPKey -> IO Bool
startWpQueryMutex Wordpress{..} wpKey =
  do now <- liftIO $ getCurrentTime
     liftIO $ modifyMVar activeMV $ \a ->
      let active = filterCurrent now a
      in if Map.member wpKey active
          then return (active, True)
          else return (Map.insert wpKey now active, False)
  where filterCurrent now = Map.filter (\v -> diffUTCTime now v < 1)


markDoneRunning :: Wordpress b -> WPKey -> IO ()
markDoneRunning Wordpress{..} wpKey =
  modifyMVar_ activeMV $ return . Map.delete wpKey

data WPQuery = WPPostsQuery{ qlimit  :: Int
                           , qnum    :: Int
                           , qoffset :: Int
                           , qpage   :: Int
                           , qtags   :: TaxSpecList TagType
                           , qcats   :: TaxSpecList CatType}

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

parseQueryNode :: X.Node -> WPQuery
parseQueryNode n =
  mkPostsQuery (readSafe =<< X.getAttribute "limit" n)
               (readSafe =<< X.getAttribute "num" n)
               (readSafe =<< X.getAttribute "offset" n)
               (readSafe =<< X.getAttribute "page" n)
               (readSafe =<< X.getAttribute "tags" n)
               (readSafe =<< X.getAttribute "categories" n)

getPosts :: Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
         -> WordpressConfig (Handler b b)
         -> Bool
         -> WPKey
         -> Handler b b Text
getPosts wpLens wpconf tryAgain wpKey =
  do wp@Wordpress{..} <- use (wpLens . snapletValue)
     cached <- liftIO $ wpCacheGet wp wpKey
     case cached of
       Just r -> return r
       Nothing ->
         do running <- liftIO $ startWpQueryMutex wp wpKey
            if running
               then if tryAgain then (do liftIO $ threadDelay 100000
                                         getPosts wpLens wpconf tryAgain wpKey)
                                else return ""
               else
                 do let endpt = endpoint wpconf
                    h <- liftIO $ (unRequester runHTTP) (endpt <> "/posts") (buildParams wpKey) id
                    liftIO $ do
                      wpCacheSet wp wpKey h
                      markDoneRunning wp wpKey
                    return h

{-
getPosts' :: WordpressConfig (Handler b b)
          -> WPKey
          -> IO (Maybe Text)
getPosts' wpconf wpKey = -- FINDME
  do wordpress@Wordpress{..} <- undefined
     cached <- runRedis $ cacheGet (cacheBehavior wpconf) wpKey
     case cached of
       Just r -> return r
       Nothing ->
         do running <- startWpQueryMutex wordpress wpKey
            if running
               then return Nothing
               else
                 do let endpt = endpoint wpconf
                    h <- liftIO $ (unRequester runHTTP) (endpt <> "/posts") (buildParams wpKey) id
                    runRedis $ wpCacheSet (cacheBehavior wpconf) wpKey h

                    with wpLens $ do
                      markDoneRunning wpKey
                    return $ Just h
-}

wpPostsSplice :: forall b. Wordpress b
              -> WordpressConfig (Handler b b)
              -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
              -> Splice (Handler b b)
wpPostsSplice wp wpconf wpLens =
  do promise <- newEmptyPromise
     outputChildren <- manyWithSplices runChildren (postSplices (extraFields wpconf))
                                                   (getPromise promise)
     postsQuery <- parseQueryNode <$> getParamNode
     tagDict <- lift $ lookupTaxDict "post_tag" wp
     catDict <- lift $ lookupTaxDict "category" wp
     let wpKey = mkWPKey tagDict catDict postsQuery
     return $ yieldRuntime $
       do res <- lift $ getPosts wpLens wpconf True wpKey
          case (decodeStrict . T.encodeUtf8 $ res) of
            Just posts -> do let postsW = extractPostIds posts
                             Wordpress{..} <- lift (use (wpLens . snapletValue))
                             let postsND = take (qlimit postsQuery) . noDuplicates requestPostSet  $ postsW
                             lift $ addPostIds wpLens (map fst postsND)
                             putPromise promise (map snd postsND)
                             codeGen outputChildren
            Nothing -> codeGen (yieldPureText "")
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just postSet) = filter (\(i,_) -> IntSet.notMember i postSet)


buildParams :: WPKey -> [(Text, Text)]
buildParams (PostsKey filters) = params
  where params = Set.toList $ Set.map mkFilter filters
        mkFilter (TagFilter (TaxPlusId i)) = ("filter[tag__in]", tshow i)
        mkFilter (TagFilter (TaxMinusId i)) = ("filter[tag__not_in]", tshow i)
        mkFilter (CatFilter (TaxPlusId i)) = ("filter[category__in]", tshow i)
        mkFilter (CatFilter (TaxMinusId i)) = ("filter[category__not_in]", tshow i)
        mkFilter (NumFilter num) = ("filter[posts_per_page]", tshow num)
        mkFilter (OffsetFilter offset) = ("filter[offset]", tshow offset)

getSpecId :: TaxDict a -> TaxSpec a -> TaxSpecId a
getSpecId taxDict spec =
  case spec of
   TaxPlus slug -> TaxPlusId $ idFor taxDict slug
   TaxMinus slug -> TaxMinusId $ idFor taxDict slug
  where
    idFor :: TaxDict a -> Text -> Int
    idFor (TaxDict{..}) slug =
      case filter (\(TaxRes (_,s)) -> s == slug) dict of
       [] -> error $ T.unpack $ "Couldn't find " <> desc <> ": " <> slug
       (TaxRes (i,_):_) -> i

decodeJsonErr :: FromJSON a => Text -> a
decodeJsonErr res = case decodeStrict $ T.encodeUtf8 res of
                      Nothing -> error $ T.unpack $ "Unparsable JSON: " <> res
                      Just val -> val

decodeJson :: FromJSON a => Text -> Maybe a
decodeJson res = decodeStrict $ T.encodeUtf8 res

lookupTaxDict :: Text -> Wordpress b -> IO (TaxSpec a -> TaxSpecId a)
lookupTaxDict resName Wordpress{..} =
  do res <- liftIO $ (unRequester runHTTP) (endpoint conf <> "/taxonomies/" <> resName <> "/terms") [] decodeJsonErr
     return (getSpecId $ TaxDict res resName)

extractPostIds :: [Object] -> [(Int, Object)]
extractPostIds = map extractPostId


addPostIds :: Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b)) -> [Int] -> Handler b b ()
addPostIds wpLens ids =
  do w@Wordpress{..} <- use (wpLens . snapletValue)
     assign (wpLens . snapletValue)
            w{requestPostSet = ((`IntSet.union` (IntSet.fromList ids)) <$> requestPostSet) }

wpGetPost :: WPKey -> Handler b (Wordpress b) (Maybe Object)
wpGetPost wpKey =
  do wp <- view snapletValue <$> getSnapletState
     liftIO $ getPost wp wpKey

getPost wp@Wordpress{..} wpKey@(PostByPermalinkKey year month slug) = do
         mres <- wpCacheGet wp wpKey
         case mres of
           Just r' -> return (decodeStrict . T.encodeUtf8 $ r')
           Nothing ->
             do running <- startWpQueryMutex wp wpKey
                if running
                   then do threadDelay 100000
                           getPost wp wpKey
                   else do post' <- (unRequester runHTTP) (endpoint conf <> "/posts")
                                      [("filter[year]",year)
                                      ,("filter[monthnum]", month)
                                      ,("filter[name]", slug)] decodeJson
                           case post' of
                             Just (post:_) ->
                               do wpCacheSet wp wpKey (TL.toStrict . TL.decodeUtf8 . encode $ post)
                                  markDoneRunning wp wpKey
                                  return $ Just post
                             _ -> do markDoneRunning wp wpKey
                                     return Nothing
getPost wp key = error $ "getPost: Don't know how to get a post from key: " ++ show key

wpPostByPermalinkSplice :: WordpressConfig (Handler b b)
                        -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                        -> Splice (Handler b b)
wpPostByPermalinkSplice conf wpLens =
  do promise <- newEmptyPromise
     outputChildren <- withSplices runChildren (postSplices (extraFields conf)) (getPromise promise)
     return $ yieldRuntime $
       do mperma <- (parsePermalink . T.decodeUtf8 . rqURI) <$> lift getRequest
          case mperma of
            Nothing -> codeGen (yieldPureText "")
            Just (year, month, slug) ->
              do res <- lift $ with wpLens $ wpGetPost (PostByPermalinkKey year month slug)
                 case res of
                   Just post -> do putPromise promise post
                                   codeGen outputChildren
                   _ -> codeGen (yieldPureText "")

parsePermalink = either (const Nothing) Just . A.parseOnly parser . T.reverse
  where parser = do A.option ' ' (A.char '/')
                    guls <- A.many1 (A.letter <|> A.char '-')
                    A.char '/'
                    htnom <- A.count 2 A.digit
                    A.char '/'
                    raey <- A.count 4 A.digit
                    A.char '/'
                    return (T.reverse $ T.pack raey
                           ,T.reverse $ T.pack htnom
                           ,T.reverse $ T.pack guls)

-- TODO(dbp 2014-10-14): date should be parsed and nested.
data Field m = F Text -- A single flat field
             | P Text (RuntimeSplice m Text -> Splice m) -- A customly parsed flat field
             | N Text [Field m] -- A nested object field
             | M Text [Field m] -- A list field, where each element is an object

instance (Functor m, Monad m) =>  Show (Field m) where
  show (F t) = "F(" ++ T.unpack t ++ ")"
  show (P t _) = "P(" ++ T.unpack t ++ ",{code})"
  show (N t n) = "N(" ++ T.unpack t ++ "," ++ show n ++ ")"
  show (M t m) = "M(" ++ T.unpack t ++ "," ++ show m ++ ")"

postFields :: (Functor m, Monad m) => [Field m]
postFields = [F "ID"
             ,F "title"
             ,F "status"
             ,F "type"
             ,N "author" [F "ID",F "name",F "first_name",F "last_name",F "description"]
             ,F "content"
             ,P "date" dateSplice
             ,F "slug"
             ,F "excerpt"
             ,N "custom_fields" [F "test"]
             ,N "featured_image" [F "content"
                                 ,F "source"
                                 ,N "attachment_meta" [F "width"
                                                      ,F "height"
                                                      ,N "sizes" [N "thumbnail" [F "width"
                                                                                ,F "height"
                                                                                ,F "url"]
                                                                 ]]]
             ,N "terms" [M "category" [F "ID", F "name", F "slug", F "count"]
                        ,M "post_tag" [F "ID", F "name", F "slug", F "count"]]
             ]

mergeFields :: (Functor m, Monad m) => [Field m] -> [Field m] -> [Field m]
mergeFields fo [] = fo
mergeFields fo (f:fs) = mergeFields (overrideInList False f fo) fs
  where overrideInList :: (Functor m, Monad m) => Bool -> Field m -> [Field m] -> [Field m]
        overrideInList False fl [] = [fl]
        overrideInList True _ [] = []
        overrideInList v fl (m:ms) = (if matchesName m fl
                                        then mergeField m fl : (overrideInList True fl ms)
                                        else m : (overrideInList v fl ms))
        matchesName a b = getName a == getName b
        getName (F t) = t
        getName (P t _) = t
        getName (N t _) = t
        getName (M t _) = t
        mergeField (N _ left) (N nm right) = N nm (mergeFields left right)
        mergeField (M _ left) (N nm right) = N nm (mergeFields left right)
        mergeField (N _ left) (M nm right) = M nm (mergeFields left right)
        mergeField (M _ left) (M nm right) = M nm (mergeFields left right)
        mergeField _ right = right

dateSplice :: (Functor m, Monad m) => RuntimeSplice m Text -> Splice m
dateSplice d = withSplices runChildren splices (parseDate <$> d)
  where splices = do "wpYear" ## pureSplice $ textSplice fst3
                     "wpMonth" ## pureSplice $ textSplice snd3
                     "wpDay" ## pureSplice $ textSplice trd3
        parseDate :: Text -> (Text,Text,Text)
        parseDate = tuplify . T.splitOn "-" . T.takeWhile (/= 'T')
        tuplify (y:m:d:_) = (y,m,d)
        fst3 (a,_,_) = a
        snd3 (_,a,_) = a
        trd3 (_,_,a) = a

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
        buildSplice (M n fs) = transformName n ## \o ->
                                 manyWithSplices runChildren
                                                    (mconcat $ map buildSplice fs)
                                                    (unArray . fromJust . M.lookup n <$> o)
        unObj (Object o) = o
        unArray (Array v) = map unObj $ V.toList v
        getText n o = case M.lookup n o of
                        Just (String t) -> t
                        Just (Number i) -> T.pack $ show i
                        _ -> ""


transformName :: Text -> Text
transformName = T.append "wp" . snd . T.foldl f (True, "")
  where f (True, rest) next = (False, T.snoc rest (toUpper next))
        f (False, rest) '_' = (True, rest)
        f (False, rest) '-' = (True, rest)
        f (False, rest) next = (False, T.snoc rest next)


wpCacheGet :: Wordpress b -> WPKey -> IO (Maybe Text)
wpCacheGet Wordpress{..} wpKey = runRedis $ cacheGet (cacheBehavior conf) wpKey

wpCacheGet' key = do
  wp <- getWordpress
  liftIO $ wpCacheGet wp key

wpCacheSet' key o= do
  wp <- getWordpress
  liftIO $ wpCacheSet wp key o

wpCacheSet :: Wordpress b -> WPKey -> Text -> IO ()
wpCacheSet Wordpress{..} key o = void $ runRedis $ cacheSet (cacheBehavior conf) key o

wpExpireAggregates :: Wordpress b -> IO Bool
wpExpireAggregates Wordpress{..} = runRedis expireAggregates

wpExpireAggregates' = do
  wp <- getWordpress
  liftIO $ wpExpireAggregates wp

wpExpirePost' i = do
  wp <- getWordpress
  liftIO $ wpExpirePost wp i

wpExpirePost :: Wordpress b -> Int -> IO Bool
wpExpirePost Wordpress{..} i = runRedis $ expirePost i

wreqRequester :: WordpressConfig (Handler b b)
              -> Text
              -> Text
              -> Requester
wreqRequester conf user passw =
  Requester $ \u ps dcode -> do let opts = (W.defaults & W.params .~ ps
                                                       & W.auth .~ W.basicAuth user' pass')
                                wplog conf $ "wreq: " <> u <> " with params: " <>
                                           (T.intercalate "&" . map (\(a,b) -> a <> "=" <> b) $ ps)
                                r <- W.getWith opts (T.unpack u)
                                return $ dcode $ TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody
  where user' = T.encodeUtf8 user
        pass' = T.encodeUtf8 passw

wplog :: WordpressConfig (Handler b b) -> Text -> IO ()
wplog conf msg = case logger conf of
                   Nothing -> return ()
                   Just f -> f msg
