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
 , transformName
 , TaxSpec(..)
 , TagType
 , CatType
 , TaxSpecList(..)
 , Field(..)
 , mergeFields
 ) where


import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.MVar
import           Control.Lens
import           Data.Aeson                      hiding (decode, encode)
import qualified Data.Attoparsec.Text            as A
import           Data.Char                       (toUpper)
import qualified Data.Configurator               as C
import           Data.Default
import qualified Data.HashMap.Strict             as M
import           Data.IntSet                     (IntSet)
import qualified Data.IntSet                     as IntSet
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Map.Syntax
import           Data.Maybe                      (fromJust, fromMaybe)
import           Data.Monoid
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Encoding         as TL
import           Data.Time.Clock
import qualified Data.Vector                     as V
import           Database.Redis                  (Redis)
import qualified Database.Redis                  as R
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import qualified Network.Wreq                    as W
import           Snap                            hiding (path)
import           Snap.Snaplet.Heist              (Heist, addConfig)
import           Snap.Snaplet.RedisDB            (RedisDB)
import qualified Snap.Snaplet.RedisDB            as R
import qualified Text.XmlHtml                    as X

import           Snap.Snaplet.Wordpress.Cache
import           Snap.Snaplet.Wordpress.Internal
import           Snap.Snaplet.Wordpress.Posts
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

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
     Wordpress { requestPostSet     :: Maybe IntSet
               , wpExpireAggregates :: IO Bool
               , wpExpirePost       :: WPKey -> IO Bool
               , cachingGet         :: WPKey -> IO (Maybe Text)
               , cachingGetRetry    :: WPKey -> IO Text
               , cachingGetError    :: WPKey -> IO Text
               , cacheInternals     :: WordpressInt b
               }

initWordpress :: Snaplet (Heist b)
              -> Snaplet RedisDB
              -> WPLens b
              -> SnapletInit b (Wordpress b)
initWordpress = initWordpress' def

type WPLens b = Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))

initWordpress' :: WordpressConfig (Handler b b)
               -> Snaplet (Heist b)
               -> Snaplet RedisDB
               -> WPLens b
               -> SnapletInit b (Wordpress b)
initWordpress' wpconf heist redis wpLens =
  makeSnaplet "wordpress" "" Nothing $
    do conf <- getSnapletUserConfig
       let logf = logger wpconf
       wpReq <- case requester wpconf of
                Nothing -> do u <- liftIO $ C.require conf "username"
                              p <- liftIO $ C.require conf "password"
                              return $ wreqRequester logf u p
                Just r -> return r
       active <- liftIO $ newMVar Map.empty
       let rrunRedis = R.runRedis $ view (snapletValue . R.redisConnection) redis
       let wpInt = WordpressInt{ wpRequest = wpRequestInt wpReq (endpoint wpconf)
                               , wpCacheSet = wpCacheSetInt rrunRedis (cacheBehavior wpconf)
                               , wpCacheGet = wpCacheGetInt rrunRedis (cacheBehavior wpconf)
                               , startReqMutex = startReqMutexInt active
                               , stopReqMutex = stopReqMutexInt active }
       let wp = Wordpress{ requestPostSet = Nothing
                         , wpExpireAggregates = wpExpireAggregatesInt rrunRedis
                         , wpExpirePost = wpExpirePostInt rrunRedis
                         , cachingGet = cachingGetInt wpInt
                         , cachingGetRetry = cachingGetRetryInt wpInt
                         , cachingGetError = cachingGetErrorInt wpInt
                         , cacheInternals = wpInt
                         }
       addConfig heist $ set scCompiledSplices (wordpressSplices wp wpconf wpLens) mempty
       return wp

wordpressSplices :: Wordpress b
                 -> WordpressConfig (Handler b b)
                 -> WPLens b
                 -> Splices (Splice (Handler b b))
wordpressSplices wp conf wpLens =
  do "wpPosts" ## wpPostsSplice wp conf wpLens
     "wpPostByPermalink" ## wpPostByPermalinkSplice conf wpLens
     "wpNoPostDuplicates" ## wpNoPostDuplicatesSplice wpLens
     "wp" ## wpPrefetch wp

wpPrefetch :: Wordpress b
           -> Splice (Handler b b)
wpPrefetch wp =
  do n <- getParamNode
     childrenRes <- runChildren
     tagDict <- lift $ lookupTaxDict (TaxDictKey "post_tag") wp
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

wpPostsSplice :: Wordpress b
              -> WordpressConfig (Handler b b)
              -> WPLens b
              -> Splice (Handler b b)
wpPostsSplice wp wpconf wpLens =
  do promise <- newEmptyPromise
     outputChildren <- manyWithSplices runChildren (postSplices (extraFields wpconf))
                                                   (getPromise promise)
     postsQuery <- parseQueryNode <$> getParamNode
     tagDict <- lift $ lookupTaxDict (TaxDictKey "post_tag") wp
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
decodeJsonErr res = case decode res of
                      Nothing -> error $ T.unpack $ "Unparsable JSON: " <> res
                      Just val -> val

decodeJson :: FromJSON a => Text -> Maybe a
decodeJson res = decode res

lookupTaxDict :: WPKey -> Wordpress b -> IO (TaxSpec a -> TaxSpecId a)
lookupTaxDict key@(TaxDictKey resName) wp@Wordpress{..} =
  do res <- decodeJsonErr <$> cachingGetError key
     return (getSpecId $ TaxDict res resName)

addPostIds :: WPLens b -> [Int] -> Handler b b ()
addPostIds wpLens ids =
  do w@Wordpress{..} <- use (wpLens . snapletValue)
     assign (wpLens . snapletValue)
            w{requestPostSet = ((`IntSet.union` (IntSet.fromList ids)) <$> requestPostSet) }

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

wpPostByPermalinkSplice :: WordpressConfig (Handler b b)
                        -> WPLens b
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

wreqRequester :: Maybe (Text -> IO ())
              -> Text
              -> Text
              -> Requester
wreqRequester logger user passw =
  Requester $ \u ps -> do let opts = (W.defaults & W.params .~ ps
                                      & W.auth .~ W.basicAuth user' pass')
                          wplog logger $ "wreq: " <> u <> " with params: " <>
                            (T.intercalate "&" . map (\(a,b) -> a <> "=" <> b) $ ps)
                          r <- W.getWith opts (T.unpack u)
                          return $ TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody
  where user' = T.encodeUtf8 user
        pass' = T.encodeUtf8 passw

wplog :: Maybe (Text -> IO ()) -> Text -> IO ()
wplog logger msg = case logger of
                    Nothing -> return ()
                    Just f -> f msg
