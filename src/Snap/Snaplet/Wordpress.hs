{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Snap.Snaplet.Wordpress (
   Wordpress(..)
 , WordpressConfig(..)
 , CacheBehavior(..)
 , initWordpress
 , initWordpress'
 , getPost
 , CacheKey(..)
 , Filter(..)
 , cacheGet
 , cacheSet
 , expirePost
 , expireAggregates

 , transformName
 , TaxSpec(..)
 , TaxSpecList(..)
 , Field(..)
 , mergeFields
 ) where

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.MVar
import           Control.Lens
import           Data.Aeson
import qualified Data.Attoparsec.Text     as A
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Char                (toUpper)
import qualified Data.Configurator        as C
import           Data.Default
import qualified Data.HashMap.Strict      as M
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import           Data.List                (intercalate)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Map.Syntax
import           Data.Maybe               (catMaybes, fromJust, fromMaybe,
                                           isJust, listToMaybe)
import           Data.Monoid
import           Data.Ratio
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Data.Time.Clock
import qualified Data.Vector              as V
import           Database.Redis           (Redis)
import qualified Database.Redis           as R
import           GHC.Generics             (Generic)
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import qualified Network.Wreq             as W
import           Snap
import           Snap.Snaplet.Heist       (Heist, addConfig)
import           Snap.Snaplet.RedisDB     (RedisDB)
import qualified Snap.Snaplet.RedisDB     as R
import qualified Text.XmlHtml             as X

import           Debug.Trace


readSafe :: Read a => Text -> Maybe a
readSafe = fmap fst . listToMaybe . reads . T.unpack

tshow :: Show a => a -> Text
tshow = T.pack . show

data CacheBehavior = NoCache | CacheSeconds Int | CacheForever deriving (Show, Eq)

data WordpressConfig m = WordpressConfig { endpoint    :: Text
                                         , requester   :: Maybe (Text -> [(Text, Text)] -> IO Text)
                                         , cacheBehavior :: CacheBehavior
                                         , extraFields :: [Field m]
                                         , logger      :: Maybe (Text -> IO ())
                                         }

instance Default (WordpressConfig m) where
  def = WordpressConfig "http://127.0.0.1/wp-json" Nothing (CacheSeconds 600) [] Nothing

data Wordpress b = Wordpress { runRedis       :: forall a. Redis a -> Handler b (Wordpress b) a
                             , runHTTP        :: Text -> [(Text, Text)] -> IO Text
                             , activeMV       :: MVar (Map CacheKey UTCTime)
                             , requestPostSet :: Maybe IntSet
                             , conf           :: WordpressConfig (Handler b b)
                             }


initWordpress :: Snaplet (Heist b)
              -> Simple Lens b (Snaplet RedisDB)
              -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
              -> SnapletInit b (Wordpress b)
initWordpress = initWordpress' def

initWordpress' :: WordpressConfig (Handler b b)
               -> Snaplet (Heist b)
               -> Simple Lens b (Snaplet RedisDB)
               -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
               -> SnapletInit b (Wordpress b)
initWordpress' wpconf heist redis wordpress =
  makeSnaplet "wordpress" "" Nothing $
    do conf <- getSnapletUserConfig
       addConfig heist $ set scCompiledSplices (wordpressSplices wpconf wordpress) mempty
       req <- case requester wpconf of
                Nothing -> do u <- liftIO $ C.require conf "username"
                              p <- liftIO $ C.require conf "password"
                              return $ wreqRequester wpconf u p
                Just r -> return r
       active <- liftIO $ newMVar Map.empty
       return $ Wordpress (withTop' id . R.runRedisDB redis) req active Nothing wpconf

wordpressSplices :: WordpressConfig (Handler b b)
                 -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                 -> Splices (Splice (Handler b b))
wordpressSplices conf wordpress =
  do "wpPosts" ## wpPostsSplice conf wordpress
     "wpPostByPermalink" ## wpPostByPermalinkSplice conf wordpress
     "wpNoPostDuplicates" ## wpNoPostDuplicatesSplice wordpress


wpNoPostDuplicatesSplice :: Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                         -> Splice (Handler b b)
wpNoPostDuplicatesSplice wordpress =
  return $ yieldRuntime $
    do (Wordpress red ht act ps wpconf) <- lift $ use (wordpress . snapletValue)
       case ps of
         Nothing -> lift $ assign (wordpress . snapletValue)
                                  (Wordpress red ht act (Just IntSet.empty) wpconf)
         Just _ -> return ()
       codeGen $ yieldPureText ""

data TaxSpec = TaxPlus Text | TaxMinus Text deriving (Eq, Ord)

data TaxSpecId = TaxPlusId Int | TaxMinusId Int deriving (Eq, Show, Ord)

instance Show TaxSpec where
  show (TaxPlus t) = "+" ++ (T.unpack t)
  show (TaxMinus t) = "-" ++ (T.unpack t)

tagChars = ['a'..'z'] ++ "-"

instance Read TaxSpec where
  readsPrec _ ('+':cs) | all (`elem` tagChars) cs = [(TaxPlus (T.pack cs), "")]
  readsPrec _ ('-':cs) | all (`elem` tagChars) cs = [(TaxMinus (T.pack cs), "")]
  readsPrec _ cs | all (`elem` tagChars) cs = [(TaxPlus (T.pack cs), "")]
  readsPrec _ _ = []

newtype TaxSpecList = TaxSpecList { unTaxSpecList :: [TaxSpec]} deriving (Eq, Ord)

instance Show TaxSpecList where
  show (TaxSpecList ts) = intercalate "," (map show ts)

instance Read TaxSpecList where
  readsPrec _ ts = let vs = map (readSafe) $ T.splitOn "," $ T.pack ts in
                     if all isJust vs
                        then [(TaxSpecList $ catMaybes vs, "")]
                        else []


runningQueryFor :: CacheKey
                -> Handler b (Wordpress b) Bool
runningQueryFor cacheKey =
  do (Wordpress _ _ act _ _) <- view snapletValue <$> getSnapletState
     now <- liftIO $ getCurrentTime
     liftIO $ modifyMVar act $ \a ->
      let active = filterCurrent now a
      in if Map.member cacheKey active
          then return (active, True)
          else return (Map.insert cacheKey now active, False)
  where filterCurrent now = Map.filter (\v -> diffUTCTime now v < 1)

markDoneRunning :: CacheKey
                -> Handler b (Wordpress b) ()
markDoneRunning cacheKey =
  do (Wordpress _ _ act _ _) <- view snapletValue <$> getSnapletState
     liftIO $ modifyMVar_ act $ return .    Map.delete cacheKey

--constructPostCacheKey :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe TaxSpecList -> [(Int, Text)] -> Maybe TaxSpecList -> [(Int, Text)] -> CacheKey

wpPostsSplice :: WordpressConfig (Handler b b)
              -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
              -> Splice (Handler b b)
wpPostsSplice wpconf wordpress =
  do promise <- newEmptyPromise
     outputChildren <- manyWithSplices runChildren (postSplices (extraFields wpconf))
                                                   (getPromise promise)
     n <- getParamNode
     let limit = (fromMaybe 20 $ readSafe =<< X.getAttribute "limit" n) :: Int
         num = (fromMaybe 20 $ readSafe =<< X.getAttribute "num" n) :: Int
         offset' = (fromMaybe 0 $ readSafe =<< X.getAttribute "offset" n) :: Int
         page' = (fromMaybe 1 $ readSafe =<< X.getAttribute "page" n) :: Int
         page = if page' < 1 then 1 else page'
         offset = num * (page - 1) + offset'
         tags' = unTaxSpecList (fromMaybe (TaxSpecList []) $ readSafe =<< X.getAttribute "tags" n)
         cats' = unTaxSpecList (fromMaybe (TaxSpecList []) $ readSafe =<< X.getAttribute "categories" n)
         getPosts =
          do (Wordpress _ req _ _ _) <- lift $ use (wordpress . snapletValue)
             tags <- lift $ with wordpress $ lookupTagIds (endpoint wpconf) tags'
             cats <- lift $ with wordpress $ lookupCategoryIds (endpoint wpconf) cats'
             let cacheKey = cacheKey' num offset tags cats
             cached <- lift $ with wordpress $ cacheGet' (cacheBehavior wpconf) cacheKey
             case cached of
               Just r -> return r
               Nothing ->
                 do running <- lift $ with wordpress $ runningQueryFor cacheKey
                    if running
                       then do liftIO $ threadDelay 100000
                               getPosts
                       else
                         do let endpt = endpoint wpconf
                            h <- liftIO $ req (endpt <> "/posts") $ buildParams cacheKey
                            lift $ with wordpress $ do
                              cacheSet' (cacheBehavior wpconf) cacheKey h
                              markDoneRunning cacheKey
                            return h
     return $ yieldRuntime $
       do res <- getPosts
          case (decodeStrict . T.encodeUtf8 $ res) of
            Just posts -> do let postsW = extractPostIds posts
                             (Wordpress _ _ _ postSet _) <- lift (use (wordpress . snapletValue))
                             let postsND = noDuplicates postSet . take limit $ postsW
                             lift $ addPostIds wordpress (map fst postsND)
                             putPromise promise (map snd postsND)
                             codeGen outputChildren
            Nothing -> codeGen (yieldPureText "")
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just postSet) = filter (\(i,_) -> IntSet.notMember i postSet)

cacheKey' :: Int -> Int -> [TaxSpecId] -> [TaxSpecId] -> CacheKey
cacheKey' num offset tags cats =
  PostsKey (Set.fromList $ [ NumFilter num , OffsetFilter offset]
            ++ map TagFilter tags ++ map CatFilter cats)


buildParams :: CacheKey -> [(Text, Text)]
buildParams (PostsKey filters) = params
  where params = Set.toList $ Set.map mkFilter filters
        mkFilter (TagFilter (TaxPlusId i)) = ("filter[tag__in]", tshow i)
        mkFilter (TagFilter (TaxMinusId i)) = ("filter[tag__not_in]", tshow i)
        mkFilter (CatFilter (TaxPlusId i)) = ("filter[category__in]", tshow i)
        mkFilter (CatFilter (TaxMinusId i)) = ("filter[category__not_in]", tshow i)
        mkFilter (NumFilter num) = ("filter[posts_per_page]", tshow num)
        mkFilter (OffsetFilter offset) = ("filter[offset]", tshow offset)

type TaxRes = (Int, Text)

instance FromJSON TaxRes where
  parseJSON (Object o) = ((,) <$> o .: "ID" <*> o .: "slug")
  parseJSON _ = mzero

filterBySnd :: (b -> Bool) -> [(a, b)] -> [(a, b)]
filterBySnd pred = filter (\(_, slug) -> pred slug)

lookupTagIds :: Text -> [TaxSpec] -> Handler b (Wordpress b) [TaxSpecId]
lookupTagIds = lookupTaxIds "/taxonomies/post_tag/terms" "tag"

lookupCategoryIds :: Text -> [TaxSpec] -> Handler b (Wordpress b) [TaxSpecId]
lookupCategoryIds = lookupTaxIds "/taxonomies/category/terms" "category"

lookupTaxIds :: Text -> Text -> Text -> [TaxSpec] -> Handler b (Wordpress b) [TaxSpecId]
lookupTaxIds _ _ _ [] = return []
lookupTaxIds url desc end specs =
  do (Wordpress _ req _ _ _) <- view snapletValue <$> getSnapletState
     res <- liftIO $ req (end <> url) []
     let (Just taxs) = decodeStrict $ T.encodeUtf8 res :: Maybe [TaxRes]
     return $ map (getSpecId taxs) specs
  where getSpecId taxs (TaxPlus slug) = matchWith taxs slug TaxPlusId
        getSpecId taxs (TaxMinus slug) = matchWith taxs slug TaxMinusId
        matchWith taxs slug constr =
          case (filterBySnd (slug ==) taxs) of
           [] -> error $ "Couldn't find " ++ T.unpack desc ++ ": " ++ T.unpack slug
           ((i,_):_) -> constr i

extractPostIds :: [Object] -> [(Int, Object)]
extractPostIds = map extractPostId

extractPostId :: Object -> (Int, Object)
extractPostId p = let i = M.lookup "ID" p
                      is = case i of
                            Just (String n) -> readSafe n
                            Just (Number n) ->
                              let rat = toRational n in
                              case denominator rat of
                                1 -> Just $ fromInteger (numerator rat)
                                _ -> Nothing
                            _ -> Nothing
                      id' = fromMaybe 0 is in
                    (id', p)

addPostIds :: Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b)) -> [Int] -> Handler b b ()
addPostIds wordpress ids =
  do (Wordpress red req act postSet conf) <- use (wordpress . snapletValue)
     assign (wordpress . snapletValue)
            (Wordpress red req act ((`IntSet.union` (IntSet.fromList ids)) <$> postSet) conf)

getPost :: CacheKey
        -> Handler b (Wordpress b) (Maybe Object)
getPost cacheKey@(PostByPermalinkKey year month slug) =
  do (Wordpress _ req _ posts conf) <- view snapletValue <$> getSnapletState
     mres <- cacheGet' (cacheBehavior conf) cacheKey
     case mres of
       Just r' -> return (decodeStrict . T.encodeUtf8 $ r')
       Nothing ->
         do running <- runningQueryFor cacheKey
            if running
               then do liftIO $ threadDelay 100000
                       getPost cacheKey
               else do h <- liftIO $ req (endpoint conf <> "/posts")
                              [("filter[year]",year)
                              ,("filter[monthnum]", month)
                              ,("filter[name]", slug)]
                       let post' = decodeStrict . T.encodeUtf8 $ h
                       case post' of
                         Just (post:_) ->
                           do cacheSet' (cacheBehavior conf) cacheKey
                                (TL.toStrict . TL.decodeUtf8 . encode $ post)
                              markDoneRunning cacheKey
                              return $ Just post
                         _ -> do markDoneRunning cacheKey
                                 return Nothing
getPost key = error $ "getPost: Don't know how to get a post from key: " ++ show key

wpPostByPermalinkSplice :: WordpressConfig (Handler b b)
                        -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                        -> Splice (Handler b b)
wpPostByPermalinkSplice conf wordpress =
  do promise <- newEmptyPromise
     outputChildren <- withSplices runChildren (postSplices (extraFields conf)) (getPromise promise)
     return $ yieldRuntime $
       do mperma <- (parsePermalink . T.decodeUtf8 . rqURI) <$> lift getRequest
          case mperma of
            Nothing -> codeGen (yieldPureText "")
            Just (year, month, slug) ->
              do res <- lift $ with wordpress $ getPost (PostByPermalinkKey year month slug)
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


type Year = Text
type Month = Text
type Slug = Text
data Filter = TagFilter TaxSpecId
            | CatFilter TaxSpecId
            | NumFilter Int
            | OffsetFilter Int
            deriving (Eq, Ord)

instance Show Filter where
  show (TagFilter t) = "tag_" ++ show t
  show (CatFilter t) = "cat_" ++ show t
  show (NumFilter n) = "num_" ++ show n
  show (OffsetFilter n) = "offset_" ++ show n

data CacheKey = PostKey Int
              | PostByPermalinkKey Year Month Slug
              | PostsKey (Set Filter)
              deriving (Eq, Show, Ord)

formatKey :: CacheKey -> ByteString
formatKey = T.encodeUtf8 . format
  where format (PostByPermalinkKey y m s) = "wordpress:post_perma:" <> y <> "_" <> m <> "_" <> s
        format (PostsKey filters) =
          "wordpress:posts:" <> T.intercalate "_" (map tshow $ Set.toAscList filters)
        format (PostKey n) = "wordpress:post:" <> tshow n

cacheGet' :: CacheBehavior -> CacheKey -> Handler b (Wordpress b) (Maybe Text)
cacheGet' NoCache _ = return Nothing
cacheGet' _ cacheKey = cacheGet cacheKey

cacheGet :: CacheKey -> Handler b (Wordpress b) (Maybe Text)
cacheGet key = do (Wordpress run _ _ _ _) <- view snapletValue <$> getSnapletState
                  res <- run $ R.get (formatKey key)
                  case res of
                   Right (Just val) ->
                     case key of
                      PostByPermalinkKey{} ->
                        do res' <- run $ R.get val
                           case res' of
                            Left err -> return Nothing
                            Right val -> return (T.decodeUtf8 <$> val)
                      _ -> return (Just $ T.decodeUtf8 val)
                   _ -> return Nothing

cacheSet' :: CacheBehavior -> CacheKey -> Text -> Handler b (Wordpress b) ()
cacheSet' NoCache _ _ = return ()
cacheSet' (CacheSeconds n) b c = void $ cacheSet (Just n) b c
cacheSet' (CacheForever) b c = void $ cacheSet Nothing b c

cacheSet :: Maybe Int -> CacheKey -> Text -> Handler b (Wordpress b) Bool
cacheSet cachetime key o =
  do (Wordpress run _ _ _ _) <- view snapletValue <$> getSnapletState
     res <- case key of
              PostByPermalinkKey{} ->
                do let (Just p) = decodeStrict . T.encodeUtf8 $ o
                       (i,_) = extractPostId p
                   r <- run $ set cachetime (formatKey $ PostKey i) (T.encodeUtf8 o)
                   case r of
                     Left err -> return r
                     Right _ ->
                       run $ set cachetime (formatKey key) (formatKey $ PostKey i)
              _ -> run $ set cachetime (formatKey key) (T.encodeUtf8 o)
     case res of
       Left err -> return False
       Right val -> return True
  where set (Just n) k v = R.setex k (toInteger n) v
        set Nothing k v = R.set k v

expireAggregates :: Handler b (Wordpress b) Bool
expireAggregates =
  do (Wordpress run _ _ _ _) <- view snapletValue <$> getSnapletState
     do r <- run $ R.eval "return redis.call('del', unpack(redis.call('keys', ARGV[1])))" [] ["wordpress:posts:*"]
        case r of
          Left _ -> return False
          Right (_ :: Integer) -> return True

expirePost :: Int -> Handler b (Wordpress b) Bool
expirePost n =
  do (Wordpress run _ _ _ _) <- view snapletValue <$> getSnapletState
     r1 <- run $ R.del [formatKey $ PostKey n]
     case r1 of
       Left _ -> return False
       _ -> expireAggregates

wreqRequester :: WordpressConfig (Handler b b) -> Text -> Text -> Text -> [(Text, Text)] -> IO Text
wreqRequester conf user pass u ps =
  do let opts = (W.defaults & W.params .~ ps
                            & W.auth .~ W.basicAuth user' pass'
                            )
     -- print (u, ps, view W.headers opts, view W.auth opts)
     wplog conf $ "wreq: " <> u <> " with params: " <>
                  (T.intercalate "&" . map (\(a,b) -> a <> "=" <> b) $ ps)
     r <- W.getWith opts (T.unpack u)
     return $ TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody
  where user' = T.encodeUtf8 user
        pass' = T.encodeUtf8 pass

wplog :: WordpressConfig (Handler b b) -> Text -> IO ()
wplog conf msg = case logger conf of
                   Nothing -> return ()
                   Just f -> f msg
