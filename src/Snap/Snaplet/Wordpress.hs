{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Snap.Snaplet.Wordpress (
   Wordpress(..)
 , WordpressConfig(..)
 , CachePeriod(..)
 , initWordpress
 , initWordpress'
 , getPost
 , CacheKey(..)
 , Filter(..)
 , cacheLookup
 , cacheSet
 , expirePost
 , expireAggregates

 , transformName
 , TaxSpec(..)
 , TaxSpecList(..)
 , Field(..)
 , mergeFields
 ) where

import           Prelude                  hiding ((++))

import           Blaze.ByteString.Builder
import           Control.Applicative
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
import           Data.List                (intercalate, partition)
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
import qualified Data.Vector              as V
import           Database.Redis           (Redis)
import qualified Database.Redis           as R
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

(++) :: Monoid a => a -> a -> a
(++) = mappend

readSafe :: Read a => Text -> Maybe a
readSafe = fmap fst . listToMaybe . reads . T.unpack

tshow :: Show a => a -> Text
tshow = T.pack . show

data CachePeriod = NoCache | CacheSeconds Int deriving (Show, Eq)

data WordpressConfig m = WordpressConfig { endpoint    :: Text
                                         , requester   :: Maybe (Text -> [(Text, Text)] -> IO Text)
                                         , cachePeriod :: CachePeriod
                                         , extraFields :: [Field m]
                                         }

instance Default (WordpressConfig m) where
  def = WordpressConfig "http://127.0.0.1/wp-json" Nothing (CacheSeconds 600) []

data Wordpress b = Wordpress { runRedis       :: forall a. Redis a -> Handler b (Wordpress b) a
                             , runHTTP        :: Text -> [(Text, Text)] -> IO Text
                             , requestPostSet :: Maybe IntSet
                             }


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
                              return $ wreqRequester u p
                Just r -> return r
       return $ Wordpress (withTop' id . R.runRedisDB redis) req Nothing

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
    do (Wordpress red ht ps) <- lift $ use (wordpress . snapletValue)
       case ps of
         Nothing -> lift $ assign (wordpress . snapletValue) (Wordpress red ht (Just IntSet.empty))
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

wpPostsSplice :: WordpressConfig (Handler b b)
              -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
              -> Splice (Handler b b)
wpPostsSplice conf wordpress =
  do promise <- newEmptyPromise
     outputChildren <- manyWithSplices runChildren (postSplices (extraFields conf))
                                                   (getPromise promise)
     n <- getParamNode
     let limit = (fromMaybe 20 $ readSafe =<< X.getAttribute "limit" n) :: Int
         num = (fromMaybe 20 $ readSafe =<< X.getAttribute "num" n) :: Int
         offset' = (fromMaybe 0 $ readSafe =<< X.getAttribute "offset" n) :: Int
         page' = (fromMaybe 1 $ readSafe =<< X.getAttribute "page" n) :: Int
         page = if page' < 1 then 1 else page'
         offset = num * (page - 1) + offset'
         tags' = unTaxSpecList (fromMaybe (TaxSpecList []) $
                                readSafe =<< X.getAttribute "tags" n)
         cats' = unTaxSpecList (fromMaybe (TaxSpecList []) $
                                readSafe =<< X.getAttribute "categories" n)
     (tagsPlus, tagsMinus) <- partition (\t -> case t of
                                                 (TaxPlusId _) -> True
                                                 (TaxMinusId _) -> False) <$>
                                (lift $ lookupTagIds conf tags')
     (catsPlus, catsMinus) <- partition (\t -> case t of
                                                  (TaxPlusId _) -> True
                                                  (TaxMinusId _) -> False) <$>
                                      (lift $ lookupCategoryIds conf cats')
     let cacheKey = PostsKey (Set.fromList $ [ LimitFilter limit
                                             , NumFilter num
                                             , OffsetFilter offset
                                             , PageFilter page
                                             ] ++ map TagFilter tags' ++ map CatFilter cats')
     return $ yieldRuntime $
       do (Wordpress _ req postSet) <- lift (use (wordpress . snapletValue))
          cached <- case cachePeriod conf of
                      NoCache -> return Nothing
                      _ -> lift $ with wordpress $ cacheLookup cacheKey
          res <-
            case cached of
              Just r -> return r
              Nothing ->
                do h <- liftIO $
                     req (endpoint conf ++ "/posts") $
                         [("filter[posts_per_page]", tshow num)
                         ,("filter[offset]", tshow offset)
                         ] ++ (map (\(TaxPlusId i) -> ("filter[tag__in]", tshow i)) tagsPlus)
                           ++ (map (\(TaxMinusId i) -> ("filter[tag__not_in]", tshow i)) tagsMinus)
                           ++ (map (\(TaxPlusId i) -> ("filter[category__in]", tshow i)) catsPlus)
                           ++ (map (\(TaxMinusId i) -> ("filter[category__not_in]", tshow i)) catsMinus)
                   case cachePeriod conf of
                     NoCache -> return ()
                     CacheSeconds n -> void $ lift $ with wordpress $ cacheSet n cacheKey h
                   return h

          case (decodeStrict . T.encodeUtf8 $ res) of
            Just posts -> do let postsW = extractPostIds posts
                             let postsND = noDuplicates postSet . take limit $ postsW
                             lift $ addPostIds wordpress (map fst postsND)
                             putPromise promise (map snd postsND)
                             codeGen outputChildren
            Nothing -> codeGen (yieldPureText "")
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just postSet) = filter (\(i,_) -> IntSet.notMember i postSet)

newtype TaxRes = TaxRes (Int, Text)

instance FromJSON TaxRes where
  parseJSON (Object o) = TaxRes <$> ((,) <$> o .: "ID" <*> o .: "slug")
  parseJSON _ = mzero

lookupTagIds = lookupTaxIds "/taxonomies/post_tag/terms" "tag"
lookupCategoryIds = lookupTaxIds "/taxonomies/category/terms" "category"

lookupTaxIds :: Text -> Text -> WordpressConfig (Handler b b) -> [TaxSpec] -> IO [TaxSpecId]
lookupTaxIds _ _ _ [] = return []
lookupTaxIds url desc conf specs =
  do res <- W.get $ (T.unpack $ endpoint conf) ++ (T.unpack url)
     let (Just tags) = decode $ res ^. W.responseBody :: Maybe [TaxRes]
     return $ map (getSpecId tags) specs
 where getSpecId tags (TaxPlus slug) = matchWith tags slug TaxPlusId
       getSpecId tags (TaxMinus slug) = matchWith tags slug TaxMinusId
       matchWith tags slug constr =
         case filter (\(TaxRes (_,s)) -> s == slug) tags of
            [] -> error $ "Couldn't find " ++ T.unpack desc ++ ": " ++ T.unpack slug
            (TaxRes  (i,_):_) -> constr i

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
  do (Wordpress red req postSet) <- use (wordpress . snapletValue)
     assign (wordpress . snapletValue)
            (Wordpress red req ((`IntSet.union` (IntSet.fromList ids)) <$> postSet))


getPost :: WordpressConfig (Handler b b)
        -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
        -> CacheKey
        -> Handler b b (Maybe Object)
getPost conf wordpress cacheKey@(PostByPermalinkKey year month slug) =
  do mres <- case cachePeriod conf of
               NoCache -> return Nothing
               _ -> with wordpress $ cacheLookup cacheKey
     case mres of
       Just r' -> return (decodeStrict . T.encodeUtf8 $ r')
       Nothing ->
         do (Wordpress _ req posts) <- use (wordpress . snapletValue)
            h <- liftIO $ req (endpoint conf ++ "/posts")
                              [("filter[year]",year)
                              ,("filter[monthnum]", month)
                              ,("filter[name]", slug)]
            let post' = decodeStrict . T.encodeUtf8 $ h
            case post' of
              Just (post:_) ->
                do case cachePeriod conf of
                     NoCache -> return ()
                     CacheSeconds n ->
                       void $ with wordpress $ cacheSet n cacheKey
                                                        (TL.toStrict . TL.decodeUtf8 . encode $ post)
                   return $ Just post
              _ -> return Nothing
getPost _ _ key = error $ "getPost: Don't know how to get a post from key: " ++ show key

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
              do res <- lift $ getPost conf wordpress (PostByPermalinkKey year month slug)
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
data Filter = TagFilter TaxSpec
            | CatFilter TaxSpec
            | NumFilter Int
            | OffsetFilter Int
            | PageFilter Int
            | LimitFilter Int
            deriving (Eq, Ord)

instance Show Filter where
  show (TagFilter t) = "tag_" ++ show t
  show (CatFilter t) = "cat_" ++ show t
  show (NumFilter n) = "num_" ++ show n
  show (OffsetFilter n) = "offset_" ++ show n
  show (PageFilter n) = "page_" ++ show n
  show (LimitFilter n) = "limit_" ++ show n

class FormatKey a where
  formatKey :: a -> ByteString

data CacheKey = PostKey Int
              | PostByPermalinkKey Year Month Slug
              | PostsKey (Set Filter)
              deriving (Eq, Show)

instance FormatKey CacheKey where
  formatKey (PostByPermalinkKey y m s) =
    T.encodeUtf8 $ "wordpress:post_perma:" ++ y ++ "_" ++ m ++ "_" ++ s
  formatKey (PostsKey filters) =
    T.encodeUtf8 $ "wordpress:posts:" ++ T.intercalate "_" (map tshow $ Set.toAscList filters)
  formatKey (PostKey n) = T.encodeUtf8 $ "wordpress:post:" ++ tshow n

cacheLookup :: CacheKey -> Handler b (Wordpress b) (Maybe Text)
cacheLookup key = do (Wordpress run _ _) <- view snapletValue <$> getSnapletState
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
cacheSet :: Int -> CacheKey -> Text -> Handler b (Wordpress b) Bool
cacheSet seconds key o =
  do (Wordpress run _ _) <- view snapletValue <$> getSnapletState
     res <- case key of
              PostByPermalinkKey{} ->
                do let (Just p) = decodeStrict . T.encodeUtf8 $ o
                       (i,_) = extractPostId p
                   r <- run $ R.setex (formatKey $ PostKey i)
                                      (toInteger seconds)
                                      (T.encodeUtf8 o)
                   case r of
                     Left err -> return r
                     Right _ ->
                       run $ R.setex (formatKey key)
                                     (toInteger seconds)
                                     (formatKey $ PostKey i)
              _ -> run $ R.setex (formatKey key)
                                 (toInteger seconds)
                                 (T.encodeUtf8 o)
     case res of
       Left err -> return False
       Right val -> return True

expireAggregates :: Handler b (Wordpress b) Bool
expireAggregates =
  do (Wordpress run _ _) <- view snapletValue <$> getSnapletState
     do r <- run $ R.eval "return redis.call('del', unpack(redis.call('keys', ARGV[1])))" [] ["wordpress:posts:*"]
        case r of
          Left _ -> return False
          Right (_ :: Integer) -> return True

expirePost :: Int -> Handler b (Wordpress b) Bool
expirePost n =
  do (Wordpress run _ _) <- view snapletValue <$> getSnapletState
     r1 <- run $ R.del [formatKey $ PostKey n]
     case r1 of
       Left _ -> return False
       _ -> expireAggregates

wreqRequester :: Text -> Text -> Text -> [(Text, Text)] -> IO Text
wreqRequester user pass u ps =
  do let opts = (W.defaults & W.params .~ ps
                            & W.auth .~ W.basicAuth user' pass'
                            )
     -- print (u, ps, view W.headers opts, view W.auth opts)
     r <- W.getWith opts (T.unpack u)
     return $ TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody
  where user' = T.encodeUtf8 user
        pass' = T.encodeUtf8 pass
