{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Prelude                  hiding ((++))

import           Blaze.ByteString.Builder
import           Configuration.Dotenv     (loadFile)
import           Control.Concurrent.MVar
import           Control.Lens             hiding ((.=))
import           Control.Monad            (mplus, void, when)
import           Control.Monad.State      (StateT, evalStateT)
import qualified Control.Monad.State      as S
import           Control.Monad.Trans      (liftIO)
import           Data.Aeson               hiding (Success)
import           Data.Default
import qualified Data.HashMap.Strict      as HM
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                 as Set
import           Data.Text                (Text, pack, unpack)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import qualified Database.Redis           as R
import           Larceny
import qualified Misc
import           Network.Wai              (Application, Response,
                                           defaultRequest, pathInfo,
                                           rawPathInfo)
import           System.Directory         (doesFileExist)
import           System.Environment       (lookupEnv)
import           Test.Hspec
import           Test.Hspec.Core.Spec     (Result (..))
import qualified Text.XmlHtml             as X
import           Web.Fn

import           Web.Offset
import           Web.Offset.Cache.Redis
import           Web.Offset.Splices       (wpPostsHelper)
import           Web.Offset.Types

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------

data Ctxt = Ctxt { _req       :: FnRequest
                 , _redis     :: R.Connection
                 , _wordpress :: Wordpress Ctxt
                 , _wpsubs    :: Substitutions Ctxt
                 , _lib       :: Library Ctxt
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

enc a = TL.toStrict . TL.decodeUtf8 . encode $ a

article2 = object [ "ID" .= (2 :: Int)
                  , "title" .= ("The post" :: Text)
                  , "excerpt" .= ("summary" :: Text)
                  ]

jacobinFields = [N "featured_image" [N "attachment_meta" [N "sizes" [N "mag-featured" [F "width"
                                                                                      ,F "height"
                                                                                      ,F "url"]
                                                                    ,N "single-featured" [F "width"
                                                                                         ,F "height"
                                                                                         ,F "url"]]]]]


site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ anything ==> larcenyServe ]
    `fallthrough` notFoundText "Page not found."

tplLibrary :: Library Ctxt
tplLibrary =
  M.fromList [(["single"], parse "<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp>")
             ,(["many"], parse "<wpPosts limit=2><wpTitle/></wpPosts>")
             ,(["many1"], parse "<wpPosts><wpTitle/></wpPosts>")
             ,(["many2"], parse "<wpPosts offset=1 limit=1><wpTitle/></wpPosts>")
             ,(["many3"], parse "<wpPosts offset=0 limit=1><wpTitle/></wpPosts>")
             ,(["page1"], parse "<wpPosts limit=10 page=1><wpTitle/></wpPosts>")
             ,(["page2"], parse "<wpPosts limit=10 page=2><wpTitle/></wpPosts>")
             ,(["num1"], parse "<wpPosts num=2><wpTitle/></wpPosts>")
             ,(["num2"], parse "<wpPosts num=2 page=2 limit=1><wpTitle/></wpPosts>")
             ,(["num3"], parse "<wpPosts num=1 page=3><wpTitle/></wpPosts>")
             ,(["tag1"], parse "<wpPosts tags=\"tag1\" limit=10><wpTitle/></wpPosts>")
             ,(["tag2"], parse "<wpPosts limit=10><wpTitle/></wpPosts>")
             ,(["tag3"], parse "<wpPosts tags=\"+tag1\" limit=10><wpTitle/></wpPosts>")
             ,(["tag4"], parse "<wpPosts tags=\"-tag1\" limit=1><wpTitle/></wpPosts>")
             ,(["tag5"], parse "<wpPosts tags=\"+tag1\" limit=1><wpTitle/></wpPosts>")
             ,(["tag6"], parse "<wpPosts tags=\"+tag1,-tag2\" limit=1><wpTitle/></wpPosts>")
             ,(["tag7"], parse "<wpPosts tags=\"+tag1,+tag2\" limit=1><wpTitle/></wpPosts>")
             ,(["cat1"], parse "<wpPosts categories=\"cat1\" limit=10><wpTitle/></wpPosts>")
             ,(["cat2"], parse "<wpPosts limit=10><wpTitle/></wpPosts>")
             ,(["cat3"], parse "<wpPosts categories=\"-cat1\" limit=10><wpTitle/></wpPosts>")
             ,(["author-date"], parse "Hello<wp><wpPostByPermalink><wpAuthor><wpName/></wpAuthor><wpDate><wpYear/>/<wpMonth/></wpDate></wpPostByPermalink></wp>")
             ,(["fields"], parse "<wp><wpPosts limit=1 categories=\"-cat1\"><wpFeaturedImage><wpAttachmentMeta><wpSizes><wpThumbnail><wpUrl/></wpThumbnail></wpSizes></wpAttachmentMeta></wpFeaturedImage></wpPosts></wp>")
               ]

larcenyServe :: Ctxt ->
                IO (Maybe Response)
larcenyServe ctxt = do
  let p = pathInfo . fst $ getRequest ctxt
  mplus <$> renderLarceny ctxt (T.intercalate "/" p)
        <*> renderLarceny ctxt (T.intercalate "/" (p <> ["index"]))

renderLarceny :: Ctxt ->
                 Text ->
                 IO (Maybe Response)
renderLarceny ctxt name =
  do let tpl = M.lookup [name] tplLibrary
     case tpl of
       Just t -> do
         rendered <- evalStateT (runTemplate t [name] (ctxt ^. wpsubs) tplLibrary) ctxt
         okHtml rendered
       _ -> return Nothing

renderLarceny2 :: Ctxt ->
                  Text ->
                  IO (Maybe Text)
renderLarceny2 ctxt name =
  do let tpl = M.lookup [name] tplLibrary
     case tpl of
       Just t -> do
         rendered <- evalStateT (runTemplate t [name] (ctxt ^. wpsubs) tplLibrary) ctxt
         return $ Just rendered
       _ -> return Nothing

fauxRequester :: Text -> Text -> [(Text, Text)] -> IO Text
fauxRequester _ "/taxonomies/post_tag/terms" [] =
  return $ enc [object [ "ID" .= (177 :: Int)
                                 , "slug" .= ("home-featured" :: Text)
                                 ]
                         ,object [ "ID" .= (160 :: Int)
                                 , "slug" .= ("featured-global" :: Text)
                                 ]
                         ]
fauxRequester _ "/taxonomies/category/terms" [] =
          return $ enc [object [ "ID" .= (159 :: Int)
                                 , "slug" .= ("bookmarx" :: Text)
                                 , "meta" .= object ["links" .= object ["self" .= ("/159" :: Text)]]
                                 ]
                         ]
fauxRequester response rqPath rqParams =
  --modifyMVar_ record $ (return . (<> [mkUrlUnescape url params]))
  return response


fauxRequester2 :: MVar [Text] -> Text -> [(Text, Text)] -> IO Text
fauxRequester2 _  "/taxonomies/post_tag/terms" [] =
  return $ enc [object [ "ID" .= (177 :: Int)
                                 , "slug" .= ("home-featured" :: Text)
                                 ]
                         ,object [ "ID" .= (160 :: Int)
                                 , "slug" .= ("featured-global" :: Text)
                                 ]
                         ]
fauxRequester2 _ "/taxonomies/category/terms" [] =
          return $ enc [object [ "ID" .= (159 :: Int)
                                 , "slug" .= ("bookmarx" :: Text)
                                 , "meta" .= object ["links" .= object ["self" .= ("/159" :: Text)]]
                                 ]
                         ]
fauxRequester2 record rqPath rqParams = do
  modifyMVar_ record $ (return . (<> [mkUrlUnescape rqPath rqParams]))
  return ""
mkUrlUnescape url params = (url <> "?" <> (T.intercalate "&" $ map (\(k, v) -> k <> "=" <> v) params))


initializer response =
  do  -- Load environment variables, or use defaults
     let lookupWithDefault key def = pack <$> fromMaybe def <$> lookupEnv key
     envExists <- doesFileExist ".env"
     when envExists $ loadFile False ".env"

     -- get redis connection information
     rconn <- R.connect R.defaultConnectInfo

     let wpconf = def { wpConfEndpoint = ""
                      , wpConfLogger = Just (putStrLn . T.unpack)
                      , wpConfRequester = Right $ Requester (fauxRequester response)
                      , wpConfExtraFields = []
                      , wpConfCacheBehavior = NoCache
                   }
     let getUri :: StateT Ctxt IO Text
         getUri = do ctxt <- S.get
                     return (T.decodeUtf8 . rawPathInfo . fst . getRequest $ ctxt)
     (wp,wpSubs) <- initWordpress wpconf rconn getUri wordpress
     let allSubs = wpSubs
     let tpls = mempty --Larceny.loadTemplates "templates"
     return (Ctxt defaultFnRequest rconn wp allSubs tpls)


initializer2 record cache =
  do  -- Load environment variables, or use defaults
     let lookupWithDefault key def = pack <$> fromMaybe def <$> lookupEnv key
     envExists <- doesFileExist ".env"
     when envExists $ loadFile False ".env"

     -- get redis connection information
     rconn <- R.connect R.defaultConnectInfo

     let wpconf = def { wpConfEndpoint = ""
                      , wpConfLogger = Just (putStrLn . T.unpack)
                      , wpConfRequester = Right $ Requester (fauxRequester2 record)
                      , wpConfExtraFields = []
                      , wpConfCacheBehavior = if cache then CacheSeconds 60 else NoCache
                   }
     let getUri :: StateT Ctxt IO Text
         getUri = do ctxt <- S.get
                     return (T.decodeUtf8 . rawPathInfo . fst . getRequest $ ctxt)
     (wp,wpSubs) <- initWordpress wpconf rconn getUri wordpress
     let allSubs = wpSubs
     let tpls = mempty --Larceny.loadTemplates "templates"
     return (Ctxt defaultFnRequest rconn wp allSubs tpls)

initializer3 :: IO Ctxt
initializer3 =
  do rconn <- R.connect R.defaultConnectInfo
     let wpconf = def { wpConfEndpoint = "http://127.0.0.1:5555/wp-json"
                      , wpConfLogger = Nothing
                      , wpConfRequester = Left ("offset", "111")
                      , wpConfExtraFields = []
                      , wpConfCacheBehavior = NoCache
                   }
     let getUri :: StateT Ctxt IO Text
         getUri = do ctxt <- S.get
                     return (T.decodeUtf8 . rawPathInfo . fst . getRequest $ ctxt)
     (wp,wpSubs) <- initWordpress wpconf rconn getUri wordpress
     return (Ctxt defaultFnRequest rconn wp wpSubs tplLibrary)

app :: (Ctxt -> IO Response) -> IO Application
app site  =
  do ctxt <- initializer "wow!"
     return (toWAI ctxt site)

app' :: Ctxt -> IO Application
app' ctxt = app site

{-
cachingApp :: SnapletInit App App
cachingApp = makeSnaplet "app" "An snaplet example application." Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  r <- nestSnaplet "" redis redisDBInitConf
  (w,splices) <- liftIO $ initWordpress config (view (snapletValue . redisConnection) r) (T.decodeUtf8 . rqURI <$> getRequest) wordpress
  addConfig h $ set scInterpretedSplices splices mempty
  return $ App h r w
  where config = (def { wpConfEndpoint = ""
                      , wpConfRequester = Right $ Requester (\_ _ -> return "")
                      , wpConfCacheBehavior = CacheSeconds 10})

--}

----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

clearRedisCache :: Ctxt -> IO Bool
clearRedisCache ctxt = R.runRedis (_redis ctxt) (rdelstar "wordpress:*")

article1 :: Value
article1 = object [ "ID" .= ("1" :: Text)
                  , "title" .= ("Foo bar" :: Text)
                  , "excerpt" .= ("summary" :: Text)
                  ]

unobj :: Value -> Object
unobj (Object x) = x
unobj _ = error "Not an object"

shouldRender :: Text
             -> Text
             -> Expectation
shouldRender t output = do
  ctxt <- initializer $ enc [ article1 ]
  let s = _wpsubs ctxt
  rendered <- evalStateT (runTemplate (toTpl t) [] s mempty) ctxt
  rendered `shouldBeIgnoreWhitespace` output

type TemplateName = Text
type TemplateText = Text

shouldRender' :: TemplateName
              -> Text
              -> Expectation
shouldRender' name output = do
  ctxt <- initializer3
  rendered <- renderLarceny2 ctxt name
  rendered `shouldMaybeIgnoreWhitespace` Just output

shouldNotRenderTheSameAs :: (TemplateName, Ctxt)
                         -> TemplateName
                         -> Expectation
shouldNotRenderTheSameAs (name1, ctxt) name2 = do
  rendered1 <- renderLarceny2 ctxt name1
  rendered2 <- renderLarceny2 ctxt name2
  rendered1 `shouldMaybeNotIgnoreWhitespace` rendered2

shouldRenderTheSameAs :: (TemplateName, Ctxt)
                      -> TemplateName
                      -> Expectation
shouldRenderTheSameAs (name1, ctxt) name2 = do
  rendered1 <- renderLarceny2 ctxt name1
  rendered2 <- renderLarceny2 ctxt name2
  rendered1 `shouldMaybeIgnoreWhitespace` rendered2

toTpl tpl = parse (TL.fromStrict tpl)

shouldBeIgnoreWhitespace a b =
  T.replace " " "" a `shouldBe` T.replace " " "" b

shouldMaybeIgnoreWhitespace a b =
  T.replace " " "" <$> a `shouldBe` T.replace " " "" <$> b

shouldMaybeNotIgnoreWhitespace a b =
  T.replace " " "" <$> a `shouldNotBe` T.replace " " "" <$> b

shouldQueryTo :: Text -> [Text] -> Spec
shouldQueryTo hQuery wpQuery =
  it ("query from " <> T.unpack hQuery) $ do
      record <- liftIO $ newMVar []
      ctxt <- liftIO $ initializer2 record False
      let s = _wpsubs ctxt
      evalStateT (runTemplate (toTpl hQuery) [] s mempty) ctxt
      x <- liftIO $ tryTakeMVar record
      x `shouldBe` Just wpQuery

shouldRenderAtUrl' :: (Text, Text) -> Text -> Expectation
shouldRenderAtUrl' (url, t) match = do
    ctxt <- initializer $ enc [ article1 ]
    let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 url }
    let ctxt' = setRequest ctxt
                 $ (\(x,y) -> (requestWithUrl, y)) defaultFnRequest
    let s = _wpsubs ctxt
    rendered <- evalStateT (runTemplate (toTpl t) [] s mempty) ctxt'
    match `shouldBe` rendered

shouldQueryAtUrl :: (Text, Text) -> [Text] -> Spec
shouldQueryAtUrl (url, hQuery) wpQuery =
  it ("query from " <> T.unpack hQuery) $ do
      record <- liftIO $ newMVar []
      let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 url }
      ctxt <- liftIO $ initializer2 record True
      let ctxt' = setRequest ctxt
            $ (\(x,y) -> (requestWithUrl, y)) defaultFnRequest
      let s = _wpsubs ctxt'
      evalStateT (runTemplate (toTpl hQuery) [] s mempty) ctxt'
      x <- liftIO $ tryTakeMVar record
      x `shouldBe` Just wpQuery


main :: IO ()
main = hspec $ do
  Misc.tests
  describe "<wpPosts>" $ do
    ("<wpTitle/>", unobj article1)
      `shouldRenderWpPostsTo` "Foo bar"
    ("<wpID/>", unobj article1)
      `shouldRenderWpPostsTo` "1"
    ("<wpExcerpt/>", unobj article1)
      `shouldRenderWpPostsTo` "summary"

  describe "<wpNoPostDuplicates/>" $ do
    it "should not duplicate any posts after call to wpNoPostDuplicates" $
      let tpl = "<wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts>"
      in do
        ctxt <- initializer $ enc [article1]
        let subs = _wpsubs ctxt
        rendered <- evalStateT (runTemplate (toTpl tpl) [] subs mempty) ctxt
        rendered `shouldBeIgnoreWhitespace` "Foo bar"
    it "should ignore duplicates if they were rendered before wpNoPostDuplicates" $ do
      "<wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts>" `shouldRender` "Foo barFoo bar"
      "<wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts>" `shouldRender` "Foo barFoo bar"
    it "should have no effect if it's at the end of the template" $
      "<wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/>" `shouldRender` "Foo barFoo bar"

  describe "<wpPostByPermalink>" $ do
    ("/2009/10/the-post/",
     "<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp>")
      `shouldQueryAtUrl`
      ["/posts?filter[year]=2009&filter[monthnum]=10&filter[name]=the-post"]
    it "should render stuff" $
      ("/2009/10/the-post/", "<wp><wpNoPostDuplicates/><wpPostByPermalink><wpTitle/></wpPostByPermalink><wpPosts limit=1><wpTitle/></wpPosts></wp>") `shouldRenderAtUrl'` "Foo bar"

{-    describe "should grab post from cache if it's there" $
      let (Object a2) = article2 in
      shouldRenderAtUrlPreCache
        (void $ with wordpress $ cacheSet (Just 10) (PostByPermalinkKey "2001" "10" "the-post")
                                            (enc a2))
        "/2001/10/the-post/"
        "<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp>"
        "The post" -}

{-
  describe "caching" $ snap (route []) cachingApp $ afterEval (void clearRedisCache) $ do
    it "should find nothing for a non-existent post" $ do
      p <- eval (wpCacheGet' wordpress (PostByPermalinkKey "2000" "1" "the-article"))
      p `shouldEqual` Nothing
    it "should find something if there is a post in cache" $ do
      eval (wpCacheSet' wordpress (PostByPermalinkKey "2000" "1" "the-article")
                                  (enc article1))
      p <- eval (wpCacheGet' wordpress (PostByPermalinkKey "2000" "1" "the-article"))
      p `shouldEqual` (Just $ enc article1)
    it "should not find single post after expire handler is called" $
      do eval (wpCacheSet' wordpress (PostByPermalinkKey "2000" "1" "the-article")
                                     (enc article1))
         eval (wpExpirePost' wordpress (PostByPermalinkKey "2000" "1" "the-article"))
         eval (wpCacheGet' wordpress (PostByPermalinkKey "2000" "1" "the-article"))
           >>= shouldEqual Nothing
    it "should find post aggregates in cache" $
      do let key = PostsKey (Set.fromList [NumFilter 20, OffsetFilter 0])
         eval (wpCacheSet' wordpress key ("[" <> enc article1 <> "]"))
         eval (wpCacheGet' wordpress key)
           >>= shouldEqual (Just $ "[" <> enc article1 <> "]")
    it "should not find post aggregates after expire handler is called" $
      do let key = PostsKey (Set.fromList [NumFilter 20, OffsetFilter 0])
         eval (wpCacheSet' wordpress key ("[" <> enc article1 <> "]"))
         eval (wpExpirePost' wordpress (PostByPermalinkKey "2000" "1" "the-article"))
         eval (wpCacheGet' wordpress key)
           >>= shouldEqual Nothing
    it "should find single post after expiring aggregates" $
      do eval (wpCacheSet' wordpress (PostByPermalinkKey "2000" "1" "the-article")
                          (enc article1))
         eval (wpExpireAggregates' wordpress)
         eval (wpCacheGet' wordpress (PostByPermalinkKey "2000" "1" "the-article"))
           >>= shouldNotEqual Nothing
    it "should find a different single post after expiring another" $
      do let key1 = (PostByPermalinkKey "2000" "1" "the-article")
             key2 = (PostByPermalinkKey "2001" "2" "another-article")
         eval (wpCacheSet' wordpress key1 (enc article1))
         eval (wpCacheSet' wordpress key2 (enc article2))
         eval (wpExpirePost' wordpress (PostByPermalinkKey "2000" "1" "the-article"))
         eval (wpCacheGet' wordpress key2) >>= shouldEqual (Just (enc article2))
    it "should be able to cache and retrieve post" $
      do let key = (PostKey 200)
         eval (wpCacheSet' wordpress key (enc article1))
         eval (wpCacheGet' wordpress key) >>= shouldEqual (Just (enc article1))-}

  describe "generate queries from <wpPosts>" $ do
    shouldQueryTo
      "<wpPosts></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=20"]
    shouldQueryTo
      "<wpPosts limit=2></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=20"]
    shouldQueryTo
      "<wpPosts offset=1 limit=1></wpPosts>"
      ["/posts?filter[offset]=1&filter[posts_per_page]=20"]
    shouldQueryTo
      "<wpPosts offset=0 limit=1></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=20"]
    shouldQueryTo
      "<wpPosts limit=10 page=1></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=20"]
    shouldQueryTo
      "<wpPosts limit=10 page=2></wpPosts>"
      ["/posts?filter[offset]=20&filter[posts_per_page]=20"]
    shouldQueryTo
      "<wpPosts num=2></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=2"]
    shouldQueryTo
      "<wpPosts num=2 page=2 limit=1></wpPosts>"
      ["/posts?filter[offset]=2&filter[posts_per_page]=2"]
    shouldQueryTo
      "<wpPosts num=1 page=3></wpPosts>"
      ["/posts?filter[offset]=2&filter[posts_per_page]=1"]
    shouldQueryTo
      "<wpPosts tags=\"+home-featured\" limit=10></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=20&filter[tag__in]=177"]
    shouldQueryTo
      "<wpPosts tags=\"-home-featured\" limit=1></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=20&filter[tag__not_in]=177"]
    shouldQueryTo
      "<wpPosts tags=\"+home-featured,-featured-global\" limit=1><wpTitle/></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=20&filter[tag__in]=177&filter[tag__not_in]=160"]
    shouldQueryTo
      "<wpPosts tags=\"+home-featured,+featured-global\" limit=1><wpTitle/></wpPosts>"
      ["/posts?filter[offset]=0&filter[posts_per_page]=20&filter[tag__in]=160&filter[tag__in]=177"]
    shouldQueryTo
      "<wpPosts categories=\"bookmarx\" limit=10><wpTitle/></wpPosts>"
      ["/posts?filter[category__in]=159&filter[offset]=0&filter[posts_per_page]=20"]
    shouldQueryTo
      "<wpPosts categories=\"-bookmarx\" limit=10><wpTitle/></wpPosts>"
      ["/posts?filter[category__not_in]=159&filter[offset]=0&filter[posts_per_page]=20"]
    shouldQueryTo
      "<wp><div><wpPosts categories=\"bookmarx\" limit=10><wpTitle/></wpPosts></div></wp>"
      (replicate 2 "/posts?filter[category__in]=159&filter[offset]=0&filter[posts_per_page]=20")


  describe "live tests (which require running wordpress server)" $ do
    ctxt <- runIO initializer3
    runIO $ clearRedisCache ctxt
    do it "should have title on page" $
         ("single", "/2014/10/a-first-post", ctxt)
         `shouldRenderAtUrlContaining` "A first post"
       it "should be able to limit" $
         ("many", ctxt) `shouldNotRenderTheSameAs` "many1"
       it "should be able to offset" $
         ("many2", ctxt) `shouldNotRenderTheSameAs` "many3"
       it "should be able to get page 2" $
         ("page1", ctxt) `shouldNotRenderTheSameAs` "page2"
       it "should be able to use page, num, and limit" $ do
         ("num1", ctxt) `shouldNotRenderTheSameAs` "num2"
         ("num2", ctxt) `shouldRenderTheSameAs` "num3"
       it "should be able to restrict based on tags" $
         ("tag1", ctxt) `shouldNotRenderTheSameAs` "tag2"
       it "should be able to say +tag instead of tag" $
         ("tag1", ctxt) `shouldRenderTheSameAs` "tag3"
       it "should be able to say -tag to NOT match a tag" $
         ("/tag4", ctxt) `shouldNotRenderTheSameAs` "tag5"
       it "should be able to have multiple tag queries" $
         ("/tag6", ctxt) `shouldNotRenderTheSameAs` "tag7"
       it "should be able to get nested attribute author name" $
         ("author-date", "/2014/10/a-second-post/", ctxt)
           `shouldRenderAtUrlContaining` "Ira Rubel"
       it "should be able to get customly parsed attribute date" $
         ("author-date", "/2014/10/a-second-post/", ctxt)
           `shouldRenderAtUrlContaining` "2014/10"
       it "should be able to restrict based on category" $
         ("cat1", ctxt) `shouldNotRenderTheSameAs` "cat2"
       it "should be able to make negative category queries" $
         ("cat1", ctxt) `shouldNotRenderTheSameAs` "cat3"

shouldRenderWpPostsTo :: (Text, Object) -> Text -> Spec
shouldRenderWpPostsTo (tags, jsonResponse) match =
  it (T.unpack $ tags <> " should render to " <> match) $ do
    rendered <- liftIO $ evalStateT (wpPostsHelper [] ([], parse (TL.fromStrict tags)) mempty [jsonResponse]) ()
    rendered `shouldBe` match

shouldRenderContaining :: ([Text], Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRenderContaining (pth, t, s, l) excerpt = do
  rendered <- evalStateT (runTemplate (parse (TL.fromStrict t)) pth s l) ()
  (excerpt `T.isInfixOf` rendered) `shouldBe` True

shouldRenderAtUrlContaining :: (Text, Text, Ctxt) -> Text -> Expectation
shouldRenderAtUrlContaining (template, url, ctxt) match = do
    let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 url }
    let ctxt' = setRequest ctxt
                 $ (\(x,y) -> (requestWithUrl, y)) defaultFnRequest
    let s = _wpsubs ctxt
    rendered <- renderLarceny2 ctxt' template
    let rendered' = fromMaybe "" rendered
    (match `T.isInfixOf` rendered') `shouldBe` True
    
wpCacheGet' wpLens wpKey = do
  WordpressInt{..} <- cacheInternals <$> use wpLens
  liftIO $ wpCacheGet wpKey

wpCacheSet' wpLens wpKey o = do
  WordpressInt{..} <- cacheInternals <$> use wpLens
  liftIO $ wpCacheSet wpKey o

wpExpireAggregates' wpLens = do
  Wordpress{..} <- use wpLens
  liftIO wpExpireAggregates

wpExpirePost' wpLens k = do
  Wordpress{..} <- use wpLens
  liftIO $ wpExpirePost k
