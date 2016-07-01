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
import           Control.Monad.State      (StateT, evalStateT, get)
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
import           Network.Wai              (Application, Response, pathInfo,
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
                 , _subs      :: Substitutions Ctxt
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


larcenyServe :: Ctxt ->
                IO (Maybe Response)
larcenyServe ctxt =
  let p = pathInfo . fst $ getRequest ctxt in
  mplus <$> renderLarceny ctxt (T.intercalate "/" p)
        <*> renderLarceny ctxt (T.intercalate "/" (p <> ["index"]))

renderLarceny :: Ctxt ->
                 Text ->
                 IO (Maybe Response)
renderLarceny ctxt name =
  do let tpl = (ctxt ^. lib) M.! [name]
     t <- evalStateT (runTemplate tpl [name] (ctxt ^. subs) (ctxt ^. lib)) ctxt
     okHtml t


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
fauxRequester response rqPath rqParams = do
  --modifyMVar_ record $ (return . (<> [mkUrlUnescape url params]))
  return response
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
         getUri = do ctxt <- get
                     return (T.decodeUtf8 . rawPathInfo . fst . getRequest $ ctxt)
     (wp,wpSubs) <- initWordpress wpconf rconn getUri wordpress
     let allSubs = wpSubs
     let tpls = mempty --Larceny.loadTemplates "templates"
     return (Ctxt defaultFnRequest rconn wp allSubs tpls)

app :: (Ctxt -> IO Response) -> IO Application
app site  =
  do ctxt <- initializer "wow!"
     return (toWAI ctxt site)


{--
localApp :: [(Text, Text)] -> SnapletInit App App
localApp tmpls =
  makeSnaplet "app" "App." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit ""
    addConfig h $ set scTemplateLocations (return templates) mempty
    r <- nestSnaplet "" redis redisDBInitConf
    (w,splices) <- liftIO $ initWordpress config (view (snapletValue . redisConnection) r) (T.decodeUtf8 . rqURI <$> getRequest) wordpress
    addConfig h $ set scInterpretedSplices splices mempty
    return $ App h r w
  where mkTmpl (name, html) = let (Right doc) = X.parseHTML "" (T.encodeUtf8 html)
                               in ([T.encodeUtf8 name], DocumentFile doc Nothing)
        templates = return $ M.fromList (map mkTmpl tmpls)
        config = (def { wpConfEndpoint = "http://localhost:5555/wp-json"
                      , wpConfCacheBehavior = NoCache
                      , wpConfExtraFields = jacobinFields
                      , wpConfLogger = Nothing
                      })

renderingApp :: [(Text, Text)] -> Text -> SnapletInit App App
renderingApp tmpls response = makeSnaplet "app" "App." Nothing $ do
  h <- nestSnaplet "" heist $ heistInit ""
  addConfig h $ set scTemplateLocations (return templates) mempty
  r <- nestSnaplet "" redis redisDBInitConf
  (w,splices) <- liftIO $ initWordpress config (view (snapletValue . redisConnection) r) (T.decodeUtf8 . rqURI <$> getRequest) wordpress
  addConfig h $ set scInterpretedSplices splices mempty
  return $ App h r w
  where mkTmpl (name, html) = let (Right doc) = X.parseHTML "" (T.encodeUtf8 html)
                               in ([T.encodeUtf8 name], DocumentFile doc Nothing)
        templates = return $ M.fromList (map mkTmpl tmpls)
        config = (def { wpConfEndpoint = ""
                      , wpConfRequester = Right $ Requester (\_ _ -> return response)
                      , wpConfCacheBehavior = NoCache
                      , wpConfExtraFields = jacobinFields})

queryingApp :: [(Text, Text)] -> MVar [Text] -> SnapletInit App App
queryingApp tmpls record = makeSnaplet "app" "An snaplet example application." Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  addConfig h $ set scTemplateLocations (return templates) mempty
  r <- nestSnaplet "" redis redisDBInitConf
  (w,splices) <- liftIO $ initWordpress config (view (snapletValue . redisConnection) r) (T.decodeUtf8 . rqURI <$> getRequest) wordpress
  addConfig h $ set scInterpretedSplices splices mempty
  return $ App h r w
  where mkTmpl (name, html) = let (Right doc) = X.parseHTML "" (T.encodeUtf8 html)
                               in ([T.encodeUtf8 name], DocumentFile doc Nothing)
        templates = return $ M.fromList (map mkTmpl tmpls)
        config = (def { wpConfEndpoint = ""
                      , wpConfRequester = Right $ Requester recordingRequester
                      , wpConfCacheBehavior = NoCache})
        recordingRequester "/taxonomies/post_tag/terms" [] =
          return $ enc $ [object [ "ID" .= (177 :: Int)
                                 , "slug" .= ("home-featured" :: Text)
                                 ]
                         ,object [ "ID" .= (160 :: Int)
                                 , "slug" .= ("featured-global" :: Text)
                                 ]
                         ]
        recordingRequester "/taxonomies/category/terms" [] =
          return $ enc $ [object [ "ID" .= (159 :: Int)
                                 , "slug" .= ("bookmarx" :: Text)
                                 , "meta" .= object ["links" .= object ["self" .= ("/159" :: Text)]]
                                 ]
                         ]
        recordingRequester url params = do
          modifyMVar_ record $ (return . (<> [mkUrlUnescape url params]))
          return ""
        mkUrlUnescape url params = (url <> "?" <> (T.intercalate "&" $ map (\(k, v) -> k <> "=" <> v) params))

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
{-
clearRedisCache :: IO Bool
clearRedisCache = runRedis $ rdelstar "wordpress:*"
-}

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
  let s = _subs ctxt
  rendered <- evalStateT (runTemplate (toTpl t) [] s mempty) ctxt
  rendered `shouldBeIgnoreWhitespace` output

toTpl tpl = parse (TL.fromStrict tpl)

shouldBeIgnoreWhitespace a b =
  T.replace " " "" a `shouldBe` T.replace " " "" b
{-
shouldQueryTo :: Text -> [Text] -> Spec
shouldQueryTo hQuery wpQuery = do
  record <- runIO $ newMVar []
  ctxt <- liftIO $ initializer [("x", hQuery)] record
  it ("query from " <> T.unpack hQuery) $ do
      let s = _subs ctxt
      evalStateT (runTemplate (toTpl t) [] s mempty) ctxt
      x <- liftIO $ tryTakeMVar record
      x `shouldBe` Just wpQuery-}

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
        let subs = _subs ctxt
        rendered <- evalStateT (runTemplate (toTpl tpl) [] subs mempty) ctxt
        rendered `shouldBeIgnoreWhitespace` "Foo bar"
    it "should ignore duplicates if they were rendered before wpNoPostDuplicates" $ do
      "<wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts>"
        `shouldRender` "Foo barFoo bar"
      "<wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts>"
        `shouldRender` "Foo barFoo bar"
    it "should have no effect if it's at the end of the template" $
      "<wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/>"
        `shouldRender` "Foo barFoo bar"

      {-
  describe "<wpPostByPermalink>" $ do
    shouldQueryAtUrl "/2009/10/the-post/"
                     "<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp>"
                     ["/posts?filter[year]=2009&filter[monthnum]=10&filter[name]=the-post"]
    shouldRenderAtUrl "/2009/10/the-post/" ("<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp>", enc [article1]) "Foo bar"
    shouldRenderAtUrl "/2009/10/the-post/" ("<wp><wpNoPostDuplicates/><wpPostByPermalink><wpTitle/></wpPostByPermalink><wpPosts limit=1><wpTitle/></wpPosts></wp>", enc [article1]) "Foo bar" -}

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
{-
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
      (replicate 2 "/posts?filter[category__in]=159&filter[offset]=0&filter[posts_per_page]=20") -}

{-
  describe "live tests (which require running wordpress server)" $
    snap (route
            [("/2014/10/a-first-post", render "single")
            ,("/many", render "many")
            ,("/many2", render "many2")
            ,("/many3", render "many3")
            ,("/page1", render "page1")
            ,("/page2", render "page2")
            ,("/num1", render "num1")
            ,("/num2", render "num2")
            ,("/num3", render "num3")
            ,("/tag1", render "tag1")
            ,("/tag2", render "tag2")
            ,("/tag3", render "tag3")
            ,("/tag4", render "tag4")
            ,("/tag5", render "tag5")
            ,("/tag6", render "tag6")
            ,("/tag7", render "tag7")
            ,("/cat1", render "cat1")
            ,("/cat2", render "cat2")
            ,("/cat3", render "cat3")
            ,("/fields", render "fields")
            ,("/2014/10/a-second-post/", render "author-date")
        ])
     (localApp [("single", "<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp>")
               ,("many", "<wpPosts limit=2><wpTitle/></wpPosts>")
               ,("many1", "<wpPosts><wpTitle/></wpPosts>")
               ,("many2", "<wpPosts offset=1 limit=1><wpTitle/></wpPosts>")
               ,("many3", "<wpPosts offset=0 limit=1><wpTitle/></wpPosts>")
               ,("page1", "<wpPosts limit=10 page=1><wpTitle/></wpPosts>")
               ,("page2", "<wpPosts limit=10 page=2><wpTitle/></wpPosts>")
               ,("num1", "<wpPosts num=2><wpTitle/></wpPosts>")
               ,("num2", "<wpPosts num=2 page=2 limit=1><wpTitle/></wpPosts>")
               ,("num3", "<wpPosts num=1 page=3><wpTitle/></wpPosts>")
               ,("tag1", "<wpPosts tags=\"tag1\" limit=10><wpTitle/></wpPosts>")
               ,("tag2", "<wpPosts limit=10><wpTitle/></wpPosts>")
               ,("tag3", "<wpPosts tags=\"+tag1\" limit=10><wpTitle/></wpPosts>")
               ,("tag4", "<wpPosts tags=\"-tag1\" limit=1><wpTitle/></wpPosts>")
               ,("tag5", "<wpPosts tags=\"+tag1\" limit=1><wpTitle/></wpPosts>")
               ,("tag6", "<wpPosts tags=\"+tag1,-tag2\" limit=1><wpTitle/></wpPosts>")
               ,("tag7", "<wpPosts tags=\"+tag1,+tag2\" limit=1><wpTitle/></wpPosts>")
               ,("cat1", "<wpPosts categories=\"cat1\" limit=10><wpTitle/></wpPosts>")
               ,("cat2", "<wpPosts limit=10><wpTitle/></wpPosts>")
               ,("cat3", "<wpPosts categories=\"-cat1\" limit=10><wpTitle/></wpPosts>")
               ,("author-date", "<wp><wpPostByPermalink><wpAuthor><wpName/></wpAuthor><wpDate><wpYear/>/<wpMonth/></wpDate></wpPostByPermalink></wp>")
               ,("fields", "<wp><wpPosts limit=1 categories=\"-cat1\"><wpFeaturedImage><wpAttachmentMeta><wpSizes><wpThumbnail><wpUrl/></wpThumbnail></wpSizes></wpAttachmentMeta></wpFeaturedImage></wpPosts></wp>")
               ]
      ) $
      do it "should have title on page" $
           do r <- get "/2014/10/a-first-post"
              shouldHaveText "A first post" r
         it "should be able to limit" $
           do p1 <- get "/many"
              get "/many1" >>= shouldNotEqual p1
         it "should be able to offset" $
           do res <- get "/many2"
              res2 <- get "/many3"
              res `shouldNotEqual` res2
         it "should be able to get page 2" $
           do p1 <- get "/page1"
              get "/page2" >>= shouldNotEqual p1
         it "should be able to use page, num, and limit" $
           do p1 <- get "/num1"
              p2 <- get "/num2"
              p3 <- get "/num3"
              p1 `shouldNotEqual` p2
              p2 `shouldEqual` p3
         it "should be able to restrict based on tags" $
           do p1 <- get "/tag1"
              get "/tag2" >>= shouldNotEqual p1
         it "should be able to say +tag instead of tag" $
           do p1 <- get "/tag1"
              get "/tag3" >>= shouldEqual p1
         it "should be able to say -tag to NOT match a tag" $
           do p1 <- get "/tag4"
              get "/tag5" >>= shouldNotEqual p1
         it "should be able to have multiple tag queries" $
           do p1 <- get "/tag6"
              get "/tag7" >>= shouldNotEqual p1
         it "should be able to get nested attribute author name" $
           get "/2014/10/a-second-post/" >>= shouldHaveText "Ira Rubel"
         it "should be able to get customly parsed attribute date" $
           get "/2014/10/a-second-post/" >>= shouldHaveText "2014/10"
         it "should be able to restrict based on category" $
           do c1 <- get "/cat1"
              c2 <- get "/cat2"
              c1 `shouldNotEqual` c2
         it "should be able to make negative category queries" $
           do c1 <- get "/cat1"
              c2 <- get "/cat3"
              c1 `shouldNotEqual` c2
-}


shouldRenderWpPostsTo :: (Text, Object) -> Text -> Spec
shouldRenderWpPostsTo (tags, jsonResponse) match =
  it (T.unpack $ tags <> " should render to " <> match) $ do
    rendered <- liftIO $ evalStateT (wpPostsHelper [] ([], parse (TL.fromStrict tags)) mempty [jsonResponse]) ()
    rendered `shouldBe` match

shouldRenderDef :: (Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRenderDef (t', s, l) output = do
    rendered <- evalStateT (runTemplate (parse (TL.fromStrict t')) ["default"] s l) ()
    T.replace " " "" rendered `shouldBe`
      T.replace " " "" output

shouldRenderContaining :: ([Text], Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRenderContaining (pth, t, s, l) excerpt = do
  rendered <- evalStateT (runTemplate (parse (TL.fromStrict t)) pth s l) ()
  (excerpt `T.isInfixOf` rendered) `shouldBe` True

{-
shouldRenderAtUrl :: Text -> (Text, Text) -> Text -> Spec
shouldRenderAtUrl url (tags, response) match =
  snap (route [(T.encodeUtf8 url, render "test")]) (renderingApp [("test", tags)] response) $
    it (T.unpack $ tags <> " should render to match " <> match) $
       get url >>= shouldHaveText match


shouldQueryTo :: Text -> [Text] -> Spec
shouldQueryTo hQuery wpQuery = do
  record <- runIO $ newMVar []
  snap (route []) (queryingApp [("x", hQuery)] record) $
    it ("query from " <> T.unpack hQuery) $ do
      eval $ render "x"
      x <- liftIO $ tryTakeMVar record
      x `shouldEqual` Just wpQuery

shouldQueryAtUrl :: Text -> Text -> [Text] -> Spec
shouldQueryAtUrl url tmpl urls = do
  record <- runIO $ newMVar []
  snap (route [(T.encodeUtf8 url,render "x")]) (queryingApp [("x", tmpl)] record) $
    it (T.unpack $ "At " <> url <> ", " <> tmpl <> " should query to " <> T.pack (show urls)) $ do
      get url
      x <- liftIO $ tryTakeMVar record
      x `shouldEqual` Just urls

getWordpress :: Handler b v v
getWordpress = view snapletValue <$> getSnapletState
-}

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
