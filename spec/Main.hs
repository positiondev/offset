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
import           Web.Larceny

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

article1 :: Value
article1 = object [ "id" .= (1 :: Int)
                  , "title" .= object ["rendered" .= ("Foo bar" :: Text)]
                  , "excerpt" .= object ["rendered" .= ("summary" :: Text)]
                  ]

article2 = object [ "id" .= (2 :: Int)
                  , "title" .= object ["rendered" .= ("The post" :: Text)]
                  , "excerpt" .= object ["rendered" .= ("summary" :: Text)]
                  ]

customFields = [N "featured_image" [N "attachment_meta" [N "sizes" [N "mag-featured" [F "width"
                                                                                     ,F "height"
                                                                                     ,F "url"]
                                                                   ,N "single-featured" [F "width"
                                                                                        ,F "height"
                                                                                        ,F "url"]]]]]

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

renderLarceny :: Ctxt ->
                 Text ->
                 IO (Maybe Text)
renderLarceny ctxt name =
  do let tpl = M.lookup [name] tplLibrary
     case tpl of
       Just t -> do
         rendered <- evalStateT (runTemplate t [name] (ctxt ^. wpsubs) tplLibrary) ctxt
         return $ Just rendered
       _ -> return Nothing

fauxRequester :: Maybe (MVar [Text]) -> Text -> [(Text, Text)] -> IO Text
fauxRequester _  "/tags/" [] =
  return $ enc [object [ "id" .= (177 :: Int)
                       , "slug" .= ("home-featured" :: Text)
                       ]
               ,object [ "id" .= (160 :: Int)
                       , "slug" .= ("featured-global" :: Text)
                       ]
               ]
fauxRequester _ "/categories/" [] =
          return $ enc [object [ "id" .= (159 :: Int)
                               , "slug" .= ("bookmarx" :: Text)
                               , "meta" .= object ["links" .= object ["self" .= ("/159" :: Text)]]
                               ]
                       ]
fauxRequester mRecord rqPath rqParams = do
  case mRecord of
    Just record -> modifyMVar_ record $ return . (<> [mkUrlUnescape rqPath rqParams])
    Nothing -> return ()
  return $ enc [article1]
  where mkUrlUnescape url params =
             url <> "?"
          <> T.intercalate "&" (map (\(k, v) -> k <> "=" <> v) params)

initializer requester cache endpoint =
  do rconn <- R.connect R.defaultConnectInfo
     let wpconf = def { wpConfEndpoint = endpoint
                      , wpConfLogger = Nothing
                      , wpConfRequester = requester
                      , wpConfExtraFields = []
                      , wpConfCacheBehavior = cache
                   }
     let getUri :: StateT Ctxt IO Text
         getUri = do ctxt <- S.get
                     return (T.decodeUtf8 . rawPathInfo . fst . getRequest $ ctxt)
     (wp,wpSubs) <- initWordpress wpconf rconn getUri wordpress
     return (Ctxt defaultFnRequest rconn wp wpSubs mempty)

initFauxRequestNoCache =
  initializer (Right $ Requester (fauxRequester Nothing)) NoCache ""

initNoRequestWithCache =
  initializer (Right $ Requester (\_ _ -> return "" )) (CacheSeconds 60) ""

----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

main :: IO ()
main = hspec $ do
  Misc.tests
  larcenyFillTests
  queryTests
  liveTests

type TemplateName = Text
type TemplateText = Text
type TemplateUrl = Text

clearRedisCache :: Ctxt -> IO Bool
clearRedisCache ctxt = R.runRedis (_redis ctxt) (rdelstar "wordpress:*")

unobj :: Value -> Object
unobj (Object x) = x
unobj _ = error "Not an object"

toTpl tpl = parse (TL.fromStrict tpl)

ignoreWhitespace = T.replace " " ""

shouldRender :: TemplateText
             -> Text
             -> Expectation
shouldRender t output = do
  ctxt <- initFauxRequestNoCache
  let s = _wpsubs ctxt
  rendered <- evalStateT (runTemplate (toTpl t) [] s mempty) ctxt
  ignoreWhitespace rendered `shouldBe` ignoreWhitespace output

larcenyFillTests = do
  describe "<wpPosts>" $
    it "should show the title, id, and excerpt" $ do
      "<wpPosts><wpTitle/></wpPosts>" `shouldRender` "Foo bar"
      "<wpPosts><wpId/></wpPosts>" `shouldRender` "1.0"
      "<wpPosts><wpExcerpt/></wpPosts>" `shouldRender` "summary"

  describe "<wpNoPostDuplicates/>" $ do
    it "should not duplicate any posts after call to wpNoPostDuplicates" $
      "<wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts>" `shouldRender` "Foo bar"
    it "should ignore duplicates if they were rendered before wpNoPostDuplicates" $ do
      "<wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts>" `shouldRender` "Foo barFoo bar"
      "<wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts>" `shouldRender` "Foo barFoo bar"
    it "should have no effect if it's at the end of the template" $
      "<wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/>" `shouldRender` "Foo barFoo bar"

  describe "<wpPostByPermalink>" $ do
    it "should query at a certain url" $ do
      record <- liftIO $ newMVar []
      let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 "/2009/10/the-post/" }
      ctxt <- liftIO $ initializer
                        (Right $ Requester $ fauxRequester (Just record))
                        (CacheSeconds 10)
                        ""
      let ctxt' = setRequest ctxt
            $ (\(x,y) -> (requestWithUrl, y)) defaultFnRequest
      let s = _wpsubs ctxt'
      let tpl = toTpl "<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp"
      evalStateT (runTemplate tpl [] s mempty) ctxt'
      liftIO (tryTakeMVar record) `shouldReturn` Just ["/posts?slug=the-post"]
    it "should render stuff" $ do
      ctxt <- initFauxRequestNoCache
      let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 "/2009/10/the-post/"}
      let ctxt' = setRequest ctxt
                 $ (\(x,y) -> (requestWithUrl, y)) defaultFnRequest
      let s = view wpsubs ctxt'
      let tpl = toTpl "<wp><wpNoPostDuplicates/><wpPostByPermalink><wpTitle/></wpPostByPermalink><wpPosts limit=1><wpTitle/></wpPosts></wp>"
      rendered <- evalStateT (runTemplate tpl [] s mempty) ctxt'
      rendered `shouldBe` "Foo bar"

-- Caching tests

wpCacheGet' wordpress wpKey = do
  let WordpressInt{..} = cacheInternals wordpress
  liftIO $ wpCacheGet wpKey

wpCacheSet'' wordpress wpKey o = do
  let WordpressInt{..} = cacheInternals wordpress
  wpCacheSet wpKey o

wpCacheSet' wordpress wpKey o = do
  let WordpressInt{..} = cacheInternals wordpress
  liftIO $ wpCacheSet wpKey o

wpExpireAggregates' wordpress = do
  let Wordpress{..} = wordpress
  liftIO wpExpireAggregates

wpExpirePost' wordpress k = do
  let Wordpress{..} = wordpress
  liftIO $ wpExpirePost k

cacheTests = do
  describe "should grab post from cache if it's there" $ do
      it "should do a thing" $ do
        let (Object a2) = article2
        ctxt <- liftIO initNoRequestWithCache
        wpCacheSet' (view wordpress ctxt) (PostByPermalinkKey "2001" "10" "the-post")
                                        (enc a2)
        ("single", "/2001/10/the-post/", ctxt) `shouldRenderAtUrlContaining` "The post"

  describe "caching" $ do
    it "should find nothing for a non-existent post" $ do
      ctxt <- initNoRequestWithCache
      p <- wpCacheGet' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
      p `shouldBe` Nothing
    it "should find something if there is a post in cache" $ do
      ctxt <- initNoRequestWithCache
      wpCacheSet' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
                                  (enc article1)
      p <- wpCacheGet' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
      p `shouldBe` (Just $ enc article1)
    it "should not find single post after expire handler is called" $ do
         ctxt <- initNoRequestWithCache
         wpCacheSet' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
                                     (enc article1)
         wpExpirePost' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
         wpCacheGet' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
           >>= shouldBe Nothing
    it "should find post aggregates in cache" $
      do ctxt <- initNoRequestWithCache
         let key = PostsKey (Set.fromList [NumFilter 20, OffsetFilter 0])
         wpCacheSet' (view wordpress ctxt) key ("[" <> enc article1 <> "]")
         wpCacheGet' (view wordpress ctxt) key
           >>= shouldBe (Just $ "[" <> enc article1 <> "]")
    it "should not find post aggregates after expire handler is called" $
      do ctxt <- initNoRequestWithCache
         let key = PostsKey (Set.fromList [NumFilter 20, OffsetFilter 0])
         wpCacheSet' (view wordpress ctxt) key ("[" <> enc article1 <> "]")
         wpExpirePost' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
         wpCacheGet' (view wordpress ctxt) key
           >>= shouldBe Nothing
    it "should find single post after expiring aggregates" $
      do ctxt <- initNoRequestWithCache
         wpCacheSet' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
                          (enc article1)
         wpExpireAggregates' (view wordpress ctxt)
         wpCacheGet' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
           >>= shouldNotBe Nothing
    it "should find a different single post after expiring another" $
      do ctxt <- initNoRequestWithCache
         let key1 = (PostByPermalinkKey "2000" "1" "the-article")
             key2 = (PostByPermalinkKey "2001" "2" "another-article")
         wpCacheSet' (view wordpress ctxt) key1 (enc article1)
         wpCacheSet' (view wordpress ctxt) key2 (enc article2)
         wpExpirePost' (view wordpress ctxt) (PostByPermalinkKey "2000" "1" "the-article")
         wpCacheGet' (view wordpress ctxt) key2 >>= shouldBe (Just (enc article2))
    it "should be able to cache and retrieve post" $
      do ctxt <- initNoRequestWithCache
         let key = (PostKey 200)
         wpCacheSet' (view wordpress ctxt) key (enc article1)
         wpCacheGet' (view wordpress ctxt) key >>= shouldBe (Just (enc article1))

shouldQueryTo :: TemplateText -> [Text] -> Spec
shouldQueryTo hQuery wpQuery =
  it ("should query from " <> T.unpack hQuery) $ do
      record <- liftIO $ newMVar []
      ctxt <- liftIO $ initializer
                         (Right $ Requester $ fauxRequester (Just record))
                         NoCache
                         ""
      let s = _wpsubs ctxt
      evalStateT (runTemplate (toTpl hQuery) [] s mempty) ctxt
      x <- liftIO $ tryTakeMVar record
      x `shouldBe` Just wpQuery

queryTests =
  describe "generate queries from <wpPosts>" $ do
      "<wpPosts></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=20"]
      "<wpPosts limit=2></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=20"]
      "<wpPosts offset=1 limit=1></wpPosts>" `shouldQueryTo`
        ["/posts?offset=1&per_page=20"]
      "<wpPosts offset=0 limit=1></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=20"]
      "<wpPosts limit=10 page=1></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=20"]
      "<wpPosts limit=10 page=2></wpPosts>" `shouldQueryTo`
        ["/posts?offset=20&per_page=20"]
      "<wpPosts num=2></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=2"]
      "<wpPosts num=2 page=2 limit=1></wpPosts>" `shouldQueryTo`
        ["/posts?offset=2&per_page=2"]
      "<wpPosts num=1 page=3></wpPosts>" `shouldQueryTo`
        ["/posts?offset=2&per_page=1"]
      "<wpPosts tags=\"+home-featured\" limit=10></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=20&tags[]=177"]
      "<wpPosts tags=\"-home-featured\" limit=1></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=20&tags_exclude[]=177"]
      "<wpPosts tags=\"+home-featured,-featured-global\" limit=1><wpTitle/></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=20&tags[]=177&tags_exclude[]=160"]
      "<wpPosts tags=\"+home-featured,+featured-global\" limit=1><wpTitle/></wpPosts>" `shouldQueryTo`
        ["/posts?offset=0&per_page=20&tags[]=160&tags[]=177"]
      "<wpPosts categories=\"bookmarx\" limit=10><wpTitle/></wpPosts>" `shouldQueryTo`
        ["/posts?categories[]=159&offset=0&per_page=20"]
      "<wpPosts categories=\"-bookmarx\" limit=10><wpTitle/></wpPosts>" `shouldQueryTo`
        ["/posts?categories_exclude[]=159&offset=0&per_page=20"]
      "<wp><div><wpPosts categories=\"bookmarx\" limit=10><wpTitle/></wpPosts></div></wp>" `shouldQueryTo`
        replicate 2 "/posts?categories[]=159&offset=0&per_page=20"

rendersDifferentlyFrom :: (TemplateName, Ctxt)
                       -> TemplateName
                       -> Expectation
rendersDifferentlyFrom (name1, ctxt) name2 = do
  rendered1 <- renderLarceny ctxt name1
  rendered2 <- renderLarceny ctxt name2
  ignoreWhitespace <$> rendered1 `shouldNotBe` ignoreWhitespace <$> rendered2

rendersSameAs :: (TemplateName, Ctxt)
              -> TemplateName
              -> Expectation
rendersSameAs (name1, ctxt) name2 = do
  rendered1 <- renderLarceny ctxt name1
  rendered2 <- renderLarceny ctxt name2
  ignoreWhitespace <$> rendered1 `shouldBe` ignoreWhitespace <$> rendered2

shouldRenderAtUrlContaining :: (TemplateName, TemplateUrl, Ctxt) -> Text -> Expectation
shouldRenderAtUrlContaining (template, url, ctxt) match = do
    let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 url }
    let ctxt' = setRequest ctxt
                 $ (\(x,y) -> (requestWithUrl, y)) defaultFnRequest
    let s = _wpsubs ctxt
    rendered <- renderLarceny ctxt' template
    let rendered' = fromMaybe "" rendered
    (match `T.isInfixOf` rendered') `shouldBe` True

liveTests :: Spec
liveTests =
  describe "live tests (which require running wordpress server)" $ do
    ctxt <- runIO $ initializer (Left ("offset", "111")) NoCache "http://localhost:5555/wp-json/wp/v2"
    runIO $ clearRedisCache ctxt
    do it "should have title on page" $
         ("single", "/2014/10/a-first-post", ctxt)
         `shouldRenderAtUrlContaining` "A first post"
       it "should be able to limit" $
         ("many", ctxt) `rendersDifferentlyFrom` "many1"
       it "should be able to offset" $
         ("many2", ctxt) `rendersDifferentlyFrom` "many3"
       it "should be able to get page 2" $
         ("page1", ctxt) `rendersDifferentlyFrom` "page2"
       it "should be able to use page, num, and limit" $ do
         ("num1", ctxt) `rendersDifferentlyFrom` "num2"
         ("num2", ctxt) `rendersSameAs` "num3"
       it "should be able to restrict based on tags" $
         ("tag1", ctxt) `rendersDifferentlyFrom` "tag2"
       it "should be able to say +tag instead of tag" $
         ("tag1", ctxt) `rendersSameAs` "tag3"
       it "should be able to say -tag to NOT match a tag" $
         ("/tag4", ctxt) `rendersDifferentlyFrom` "tag5"
       it "should be able to have multiple tag queries" $
         ("/tag6", ctxt) `rendersDifferentlyFrom` "tag7"
       it "should be able to get customly parsed attribute date" $
         ("author-date", "/2014/10/a-second-post/", ctxt)
           `shouldRenderAtUrlContaining` "2014/10"
       it "should be able to restrict based on category" $
         ("cat1", ctxt) `rendersDifferentlyFrom` "cat2"
       it "should be able to make negative category queries" $
         ("cat1", ctxt) `rendersDifferentlyFrom` "cat3"
