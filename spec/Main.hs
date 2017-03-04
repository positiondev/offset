{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where
 
import           Prelude                 hiding ((++))

import           Control.Concurrent.MVar
import           Control.Lens            hiding ((.=))
import           Control.Monad           (void)
import           Control.Monad.State     (evalStateT)
import           Control.Monad.Trans     (liftIO)
import           Data.Aeson              hiding (Success)
import           Data.Monoid
import qualified Data.Set                as Set
import qualified Data.Text.Encoding      as T
import qualified Misc
import           Network.Wai             (defaultRequest, rawPathInfo)
import           Test.Hspec
import           Web.Fn
import           Web.Larceny

import           Web.Offset

import           Common

runTests :: IO ()
runTests = hspec $ do
  Misc.tests
  larcenyFillTests
  cacheTests
  queryTests
  liveTests

main :: IO ()
main = runTests

--larcenyFillTests :: SpecM () ()
larcenyFillTests :: Spec
larcenyFillTests = do
  describe "<wpPosts>" $ do
    it "should show the title, id, and excerpt" $ do
      "<wpPosts><wpTitle/></wpPosts>" `shouldRender` "Foo bar"
      "<wpPosts><wpId/></wpPosts>" `shouldRender` "1"
      "<wpPosts><wpExcerpt/></wpPosts>" `shouldRender` "summary"
    it "should allow formating of dates" $
      "<wpPosts> \
      \  <wpDate> \
      \    <wpMonth format=\"%B\"/> <wpDay format=\"%-d\"/>, <wpYear /> \
      \  </wpDate> \
      \</wpPosts>" `shouldRender` "October 20, 2014"
    it "should render customly parsed fields" $
      "<wpPosts><wpDepartments><name /></wpDepartments></wpPosts>"
      `shouldRender` "some department"
  describe "<wpPage>" $
    it "should show the content" $
      "<wpPage name=a-first-page />" `shouldRender` "<b>rendered</b> page content"
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
            $ (\(_,y) -> (requestWithUrl, y)) defaultFnRequest
      let s = _cmssubs ctxt'
      let tpl = toTpl "<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp"
      void $ evalStateT (runTemplate tpl [] s mempty) ctxt'
      liftIO (tryTakeMVar record) `shouldReturn` Just ["/wp/v2/posts?slug=the-post"]
    it "should render stuff" $ do
      ctxt <- initFauxRequestNoCache
      let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 "/2009/10/the-post/"}
      let ctxt' = setRequest ctxt
                 $ (\(_,y) -> (requestWithUrl, y)) defaultFnRequest
      let s = view cmssubs ctxt'
      let tpl = toTpl "<wp><wpNoPostDuplicates/><wpPostByPermalink><wpTitle/></wpPostByPermalink><wpPosts limit=1><wpTitle/></wpPosts></wp>"
      rendered <- evalStateT (runTemplate tpl [] s mempty) ctxt'
      rendered `shouldBe` "Foo bar"

  describe "<wpCustom>" $
    it "should render an HTML comment if JSON field is null" $
      "<cmsCustom endpoint=\"dev/null\"><cmsThisIsNull /></cmsCustom>" `shouldRender` "<!-- JSON field found, but value is null. -->"
  describe "<cmsCustomDate>" $ do
    it "should parse a date field with the format string it's given" $
      "<cmsCustomDate date=\"2013-04-26 10:11:52\" format=\"%Y-%m-%d %H:%M:%S\"> \
      \   <cmsDay />~<cmsMonth />~<cmsYear /> \
      \ </cmsCustomDate>" `shouldRender` "26~04~2013"
    it "should format a date field with the format strings it's given" $
      "<cmsCustomDate date=\"2013-04-26 10:11:52\" format=\"%Y-%m-%d %H:%M:%S\"> \
      \   <cmsMonth format=\"%B\"/> <cmsDay format=\"%-d\"/>, <cmsYear /> \
      \ </cmsCustomDate>" `shouldRender` "April 26, 2013"
    it "should use default WordPress date format if none specified" $
      "<cmsCustomDate date=\"2013-04-26 10:11:52\"> \
      \    <cmsDay />~<cmsMonth />~<cmsYear /> \
      \ </cmsCustomDate>" `shouldRender` "26~04~2013"
    it "should allow formatting the whole date in a single tag" $
      "<cmsCustomDate date=\"2013-04-26 10:11:52\"> \
      \    <cmsFullDate /> \
      \ </cmsCustomDate>" `shouldRender` "04/26/13"

-- Caching tests

cacheTests :: Spec
cacheTests = do
  describe "should grab post from cache if it's there" $
      it "should render the post even w/o json source" $ do
        let (Object a2) = article2
        ctxt <- liftIO initNoRequestWithCache
        cmsCacheSet' (view cms ctxt) (PostByPermalinkKey "2001" "10" "the-post")
                                          (enc [a2])
        ("single", ctxt) `shouldRenderAtUrlContaining` ("/2001/10/the-post/", "The post")

  describe "caching" $ do
    it "should find nothing for a non-existent post" $ do
      ctxt <- initNoRequestWithCache
      p <- cmsCacheGet' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
      p `shouldBe` Nothing
    it "should find something if there is a post in cache" $ do
      ctxt <- initNoRequestWithCache
      void $ cmsCacheSet' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
                                          (enc article1)
      p <- cmsCacheGet' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
      p `shouldBe` (Just $ enc article1)
    it "should not find single post after expire handler is called" $ do
         ctxt <- initNoRequestWithCache
         void $ cmsCacheSet' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
                                                  (enc article1)
         void $ cmsExpirePost' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
         cmsCacheGet' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
           >>= shouldBe Nothing
    it "should find post aggregates in cache" $
      do ctxt <- initNoRequestWithCache
         let key = PostsKey (Set.fromList [NumFilter 20, OffsetFilter 0])
         void $ cmsCacheSet' (view cms ctxt) key ("[" <> enc article1 <> "]")
         void $ cmsCacheGet' (view cms ctxt) key
           >>= shouldBe (Just $ "[" <> enc article1 <> "]")
    it "should not find post aggregates after expire handler is called" $
      do ctxt <- initNoRequestWithCache
         let key = PostsKey (Set.fromList [NumFilter 20, OffsetFilter 0])
         void $ cmsCacheSet' (view cms ctxt) key ("[" <> enc article1 <> "]")
         void $ cmsExpirePost' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
         cmsCacheGet' (view cms ctxt) key
           >>= shouldBe Nothing
    it "should find single post after expiring aggregates" $
      do ctxt <- initNoRequestWithCache
         void $ cmsCacheSet' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
                          (enc article1)
         void $ cmsExpireAggregates' (view cms ctxt)
         cmsCacheGet' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
           >>= shouldNotBe Nothing
    it "should find a different single post after expiring another" $
      do ctxt <- initNoRequestWithCache
         let key1 = PostByPermalinkKey "2000" "1" "the-article"
             key2 = PostByPermalinkKey "2001" "2" "another-article"
         void $ cmsCacheSet' (view cms ctxt) key1 (enc article1)
         void $ cmsCacheSet' (view cms ctxt) key2 (enc article2)
         void $ cmsExpirePost' (view cms ctxt) (PostByPermalinkKey "2000" "1" "the-article")
         cmsCacheGet' (view cms ctxt) key2 >>= shouldBe (Just (enc article2))
    it "should be able to cache and retrieve post" $
      do ctxt <- initNoRequestWithCache
         let key = PostKey 200
         cmsCacheSet' (view cms ctxt) key (enc article1)
         cmsCacheGet' (view cms ctxt) key >>= shouldBe (Just (enc article1))

queryTests :: Spec
queryTests =
  describe "generate queries from <wpPosts>" $ do
      "<wpPosts></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=20"]
      "<wpPosts limit=2></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=20"]
      "<wpPosts offset=1 limit=1></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=1&per_page=20"]
      "<wpPosts offset=0 limit=1></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=20"]
      "<wpPosts limit=10 page=1></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=20"]
      "<wpPosts limit=10 page=2></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=20&per_page=20"]
      "<wpPosts num=2></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=2"]
      "<wpPosts num=2 page=2 limit=1></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=2&per_page=2"]
      "<wpPosts num=1 page=3></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=2&per_page=1"]
      "<wpPosts tags=\"+home-featured\" limit=10></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=20&tags[]=177"]
      "<wpPosts tags=\"-home-featured\" limit=1></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=20&tags_exclude[]=177"]
      "<wpPosts tags=\"+home-featured,-featured-global\" limit=1><wpTitle/></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=20&tags[]=177&tags_exclude[]=160"]
      "<wpPosts tags=\"+home-featured,+featured-global\" limit=1><wpTitle/></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?offset=0&per_page=20&tags[]=160&tags[]=177"]
      "<wpPosts categories=\"bookmarx\" limit=10><wpTitle/></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?categories[]=159&offset=0&per_page=20"]
      "<wpPosts categories=\"-bookmarx\" limit=10><wpTitle/></wpPosts>" `shouldQueryTo`
        ["/wp/v2/posts?categories_exclude[]=159&offset=0&per_page=20"]
      "<wp><div><wpPosts categories=\"bookmarx\" limit=10><wpTitle/></wpPosts></div></wp>" `shouldQueryTo`
        replicate 2 "/wp/v2/posts?categories[]=159&offset=0&per_page=20"
      "<wpPage name=blah />" `shouldQueryTo`
        ["/wp/v2/pages?slug=blah"]

liveTests :: Spec
liveTests =
  describe "live tests (which require running wordpress server)" $ do
    ctxt <- runIO $ initializer (Left ("offset", "111")) NoCache "http://localhost:5555/wp-json"
    void $ runIO $ clearRedisCache ctxt
    do it "should have title on page" $
         ("single", ctxt)
         `shouldRenderAtUrlContaining` ("/2014/10/a-first-post", "A first post")
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
         ("author-date", ctxt) `shouldRenderAtUrlContaining`
           ("/2014/10/a-second-post/", "2014/10")
       it "should be able to restrict based on category" $
         ("cat1", ctxt) `rendersDifferentlyFrom` "cat2"
       it "should be able to make negative category queries" $
         ("cat1", ctxt) `rendersDifferentlyFrom` "cat3"
       it "should be able to render a single page" $
         ("single-page", ctxt) `shouldRenderAtUrlContaining`
           ("/pages?slug=a-first-post", "This is the first page content")
       it "should be able to query custom taxonomies" $ do
         ("department", ctxt) `shouldRenderContaining` "A sports post"
         ("department", ctxt) `shouldNotRenderContaining` "A first post"
       it "should be able to query custom endpoints" $ do
         ("custom-endpoint-object", ctxt) `shouldRenderContaining` "categories"
         ("custom-endpoint-object", ctxt) `shouldNotRenderContaining` "departments"
       it "should be able to query custom endpoints" $ do
         ("custom-endpoint-array", ctxt) `shouldRenderContaining` "2014-10-01"
         ("custom-endpoint-array", ctxt) `shouldRenderContaining` "2014-10-02"
         ("custom-endpoint-array", ctxt) `shouldRenderContaining` "2014-10-15"
       it "should be able to reference fields from the custom endpoint in another custom endpoint query" $
         ("custom-endpoint-array", ctxt) `rendersSameAs` "custom-endpoint-enter-the-matrix"
