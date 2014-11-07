{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Prelude                     hiding ((++))

import           Blaze.ByteString.Builder
import           Control.Lens                hiding ((.=))
import           Control.Monad               (join)
import           Control.Monad.Trans         (liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson                  hiding (Success)
import           Data.Default
import qualified Data.HashMap.Strict         as M
import           Data.List                   (intersect)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TL
import qualified Database.Redis              as R
import           Heist
import           Heist.Compiled
import           Snap                        hiding (get)
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.RedisDB
import           Snap.Snaplet.Wordpress
import           Test.Hspec
import           Test.Hspec.Core             (Result (..))
import           Test.Hspec.Snap
import qualified Text.XmlHtml                as X

(++) :: Monoid a => a -> a -> a
(++) = mappend

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------

data App = App { _heist     :: Snaplet (Heist App)
               , _redis     :: Snaplet RedisDB
               , _wordpress :: Snaplet (Wordpress App) }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

enc a = T.concat ["[", TL.toStrict . TL.decodeUtf8 . encode $ a, "]"]

article1 = object [ "ID" .= (1 :: Int)
                  , "title" .= ("Foo bar" :: Text)
                  , "excerpt" .= ("summary" :: Text)
                  ]

article2 = object [ "ID" .= (2 :: Int)
                  , "title" .= ("The post" :: Text)
                  , "excerpt" .= ("summary" :: Text)
                  ]

fakeRequester "/posts" ps | length (ps `intersect` [ ("filter[year]", "2009")
                                                   , ("filter[monthnum]", "10")
                                                   , ("filter[name]", "the-post")]) == 3 =
  return $ enc article2
fakeRequester "/posts" _ = return $ enc article1
fakeRequester a b = error $ show a ++ show b

app :: [(Text, Text)] -> Maybe WordpressConfig -> SnapletInit App App
app tmpls mconf = makeSnaplet "app" "An snaplet example application." Nothing $ do
                     h <- nestSnaplet "" heist $ heistInit "templates"
                     addConfig h $ set scTemplateLocations (return templates) mempty
                     r <- nestSnaplet "" redis redisDBInitConf
                     let conf = fromMaybe (def { endpoint = ""
                                               , requester = Just fakeRequester
                                               , cachePeriod = NoCache
                                               })
                                          mconf
                     w <- nestSnaplet "" wordpress $ initWordpress' conf
                                                                    h
                                                                    redis
                                                                    wordpress
                     return $ App h r w
  where mkTmpl (name, html) = let (Right doc) = X.parseHTML "" (T.encodeUtf8 html)
                               in ([T.encodeUtf8 name], DocumentFile doc Nothing)
        templates = return $ M.fromList (map mkTmpl tmpls)


----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

shouldRenderTo :: Text -> Text -> Spec
shouldRenderTo = shouldRenderToPred T.isInfixOf

shouldRenderToExact :: Text -> Text -> Spec
shouldRenderToExact = shouldRenderToPred (==)

shouldRenderToPred :: (Text -> Text -> Bool) -> Text -> Text -> Spec
shouldRenderToPred pred tags match =
  snap (route []) (app [("test", tags)] Nothing) $
    it (T.unpack $ tags ++ " should render to contain " ++ match) $
      do t <- eval (do st <- getHeistState
                       builder <- (fst.fromJust) $ renderTemplate st "test"
                       return $ T.decodeUtf8 $ toByteString builder)
         if pred match t
           then setResult Success
            else setResult (Fail "Didn't contain.")

shouldRenderAtUrl :: Text -> Text -> Text -> Spec
shouldRenderAtUrl = shouldRenderAtUrlPre (return ())

shouldRenderAtUrlPre :: Handler App App () -> Text -> Text -> Text -> Spec
shouldRenderAtUrlPre act url tags match =
  snap (route [(T.encodeUtf8 url, h)]) (app [("test", tags)]
                                            (Just def { requester = Just fakeRequester
                                                      , endpoint = ""
                                                      , cachePeriod = NoCache
                                                      })) $
    it (T.unpack $ "rendered with url " ++ url ++  ", should contain " ++ match) $
      do eval act
         get url >>= shouldHaveText match
  where h = do st <- getHeistState
               render "test"

shouldRenderAtUrlPreCache :: Handler App App () -> Text -> Text -> Text -> Spec
shouldRenderAtUrlPreCache act url tags match =
  snap (route [(T.encodeUtf8 url, h)]) (app [("test", tags)]
                                            (Just def { requester = Just fakeRequester
                                                      , endpoint = ""
                                                      })) $
    it (T.unpack $ "rendered with url " ++ url ++  ", should contain " ++ match) $
      do eval act
         get url >>= shouldHaveText match
  where h = do st <- getHeistState
               render "test"

clearRedisCache :: Handler App App (Either R.Reply Integer)
clearRedisCache = runRedisDB redis (R.eval "return redis.call('del', unpack(redis.call('keys', ARGV[1])))" [] ["wordpress:*"])

shouldTransformTo :: Text -> Text -> Spec
shouldTransformTo from to =
  it (T.unpack ("should convert " ++ from ++ " to " ++ to)) $ transformName from `shouldBe` to

main :: IO ()
main = hspec $ do
  describe "<wpPosts>" $ do
    "<wpPosts><wpTitle/></wpPosts>" `shouldRenderTo` "Foo bar"
    "<wpPosts><wpID/></wpPosts>" `shouldRenderTo` "1"
    "<wpPosts><wpExcerpt/></wpPosts>" `shouldRenderTo` "summary"
  describe "<wpNoPostDuplicates/>" $ do
    "<wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts>"
      `shouldRenderToExact` "Foo bar"
    "<wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts>"
      `shouldRenderToExact` "Foo barFoo bar"
    "<wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/><wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts>"
      `shouldRenderToExact` "Foo barFoo bar"
    "<wpPosts><wpTitle/></wpPosts><wpPosts><wpTitle/></wpPosts><wpNoPostDuplicates/>"
      `shouldRenderToExact` "Foo barFoo bar"
  describe "<wpPostByPermalink>" $ do
    shouldRenderAtUrl "/2009/10/the-post/"
                      "<wpPostByPermalink><wpTitle/></wpPostByPermalink>"
                      "The post"
    shouldRenderAtUrl "/posts/2009/10/the-post/"
                      "<wpPostByPermalink><wpTitle/></wpPostByPermalink>"
                      "The post"
    shouldRenderAtUrl "/posts/2009/10/the-post"
                      "<wpPostByPermalink><wpTitle/></wpPostByPermalink>"
                      "The post"
    shouldRenderAtUrl "/2009/10/the-post/"
                      "<wpPostByPermalink><wpTitle/>: <wpExcerpt/></wpPostByPermalink>"
                      "The post: summary"
    describe "should grab post from cache if it's there" $
      let (Object a2) = article2 in
      shouldRenderAtUrlPreCache
        (void $ with wordpress $ cacheSet 10 (PostByPermalinkKey "2001" "10" "the-post")
                                             (enc a2))
        "/2001/10/the-post/"
        "<wpPostByPermalink><wpTitle/></wpPostByPermalink>"
        "The post"
  describe "caching" $ snap (route []) (app [] Nothing) $ afterEval (void clearRedisCache) $ do
    it "should find nothing for a non-existent post" $ do
      p <- eval (with wordpress $ cacheLookup (PostByPermalinkKey "2000" "1" "the-article"))
      p `shouldEqual` Nothing
    it "should find something if there is a post in cache" $ do
      eval (with wordpress $ cacheSet 10 (PostByPermalinkKey "2000" "1" "the-article")
                                         (enc article1))
      p <- eval (with wordpress $ cacheLookup (PostByPermalinkKey "2000" "1" "the-article"))
      p `shouldEqual` (Just $ enc article1)
    it "should not find single post after expire handler is called" $
      do eval (with wordpress $ cacheSet 10 (PostByPermalinkKey "2000" "1" "the-article")
                                            (enc article1))
         eval (with wordpress $ expirePost 1)
         eval (with wordpress $ cacheLookup (PostByPermalinkKey "2000" "1" "the-article"))
           >>= shouldEqual Nothing
    it "should not find post aggregates after expire handler is called" $
      do let key = PostsKey (Set.fromList [NumFilter 20, OffsetFilter 0, PageFilter 1, LimitFilter 20])
         eval (with wordpress $ cacheSet 10 key ("[" ++ enc article1 ++ "]"))
         eval (with wordpress $ expirePost 1)
         eval (with wordpress $ cacheLookup key)
           >>= shouldEqual Nothing
    it "should find a different single post after expiring another" $
      do let key1 = (PostByPermalinkKey "2000" "1" "the-article")
             key2 = (PostByPermalinkKey "2001" "2" "another-article")
         eval (with wordpress $ cacheSet 10 key1 (enc article1))
         eval (with wordpress $ cacheSet 10 key2 (enc article2))
         eval (with wordpress $ expirePost 1)
         eval (with wordpress $ cacheLookup key2) >>= shouldEqual (Just (enc article2))
  describe "transformName" $ do
    "ID" `shouldTransformTo` "wpID"
    "title" `shouldTransformTo` "wpTitle"
    "post_tag" `shouldTransformTo` "wpPostTag"
  describe "tag-specs" $ do
    it "should parse bare tag plus" $
      read "foo-bar" `shouldBe` (TaxPlus "foo-bar")
    it "should parse tag plus" $
      read "+foo-bar" `shouldBe` (TaxPlus "foo-bar")
    it "should parse tag minus" $
      read "-foo-bar" `shouldBe` (TaxMinus "foo-bar")
    it "should parse a list" $
      read "foo-bar,baz" `shouldBe` (TaxSpecList [TaxPlus "foo-bar", TaxPlus "baz"])
    it "should parse a list with mixed pluses and minuses" $
      read "+foo-bar,-baz,-qux" `shouldBe`
        (TaxSpecList [TaxPlus "foo-bar", TaxMinus "baz", TaxMinus "qux"])
    it "should round trip tag plus" $
      show (read "+foo-bar" :: TaxSpec) `shouldBe` "+foo-bar"
    it "should round trip tag minus" $
      show (read "-foo-bar" :: TaxSpec) `shouldBe` "-foo-bar"
    it "should add plus to bare tag plus when round tripping" $
      show (read "foo-bar" :: TaxSpec) `shouldBe` "+foo-bar"
    it "should round trip list" $
      show (read "+foo-bar,-baz,-qux" :: TaxSpecList) `shouldBe` "+foo-bar,-baz,-qux"
    it "should add plus to bare tag pluses in list roundtrip" $
      show (read "foo-bar,-baz,-qux" :: TaxSpecList) `shouldBe` "+foo-bar,-baz,-qux"

  describe "live tests (which require config file w/ user and pass to sandbox.jacobinmag.com)" $
    snap (route [("/2014/10/a-war-for-power", render "single")
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
                ,("/2014/10/the-assassination-of-detroit/", render "author-date")
                ])
         (app [("single", "<wpPostByPermalink><wpTitle/></wpPostByPermalink>")
              ,("many", "<wpPosts limit=2><wpTitle/></wpPosts>")
              ,("many1", "<wpPosts><wpTitle/></wpPosts>")
              ,("many2", "<wpPosts offset=1 limit=1><wpTitle/></wpPosts>")
              ,("many3", "<wpPosts offset=0 limit=1><wpTitle/></wpPosts>")
              ,("page1", "<wpPosts limit=10 page=1><wpTitle/></wpPosts>")
              ,("page2", "<wpPosts limit=10 page=2><wpTitle/></wpPosts>")
              ,("num1", "<wpPosts num=2><wpTitle/></wpPosts>")
              ,("num2", "<wpPosts num=2 page=2 limit=1><wpTitle/></wpPosts>")
              ,("num3", "<wpPosts num=1 page=3><wpTitle/></wpPosts>")
              ,("tag1", "<wpPosts tags=\"home-featured\" limit=10><wpTitle/></wpPosts>")
              ,("tag2", "<wpPosts limit=10><wpTitle/></wpPosts>")
              ,("tag3", "<wpPosts tags=\"+home-featured\" limit=10><wpTitle/></wpPosts>")
              ,("tag4", "<wpPosts tags=\"-home-featured\" limit=1><wpTitle/></wpPosts>")
              ,("tag5", "<wpPosts tags=\"+home-featured\" limit=1><wpTitle/></wpPosts>")
              ,("tag6", "<wpPosts tags=\"+home-featured,-featured-global\" limit=1><wpTitle/></wpPosts>")
              ,("tag7", "<wpPosts tags=\"+home-featured,+featured-global\" limit=1><wpTitle/></wpPosts>")
              ,("cat1", "<wpPosts categories=\"bookmarx\" limit=10><wpTitle/></wpPosts>")
              ,("cat2", "<wpPosts limit=10><wpTitle/></wpPosts>")
              ,("cat3", "<wpPosts categories=\"-bookmarx\" limit=10><wpTitle/></wpPosts>")
              ,("author-date", "<wpPostByPermalink><wpAuthor><wpName/></wpAuthor><wpDate><wpYear/>/<wpMonth/></wpDate></wpPostByPermalink>")
              ]
              (Just $ def { cachePeriod = NoCache,
                            endpoint = "https://sandbox.jacobinmag.com/wp-json" })) $
      do it "should have title on page" $
           get "/2014/10/a-war-for-power" >>= shouldHaveText "A War for Power"
         it "should not have most recent post's title" $
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
           get "/2014/10/the-assassination-of-detroit/" >>= shouldHaveText "Carlos Salazar"
         it "should be able to get customly parsed attribute date" $
           get "/2014/10/the-assassination-of-detroit/" >>= shouldHaveText "2014/10"
         it "should be able to restrict based on category" $
           do c1 <- get "/cat1"
              c2 <- get "/cat2"
              c1 `shouldNotEqual` c2
         it "should be able to make negative category queries" $
           do c1 <- get "/cat1"
              c2 <- get "/cat3"
              c1 `shouldNotEqual` c2
