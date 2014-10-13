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

enc = TL.toStrict . TL.decodeUtf8 . encode

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
  return $ "[" ++ enc article2 ++ "]"
fakeRequester "/posts" _ = return $ "[" ++  enc article1 ++ "]"

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
  snap (route [(T.encodeUtf8 url, h)]) (app [("test", tags)] Nothing) $
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
    shouldRenderAtUrl "/2009/10/the-post/"
                      "<wpPostByPermalink><wpTitle/>: <wpExcerpt/></wpPostByPermalink>"
                      "The post: summary"
    describe "should grab post from cache if it's there" $
      let (Object a2) = article2 in
      shouldRenderAtUrlPre
        (void $ with wordpress $ cacheSet (PostByPermalinkKey "2001" "10" "the-post") a2)
        "/2001/10/the-post/"
        "<wpPostByPermalink><wpTitle/></wpPostByPermalink>"
        "The post"
  describe "caching" $ snap (route []) (app [] Nothing) $ afterEval (void clearRedisCache) $ do
    it "should find nothing for a non-existent post" $ do
      p <- eval (with wordpress $ cacheLookup (PostByPermalinkKey "2000" "1" "the-article"))
      p `shouldEqual` Nothing
    it "should find something if there is a post in cache" $ do
      eval (runRedisDB redis (R.set "wordpress:post_perma:2000_1_the-article" (T.encodeUtf8 $ enc article1)))
      p <- eval (with wordpress $ cacheLookup (PostByPermalinkKey "2000" "1" "the-article"))
      let Just postcontent = decodeStrict (T.encodeUtf8 $ enc article1)
      p `shouldEqual` (Just postcontent)
  describe "transformName" $ do
    "ID" `shouldTransformTo` "wpID"
    "title" `shouldTransformTo` "wpTitle"
    "post_tag" `shouldTransformTo` "wpPostTag"

  describe "live tests (which require config file w/ user and pass)" $
    snap (route [("/2014/10/a-war-for-power", render "single")
                ,("/many", render "many")
                ,("/many2", render "many2")
                ,("/many3", render "many3")
                ,("/page1", render "page1")
                ,("/page2", render "page2")
                ,("/num1", render "num1")
                ,("/num2", render "num2")
                ,("/num3", render "num3")
                ])
         (app [("single", "<wpPostByPermalink><wpTitle/></wpPostByPermalink>")
              ,("many", "<wpPosts limit=2><wpTitle/></wpPosts>")
              ,("many2", "<wpPosts offset=1 limit=1><wpTitle/></wpPosts>")
              ,("many3", "<wpPosts offset=0 limit=1><wpTitle/></wpPosts>")
              ,("page1", "<wpPosts limit=10 page=1><wpTitle/></wpPosts>")
              ,("page2", "<wpPosts limit=10 page=2><wpTitle/></wpPosts>")
              ,("num1", "<wpPosts num=2><wpTitle/></wpPosts>")
              ,("num2", "<wpPosts num=2 page=2 limit=1><wpTitle/></wpPosts>")
              ,("num3", "<wpPosts num=1 page=3><wpTitle/></wpPosts>")
              ]
              (Just $ def { endpoint = "https://sandbox.jacobinmag.com/wp-json" })) $
      do it "should have title on page" $
           get "/2014/10/a-war-for-power" >>= shouldHaveText "A War for Power"
         describe "NOTE: This is a fragile test (not super useful longterm)." $
           it "should not have most recent post's title" $
             get "/many" >>= shouldNotHaveText "All in the Family"
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
