{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Prelude                     hiding ((++))

import           Blaze.ByteString.Builder
import           Control.Lens
import           Control.Monad               (join)
import           Control.Monad.Trans         (liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson                  hiding (Success)
import           Data.Default
import qualified Data.HashMap.Strict         as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
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

article1 = "{\"ID\": 1, \"title\": \"Foo bar\"}"
article2 = "{\"ID\": 2, \"title\": \"The post\"}"

fakeRequester "/posts" = return $ Just $ "[" ++  article1 ++ "]"
fakeRequester "/posts?filter[year]=2009&filter[monthnum]=10&filter[name]=the-post" =
  return $ Just $ "[" ++ article2 ++ "]"
fakeRequster _ = return Nothing

app :: [(Text, Text)] -> SnapletInit App App
app tmpls = makeSnaplet "app" "An snaplet example application." Nothing $ do
               h <- nestSnaplet "" heist $ heistInit "templates"
               addConfig h mempty { hcTemplateLocations = return templates}
               r <- nestSnaplet "" redis redisDBInitConf
               w <- nestSnaplet "" wordpress $ initWordpress' def { endpoint = ""
                                                                  , requester = Just fakeRequester
                                                                  , cachePeriod = NoCache
                                                                  }
                                                              h
                                                              r
               return $ App h r w
  where mkTmpl (name, html) = let (Right doc) = X.parseHTML "" (T.encodeUtf8 html)
                               in ([T.encodeUtf8 name], DocumentFile doc Nothing)
        templates = return $ M.fromList (map mkTmpl tmpls)


----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

shouldRenderTo :: Text -> Text -> Spec
shouldRenderTo tags match =
  snap (route []) (app [("test", tags)]) $
    it (T.unpack $ tags ++ " should render to contain " ++ match) $
      do t <- eval (do st <- getHeistState
                       builder <- (fst.fromJust) $ renderTemplate st "test"
                       return $ T.decodeUtf8 $ toByteString builder)
         if match `T.isInfixOf` t
           then setResult Success
            else setResult (Fail "Didn't contain.")

shouldRenderAtUrl :: Text -> Text -> Text -> Spec
shouldRenderAtUrl url tags match =
  snap (route [(T.encodeUtf8 url, h)]) (app [("test", tags)]) $
    it (T.unpack $ "rendered with url " ++ url ++  ", should contain " ++ match) $
      get url >>= shouldHaveText match
  where h = do st <- getHeistState
               render "test"

clearRedisCache :: Handler App App (Either R.Reply Integer)
clearRedisCache = runRedisDB redis (R.eval "return redis.call('del', unpack(redis.call('keys', ARGV[1])))" [] ["wordpress:*"])

main :: IO ()
main = hspec $ do
  describe "<wpPosts>" $ do
    "<wpPosts><wpTitle/></wpPosts>" `shouldRenderTo` "Foo bar"
  describe "<wpPostByPermalink>" $ do
    shouldRenderAtUrl "/2009/10/the-post/"
                      "<wpPostByPermalink><wpTitle/></wpPostByPermalink>"
                      "The post"
  describe "caching" $ snap (route []) (app []) $ afterEval (void clearRedisCache) $ do
    it "should find nothing for a non-existent post" $ do
      p <- eval (with wordpress $ cacheLookup (PostByPermalinkKey "2000" "1" "the-article"))
      p `shouldEqual` Nothing
    it "should find something if there is a post in cache" $ do
      eval (runRedisDB redis (R.set "wordpress:post_perma:2000_1_the-article" (T.encodeUtf8 article1)))
      p <- eval (with wordpress $ cacheLookup (PostByPermalinkKey "2000" "1" "the-article"))
      let Just postcontent = decodeStrict (T.encodeUtf8 article1)
      p `shouldEqual` (Just postcontent)
