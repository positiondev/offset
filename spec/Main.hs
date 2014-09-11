{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Blaze.ByteString.Builder
import           Control.Lens
import           Control.Monad            (join)
import           Data.Default
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Heist
import qualified Heist.Interpreted        as I
import           Snap                     (Handler, Method (..), Snaplet,
                                           addRoutes, makeSnaplet, nestSnaplet,
                                           route, subSnaplet)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.RedisDB
import           Snap.Snaplet.Wordpress
import           Test.Hspec
import           Test.Hspec.Core          (Result (..))
import           Test.Hspec.Snap
import qualified Text.XmlHtml             as X

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------

data App = App { _heist     :: Snaplet (Heist App)
               , _redis     :: Snaplet RedisDB
               , _wordpress :: Snaplet (Wordpress App) }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

routes = [("test", render "test")
         ]

fakeRequester "/posts" = return $ Just "[{\"ID\": 1, \"title\": \"Foo bar\"}]"
fakeRequster _ = return Nothing

app = makeSnaplet "app" "An snaplet example application." Nothing $ do
         addRoutes routes
         h <- nestSnaplet "" heist $ heistInit "templates"
         r <- nestSnaplet "" redis redisDBInitConf
         w <- nestSnaplet "" wordpress $ initWordpress' def { endpoint = ""
                                                            , requester = Just fakeRequester
                                                            , cachePeriod = NoCache
                                                            } h redis
         return $ App h r w

----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

shouldRenderTo :: Text -> Text -> SnapHspecM App ()
shouldRenderTo tags match =
  do t <- eval (join $ withHeistState $ \st ->
            do Just (builder, _) <- I.renderTemplate (I.addTemplate "test"
                                                      (X.docContent doc)
                                                      Nothing
                                                      st)
                                       "test"
               return $ T.decodeUtf8 $ toByteString builder)
     if match `T.isInfixOf` t
       then setResult Success
       else setResult (Fail $ (T.unpack t) ++ "\n\nShould have contained:\n\n" ++ (T.unpack match))
  where (Right doc) = X.parseHTML "" (T.encodeUtf8 tags)

main :: IO ()
main = hspec $ snap (route routes) app $ do
  describe "<wpPosts>" $ do
    it "should render with <wpTitle/> bound within" $
      "<wpPosts><wpTitle/></wpPosts>" `shouldRenderTo` "Foo bar"
