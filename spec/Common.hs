{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Common where

import           Control.Concurrent.MVar
import           Control.Lens            hiding ((.=))
import           Control.Monad           (void)
import           Control.Monad.State     (StateT, evalStateT)
import qualified Control.Monad.State     as S
import           Control.Monad.Trans     (liftIO)
import           Data.Aeson              hiding (Success)
import           Data.Default
import qualified Data.HashMap.Strict     as HM
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Database.Redis          as R
import           Network.Wai             (defaultRequest, rawPathInfo)
import           Prelude                 hiding ((++))
import           Test.Hspec
import           Web.Fn
import           Web.Larceny

import           Web.Offset
import           Web.Offset.Cache.Redis
import           Web.Offset.Types

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------

data Ctxt = Ctxt { _req     :: FnRequest
                 , _redis   :: R.Connection
                 , _cms     :: CMS Ctxt
                 , _cmssubs :: Substitutions Ctxt
                 , _lib     :: Library Ctxt
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

enc :: ToJSON r => r -> Text
enc val = TL.toStrict . TL.decodeUtf8 . encode $ val

article1 :: Value
article1 = object [ "id" .= (1 :: Int)
                  , "date" .= ("2014-10-20T07:00:00" :: Text)
                  , "title" .= object ["rendered" .= ("Foo bar" :: Text)]
                  , "excerpt" .= object ["rendered" .= ("summary" :: Text)]
                  , "departments" .= [ object [ "name" .= ("some department" :: Text)]]
                  ]

article2 :: Value
article2 = object [ "id" .= (2 :: Int)
                  , "date" .= ("2014-10-20T07:00:00" :: Text)
                  , "title" .= object ["rendered" .= ("The post" :: Text)]
                  , "excerpt" .= object ["rendered" .= ("summary" :: Text)]
                  ]

page1 :: Value
page1 = object [ "id" .= (3 :: Int)
               , "date" .= ("2014-10-20T07:00:00" :: Text)
               , "title" .= object ["rendered" .= ("Page foo" :: Text)]
               , "content" .= object ["rendered" .= ("<b>rendered</b> page content" :: Text)]
               ]

customFields :: [Field s]
customFields = [N "featured_image" [N "attachment_meta" [N "sizes" [N "mag-featured" [F "width"
                                                                                     ,F "height"
                                                                                     ,F "url"]
                                                                   ,N "single-featured" [F "width"
                                                                                        ,F "height"
                                                                                        ,F "url"]]]]
               ,PM "departments" departmentFill ]

departmentFill :: [Object] -> Fill s
departmentFill objs =
  let singleText :: Object -> Text
      singleText o = toStr $ HM.lookup "name" o
      toStr (Just (String s)) = s
      toStr _ = error "not a string" in
  mapSubs (\x -> subs [("name", textFill x)])
    (map singleText objs)

tplLibrary :: Library Ctxt
tplLibrary =
  M.fromList [(["single"], parse "<wp><wpPostByPermalink><wpTitle/></wpPostByPermalink></wp>")
             ,(["single-page"], parse "<wpPage name=a-first-page />")
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
             ,(["department"], parse "<wpPosts departments=\"sports\"><wpTitle/></wpPosts>")
             ,(["author-date"], parse "Hello<wp><wpPostByPermalink><wpAuthor><wpName/></wpAuthor><wpDate><wpYear/>/<wpMonth/></wpDate></wpPostByPermalink></wp>")
             ,(["fields"], parse "<wp><wpPosts limit=1 categories=\"-cat1\"><wpFeaturedImage><wpAttachmentMeta><wpSizes><wpThumbnail><wpUrl/></wpThumbnail></wpSizes></wpAttachmentMeta></wpFeaturedImage></wpPosts></wp>")
             ,(["custom-endpoint-object"], parse "<wpCustom endpoint=\"wp/v2/taxonomies\"><wpCategory><wpRestBase /></wpCategory></wpCustom>")
             ,(["custom-endpoint-array"], parse "<wpCustom endpoint=\"wp/v2/posts\"><wpDate /></wpCustom>")
             ,(["custom-endpoint-enter-the-matrix"], parse "<wpCustom endpoint=\"wp/v2/posts\"><wpCustom endpoint=\"wp/v2/posts/${wpId}\"><wpDate /></wpCustom></wpCustom>")
               ]

renderLarceny :: Ctxt ->
                 Text ->
                 IO (Maybe Text)
renderLarceny ctxt name =
  do let tpl = M.lookup [name] tplLibrary
     case tpl of
       Just t -> do
         rendered <- evalStateT (runTemplate t [name] (ctxt ^. cmssubs) tplLibrary) ctxt
         return $ Just rendered
       _ -> return Nothing

fauxRequester :: Maybe (MVar [Text]) -> Text -> [(Text, Text)] -> IO (Either StatusCode Text)
fauxRequester _ "/wp/v2/tags" [("slug", "home-featured")] =
  return $ Right $ enc [object [ "id" .= (177 :: Int)
                       , "slug" .= ("home-featured" :: Text)
                       ]]
fauxRequester _ "/wp/v2/tags" [("slug", "featured-global")] =
  return $ Right $ enc [object [ "id" .= (160 :: Int)
                       , "slug" .= ("featured-global" :: Text)
                       ]]
fauxRequester _ "/wp/v2/categories" [("slug", "bookmarx")] =
  return $ Right $ enc [object [ "id" .= (159 :: Int)
                       , "slug" .= ("bookmarx" :: Text)
                       , "meta" .= object ["links" .= object ["self" .= ("/159" :: Text)]]
                       ] ]
fauxRequester _ "/jacobin/featured-content/editors-picks" [] =
  return $ Right $ enc [object [ "post_date" .= ("2013-04-26 10:11:52" :: Text)
                       , "date" .= ("2014-04-26 10:11:52" :: Text)
                       , "post_date_gmt" .= ("2015-04-26 15:11:52" :: Text)
                       ]]
fauxRequester _ "/wp/v2/pages" [("slug", "a-first-page")] =
  return $ Right $ enc [page1]
fauxRequester _ "/dev/null" [] =
  return $ Right $ enc [object ["this_is_null" .= Null]]
fauxRequester mRecord rqPath rqParams = do
  case mRecord of
    Just record -> modifyMVar_ record $ return . (<> [mkUrlUnescape rqPath rqParams])
    Nothing -> return ()
  return $ Right $ enc [article1]
  where mkUrlUnescape url params =
             url <> "?"
          <> T.intercalate "&" (map (\(k, v) -> k <> "=" <> v) params)

initializer :: Either UserPassword Requester -> CacheBehavior -> Text -> IO Ctxt
initializer requester cache endpoint =
  do rconn <- R.connect R.defaultConnectInfo
     let wpconf = def { cmsConfEndpoint = endpoint
                      , cmsConfLogger = Nothing
                      , cmsConfRequest = requester
                      , cmsConfExtraFields = customFields
                      , cmsConfCacheBehavior = cache
                   }
     let getUri :: StateT Ctxt IO Text
         getUri = do ctxt <- S.get
                     return (T.decodeUtf8 . rawPathInfo . fst . getRequest $ ctxt)
     (cms', cmssubs) <- initCMS wpconf rconn getUri cms
     return (Ctxt defaultFnRequest rconn cms' cmssubs mempty)

initFauxRequestNoCache :: IO Ctxt
initFauxRequestNoCache =
  initializer (Right $ Requester (fauxRequester Nothing)) NoCache ""

initNoRequestWithCache :: IO Ctxt
initNoRequestWithCache =
  initializer (Right $ Requester (\_ _ -> return (Right "") )) (CacheSeconds 600) ""


----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------


type TemplateName = Text
type TemplateText = Text
type TemplateUrl = Text

clearRedisCache :: Ctxt -> IO Bool
clearRedisCache ctxt = R.runRedis (_redis ctxt) (rdelstar "wordpress:*")

unobj :: Value -> Object
unobj (Object x) = x
unobj _ = error "Not an object"

toTpl :: Text -> Template s
toTpl tpl = parse (TL.fromStrict tpl)

ignoreWhitespace :: Text -> Text
ignoreWhitespace = T.replace " " ""

shouldRender :: TemplateText
             -> Text
             -> Expectation
shouldRender t output = do
  ctxt <- initFauxRequestNoCache
  let s = _cmssubs ctxt
  rendered <- evalStateT (runTemplate (toTpl t) [] s mempty) ctxt
  ignoreWhitespace rendered `shouldBe` ignoreWhitespace output

-- Caching helpers

cmsCacheGet' :: S.MonadIO m => CMS b -> CMSKey -> m (Maybe Text)
cmsCacheGet' cms' wpKey = do
  let CMSInt{..} = cacheInternals cms'
  liftIO $ cmsCacheGet wpKey

cmsCacheSet' :: S.MonadIO m => CMS b -> CMSKey -> Text -> m ()
cmsCacheSet' cms' wpKey o = do
  let CMSInt{..} = cacheInternals cms'
  liftIO $ cmsCacheSet wpKey o

cmsExpireAggregates' :: S.MonadIO m => CMS t -> m Bool
cmsExpireAggregates' CMS{..} =
  liftIO cmsExpireAggregates

cmsExpirePost' :: S.MonadIO m => CMS t -> CMSKey -> m Bool
cmsExpirePost'  CMS{..} k =
  liftIO $ cmsExpirePost k

{-
shouldRenderAtUrlContaining' :: (TemplateName, Ctxt)
                            -> (TemplateUrl, Text)
                            -> Expectation
shouldRenderAtUrlContaining' (template, ctxt) (url, match) = do
    let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 url }
    let ctxt' = setRequest ctxt
                 $ (\(x,y) -> (requestWithUrl, y)) defaultFnRequest
    let s = _cmssubs ctxt
    rendered <- renderLarceny ctxt' template
    print rendered
    let rendered' = fromMaybe "" rendered
    (match `T.isInfixOf` rendered') `shouldBe` True -}

-- query helpers

shouldQueryTo :: TemplateText -> [Text] -> Spec
shouldQueryTo hQuery wpQuery =
  it ("should query from " <> T.unpack hQuery) $ do
      record <- liftIO $ newMVar []
      ctxt <- liftIO $ initializer
                       (Right $ Requester $ fauxRequester (Just record))
                       NoCache
                       ""
      let s = _cmssubs ctxt
      void $ evalStateT (runTemplate (toTpl hQuery) [] s mempty) ctxt
      x <- liftIO $ tryTakeMVar record
      x `shouldBe` Just wpQuery


-- live test helpers


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

shouldRenderContaining :: (TemplateName, Ctxt) -> Text -> Expectation
shouldRenderContaining (template, ctxt) match = do
    rendered <- renderLarceny ctxt template
    let rendered' = fromMaybe "" rendered
    (match `T.isInfixOf` rendered') `shouldBe` True

shouldNotRenderContaining :: (TemplateName, Ctxt) -> Text -> Expectation
shouldNotRenderContaining (template, ctxt) match = do
    rendered <- renderLarceny ctxt template
    let rendered' = fromMaybe "" rendered
    (match `T.isInfixOf` rendered') `shouldBe` False

shouldRenderAtUrlContaining :: (TemplateName, Ctxt)
                            -> (TemplateUrl, Text)
                            -> Expectation
shouldRenderAtUrlContaining (template, ctxt) (url, match) = do
    let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 url }
    let ctxt' = setRequest ctxt
                 $ (\(_,x) -> (requestWithUrl, x)) defaultFnRequest
    rendered <- renderLarceny ctxt' template
    let rendered' = fromMaybe "" rendered
    (match `T.isInfixOf` rendered') `shouldBe` True
