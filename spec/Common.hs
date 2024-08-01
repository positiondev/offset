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
import qualified Data.Aeson.KeyMap       as KM
import           Data.Aeson.Types        (parseMaybe)
import qualified Data.CaseInsensitive    as CI
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
import qualified Network.HTTP.Types.Header as H
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

data Ctxt = Ctxt { _req       :: FnRequest
                 , _redis     :: R.Connection
                 , _wordpress :: Wordpress Ctxt
                 , _wpsubs    :: Substitutions Ctxt
                 , _lib       :: Library Ctxt
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

enc :: ToJSON r => r -> Text
enc val = TL.toStrict . TL.decodeUtf8 . encode $ val

article1 :: Value
article1 = object [ "id" .= (1 :: Int)
                  , "date" .= ("2014-10-02T07:00:00" :: T.Text)
                  , "modified" .= ("2014-10-02T07:00:00" :: T.Text)
                  , "slug" .= ("foo-bar" :: T.Text)
                  , "title" .= object ["rendered" .= ("<i>Foo</i> bar" :: T.Text)]
                  , "excerpt" .= object ["rendered" .= ("summary" :: T.Text)]
                  , "departments" .= [ object [ "name" .= ("some department" :: T.Text)]]
                  , "department" .= ("15" :: T.Text)
                  , "authors" .= [ object ["name" .= ("Emma Goldman" :: T.Text)] ]
                  , "guest_authors" .= [ "2" :: Text, "4" :: Text ]
                  , "boolean" .= True ]

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
               , "department" .= ("15" :: Text)
               ]

department :: Value
department = object [ "id" .= (2 :: Int)
                    , "slug" .= ("2014-10-20T07:00:00" :: Text)
                    , "name" .= ("Sports" :: Text)
                    ]

authors :: [Value]
authors = [ object [ "id" .= (2 :: Int)
                   , "slug" .= ("lucy-parsons" :: Text)
                   , "name" .= ("Lucy Parsons" :: Text)
                   ],
            object [ "id" .= (4 :: Int)
                   , "slug" .= ("emma-goldman" :: Text)
                   , "name" .= ("Emma Goldman" :: Text) ]
          ]

boolFieldTrue :: Value
boolFieldTrue = object [ "person" .= object [ "name" .= ("Ada Lovelace" :: Text )]]

boolFieldFalse :: Value
boolFieldFalse = object [ "person" .= False ]

customFields :: [Field s]
customFields = [N "featured_image"
                 [N "attachment_meta"
                   [N "sizes"
                     [N "mag-featured"
                       [F "width"
                       ,F "height"
                       ,F "url"]
                     ,N "single-featured" [F "width"
                                          ,F "height"
                                          ,F "url"]]]]
               ,PM "departments" departmentFill
               ,Q "department" (UseId "wp/v2/department/")
               ,QM "guest_authors" (UseInclude "wp/v2/authors")
               ,M "authors" [F "name"]
               ,B "boolean" ]

departmentFill :: [Object] -> Fill s
departmentFill objs =
  let singleText :: Object -> Text
      singleText o = toStr $ KM.lookup "name" o
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
             ,(["feed"], parse "<wpPost>This is the title: <wpTitle /></wpPost>") ]

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

renderFeedContent :: Ctxt -> Object -> IO (Maybe T.Text)
renderFeedContent ctxt obj =
  renderWith tplLibrary (feedSubs [] wordpress obj Nothing) ctxt ["feed"]

buildEntryLinks :: Object -> [Link]
buildEntryLinks o =
  maybeToList $ buildPermalink "https://myurl.com" o

toWPResp :: Text -> IO (Either StatusCode WPResponse)
toWPResp body = return $ Right $ WPResponse [] body

fauxRequester :: Maybe (MVar [Text]) -> Text -> [(Text, Text)] -> IO (Either StatusCode WPResponse)
fauxRequester _ "/wp/v2/tags" [("slug", "home-featured")] =
  toWPResp $ enc [object [ "id" .= (177 :: Int)
                         , "slug" .= ("home-featured" :: Text)
                         ]]
fauxRequester _ "/wp/v2/tags" [("slug", "featured-global")] =
  toWPResp $ enc [object [ "id" .= (160 :: Int)
                         , "slug" .= ("featured-global" :: Text)
                         ]]
fauxRequester _ "/wp/v2/categories" [("slug", "bookmarx")] =
  toWPResp $ enc [object [ "id" .= (159 :: Int)
                         , "slug" .= ("bookmarx" :: Text)
                         , "meta" .= object ["links" .= object ["self" .= ("/159" :: Text)]]
                         ] ]
fauxRequester _ "/wp/v2/department/15" [] =
  toWPResp $ enc department
fauxRequester _ "/wp/v2/authors" [("include[]","2"), ("include[]","4")] =
  toWPResp $ enc authors
fauxRequester _ "/jacobin/featured-content/editors-picks" [] =
  toWPResp $ enc [object [ "post_date" .= ("2013-04-26 10:11:52" :: Text)
                       , "date" .= ("2014-04-26 10:11:52" :: Text)
                       , "post_date_gmt" .= ("2015-04-26 15:11:52" :: Text)
                       ]]
fauxRequester _ "/wp/v2/pages" [("slug", "a-first-page")] =
  toWPResp $ enc [page1]
fauxRequester _ "/dev/null" [] =
  toWPResp $ enc [object ["this_is_null" .= Null]]
fauxRequester _ "/dev/rendered_text" [] =
  toWPResp $ enc [object ["rendered_text" .= ("<i>Jezza</i>" :: Text)]]
fauxRequester _ "/false" [] =
  toWPResp $ enc [boolFieldFalse]
fauxRequester _ "/true" [] =
  toWPResp $ enc [boolFieldTrue]
fauxRequester _ "/object_array" [] =
  toWPResp $ enc $
    object ["some_array" .=
      [ object ["object_key" .= ("object value 1" :: Text)]
      , object ["object_key" .= ("object value 2" :: Text)]]]
fauxRequester _ "/number_array" [] = toWPResp $ enc $ object ["some_array" .= [1 :: Int, 2 :: Int, 3 :: Int]]
fauxRequester _ "/string_array" [] =  toWPResp $ enc $ object ["some_array" .= ["a" :: Text, "b" :: Text, "c" :: Text]]
fauxRequester _ "/many-pages" [] =
  return $ Right $ WPResponse [(CI.mk "X-WP-TotalPages", "478")
                              ,(CI.mk "X-WP-Total", "7337")] (enc [article1])
fauxRequester _ "/single-page" [] =
  return $ Right $ WPResponse [(CI.mk "X-WP-TotalPages", "1")
                              ,(CI.mk "X-WP-Total", "5")] (enc [article1])
fauxRequester mRecord rqPath rqParams = do
  case mRecord of
    Just record -> modifyMVar_ record $ return . (<> [mkUrlUnescape rqPath rqParams])
    Nothing -> return ()
  return $ Right $ WPResponse [(CI.mk "X-WP-TotalPages", "478")] (enc [article1])
  where mkUrlUnescape url params =
             url <>
              if null params
              then ""
              else "?" <> T.intercalate "&" (map (\(k, v) -> k <> "=" <> v) params)

initializer :: Either UserPassword Requester -> CacheBehavior -> Text -> IO Ctxt
initializer requester cache endpoint =
  do rconn <- R.connect R.defaultConnectInfo
     let wpconf = def { wpConfEndpoint = endpoint
                      , wpConfLogger = Nothing
                      , wpConfRequester = requester
                      , wpConfExtraFields = customFields
                      , wpConfCacheBehavior = cache
                   }
     let getUri :: StateT Ctxt IO Text
         getUri = do ctxt <- S.get
                     return (T.decodeUtf8 . rawPathInfo . fst . getRequest $ ctxt)
     (wp,wpSubs) <- initWordpress wpconf rconn getUri wordpress
     return (Ctxt defaultFnRequest rconn wp wpSubs mempty)

initFauxRequestNoCache :: IO Ctxt
initFauxRequestNoCache =
  initializer (Right $ Requester (fauxRequester Nothing)) NoCache ""

initNoRequestWithCache :: IO Ctxt
initNoRequestWithCache =
  initializer (Right $ Requester (\_ _ -> return (Right (WPResponse mempty "")) )) (CacheSeconds 600) ""

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
  let s = _wpsubs ctxt
  rendered <- evalStateT (runTemplate (toTpl t) ["fake-template"] s tplLibrary) ctxt
  ignoreWhitespace rendered `shouldBe` ignoreWhitespace output

-- Caching helpers

wpCacheGet' :: S.MonadIO m => Wordpress b -> WPKey -> m (Maybe Text)
wpCacheGet' wordpress' wpKey = do
  let WordpressInt{..} = cacheInternals wordpress'
  liftIO $ wpCacheGet wpKey

wpCacheSet' :: S.MonadIO m => Wordpress b -> WPKey -> Text -> m ()
wpCacheSet' wordpress' wpKey o = do
  let WordpressInt{..} = cacheInternals wordpress'
  liftIO $ wpCacheSet wpKey o

wpExpireAggregates' :: S.MonadIO m => Wordpress t -> m Bool
wpExpireAggregates' Wordpress{..} =
  liftIO wpExpireAggregates

wpExpirePost' :: S.MonadIO m => Wordpress t -> WPKey -> m Bool
wpExpirePost'  Wordpress{..} k =
  liftIO $ wpExpirePost k

{-
shouldRenderAtUrlContaining' :: (TemplateName, Ctxt)
                            -> (TemplateUrl, Text)
                            -> Expectation
shouldRenderAtUrlContaining' (template, ctxt) (url, match) = do
    let requestWithUrl = defaultRequest {rawPathInfo = T.encodeUtf8 url }
    let ctxt' = setRequest ctxt
                 $ (\(x,y) -> (requestWithUrl, y)) defaultFnRequest
    let s = _wpsubs ctxt
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
      let s = _wpsubs ctxt
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
