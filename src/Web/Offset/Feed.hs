{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Web.Offset.Feed where

import           Control.Monad.State
import           Data.Aeson              hiding (decode, encode, json, object)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
import           Text.XML.Light
import           Data.Time.Clock
import           Web.Atom
import qualified Data.Text               as T

import           Web.Offset.Utils
import           Web.Offset.Field
import           Web.Offset.Splices
import           Web.Offset.Types

data WPFeed =
  WPFeed { wpFeedURI :: T.Text
         , wpFeedTitle :: T.Text
         , wpFeedIcon :: Maybe T.Text
         , wpFeedLogo :: Maybe T.Text
         , wpRenderEntry :: Object -> IO (Maybe T.Text) }

toXMLFeed :: Wordpress b -> WPFeed -> IO T.Text
toXMLFeed wp (WPFeed uri title icon logo renderer) = do
  wpEntries <- getWPEntries wp
  let mostRecentUpdate = maximum (map wpEntryUpdated wpEntries)
  entries <- mapM (toEntry renderer) wpEntries
  let feed = (makeFeed (unsafeURI $ T.unpack uri) (TextPlain title) mostRecentUpdate)
             { feedIcon = unsafeURI <$> T.unpack <$> icon
             , feedLogo = unsafeURI <$> T.unpack <$> logo
             , feedEntries = entries }
  return $ T.pack $ ppTopElement $ feedXML xmlgen feed

getWPEntries :: Wordpress b -> IO [WPEntry]
getWPEntries wp = do
  res <- liftIO $ cachingGetRetry wp (mkWPKey [] allQuery)
  case res of
    Left statusCode -> error $ "Status code error: " ++ show statusCode
    Right resp ->
      case decode resp of
        Just posts -> return posts
        Nothing -> error $ "Couldn't decode: " <> T.unpack resp

allQuery :: WPQuery
allQuery =
  WPPostsQuery{ qlimit = 20
              , qnum = 20
              , qoffset = 0
              , qpage = 1
              , qtaxes = []
              , quser = Nothing }

xmlgen :: XMLGen Element Text.XML.Light.Content QName Attr
xmlgen = XMLGen
    { xmlElem     = \n as ns    -> Element n as ns Nothing
    , xmlName     = \nsMay name -> QName (T.unpack name)
                                          (fmap T.unpack nsMay) Nothing
    , xmlAttr     = \k v        -> Attr k (T.unpack v)
    , xmlTextNode = \t          -> Text $ CData CDataText (T.unpack t) Nothing
    , xmlElemNode = Elem }

wpEntryContent :: (Object -> IO (Maybe T.Text)) -> WPEntry -> IO (Maybe (Web.Atom.Content e))
wpEntryContent f wpentry =
  do rendered <- f (wpEntryJSON wpentry)
     case rendered of
       Just x -> return $ Just (InlineHTMLContent x)
       Nothing -> return Nothing

toEntry :: (Object -> IO (Maybe T.Text)) -> WPEntry -> IO (Entry e)
toEntry f e@(WPEntry uri title updated published summary authors _json) = do
  content <- wpEntryContent f e
  return (makeEntry (unsafeURI uri) (TextHTML title) updated)
           { entryPublished = Just published
           , entrySummary = Just (TextHTML summary)
           , entryContent = content
           , entryAuthors = map unWP authors }

data WPEntry =
  WPEntry { wpEntryId :: String
          , wpEntryTitle :: T.Text
          , wpEntryUpdated :: UTCTime
          , wpEntryPublished :: UTCTime
          , wpEntrySummary :: T.Text
          , wpEntryAuthors :: [WPPerson]
          , wpEntryJSON :: Object } deriving (Eq, Show)

instance FromJSON WPEntry where
  parseJSON (Object v) =
    WPEntry <$> (do t <- v .: "guid"
                    t .: "rendered") <*>
                (do t <- v .: "title"
                    t .: "rendered") <*>
                (jsonParseDate <$> (v .:"modified")) <*>
                (jsonParseDate <$> (v .: "date")) <*>
                (do e <- v .: "excerpt"
                    e .: "rendered") <*>
                v .: "authors" <*>
                return v
  parseJSON _ = error "bad post"

newtype WPPerson = WPPerson { unWP :: Person } deriving (Eq, Show)

instance FromJSON WPPerson where
  parseJSON (Object v) =
    WPPerson <$> (Person <$> v .: "name" <*> return Nothing <*> return Nothing)
  parseJSON _ = error "bad author"

jsonParseDate :: Value -> UTCTime
jsonParseDate (String t) =
  fromMaybe (error $ "Unable to parse date: " ++ show t)
    $ parseWPDate "%Y-%m-%dT%H:%M:%S" t
jsonParseDate _ = error "Unable to parse date."
