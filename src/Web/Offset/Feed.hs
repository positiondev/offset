{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards        #-}

module Web.Offset.Feed where

import           Control.Monad.State
import           Data.Aeson              hiding (decode, encode, json, object)
import           Data.Aeson.Types (parseMaybe)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
import           Text.XML.Light
import           Data.Time.Clock
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Web.Atom hiding (Link)
import qualified Web.Atom as A (Link(..))
import qualified Data.Text               as T

import           Web.Offset.Utils
import           Web.Offset.Field
import           Web.Offset.Splices
import           Web.Offset.Types
import           Web.Offset.Date
import           Web.Offset.Link

data WPFeed =
  WPFeed { wpFeedURI :: T.Text
         , wpFeedTitle :: T.Text
         , wpFeedIcon :: Maybe T.Text
         , wpFeedLogo :: Maybe T.Text
         , wpBuildLinks :: Object -> [Link]
         , wpRenderEntry :: Object -> IO (Maybe T.Text) }

toXMLFeed :: Wordpress b -> WPFeed -> IO T.Text
toXMLFeed wp (WPFeed uri title icon logo linkmaker renderer) = do
  wpEntries <- getWPEntries wp
  let mostRecentUpdate = maximum (map wpEntryUpdated wpEntries)
  entries <- mapM (toEntry linkmaker renderer) wpEntries
  let feed = (makeFeed (unsafeURI $ T.unpack uri) (TextPlain title) mostRecentUpdate)
             { feedIcon = unsafeURI <$> T.unpack <$> icon
             , feedLogo = unsafeURI <$> T.unpack <$> logo
             , feedEntries = entries }
  return $ T.pack $ ppTopElement $ feedXML xmlgen feed

getWPEntries :: Wordpress b -> IO [WPEntry]
getWPEntries wp = do
  res <- liftIO $ cachingGetRetry wp (mkWPKey [] allPostsQuery)
  case res of
    Left statusCode -> error $ "Status code error: " ++ show statusCode
    Right resp ->
      case decode resp of
        Just posts -> return posts
        Nothing -> error $ "Couldn't decode: " <> T.unpack resp

allPostsQuery :: WPQuery
allPostsQuery =
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

wpEntryContent :: (Object -> IO (Maybe T.Text))
               -> WPEntry
               -> IO (Maybe (Web.Atom.Content e))
wpEntryContent f wpentry =
  do rendered <- f (wpEntryJSON wpentry)
     case rendered of
       Just x -> return $ Just (InlineHTMLContent x)
       Nothing -> return Nothing

toEntry :: (Object -> [Link])
        -> (Object -> IO (Maybe T.Text))
        -> WPEntry
        -> IO (Entry e)
toEntry buildLinks renderContent entry@WPEntry{..} = do
  content <- wpEntryContent renderContent entry
  let baseEntry = makeEntry (unsafeURI wpEntryId) (TextHTML wpEntryTitle) wpEntryUpdated
  return $ baseEntry { entryPublished = Just wpEntryPublished
                     , entrySummary = Just (TextHTML wpEntrySummary)
                     , entryContent = content
                     , entryAuthors = map unWP wpEntryAuthors
                     , entryLinks = map toAtomLink (buildLinks wpEntryJSON)}

toAtomLink :: Link -> A.Link
toAtomLink (Link href title) =
  A.Link { linkHref = unsafeURI $ T.unpack href
         , linkRel = Nothing
         , linkType = Nothing
         , linkHrefLang = Nothing
         , linkTitle = Just title
         , linkLength = Nothing }

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
