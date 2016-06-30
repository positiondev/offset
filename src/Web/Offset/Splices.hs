{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Offset.Splices where

import Control.Monad.State
import           Control.Applicative     ((<|>))
import           Control.Lens            hiding (children)
import           Control.Monad           (void, sequence)
import           Control.Monad.Trans     (lift, liftIO)
import           Data.Aeson              hiding (decode, encode)
import qualified Data.Attoparsec.Text    as A
import           Data.Char               (toUpper)
import qualified Data.HashMap.Strict     as M
import           Data.IntSet             (IntSet)
import qualified Data.IntSet             as IntSet
import           Data.Map.Syntax
import           Data.Maybe              (fromJust, fromMaybe)
import           Data.Monoid
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml            as X
import           Larceny

import           Web.Offset.Field
import           Web.Offset.Posts
import           Web.Offset.Queries
import           Web.Offset.Types
import           Web.Offset.Utils

wordpressSubs ::   Wordpress b
                 -> [Field s]
                 -> StateT s IO Text
                 -> WPLens b s
                 -> Larceny.Substitutions s
wordpressSubs wp extraFields getURI wpLens =
  fills [ ("wpPosts", wpPostsFill wp extraFields wpLens)
        , ("wpPostByPermalink", wpPostByPermalinkFill extraFields getURI wpLens)
        , ("wpPage", wpPageFill wpLens)
        , ("wpNoPostDuplicates", wpNoPostDuplicatesFill wpLens)
        , ("wp", wpPrefetch wp extraFields getURI wpLens) ]

wpPostsFill ::  Wordpress b
              -> [Field s]
              -> WPLens b s
              -> Fill s
wpPostsFill wp extraFields wpLens = \_m (pth, tpl) lib ->
  do --n <- undefined--getParamNode
     --attrs <- undefined --runAttributes (X.elementAttrs n)
     tagDict <- liftIO $ lookupTaxDict (TaxDictKey "post_tag") wp
     catDict <- liftIO $ lookupTaxDict (TaxDictKey "category") wp
     let postsQuery = parseQueryNode [] -- attrs
     let wpKey = mkWPKey tagDict catDict postsQuery
     res <- liftIO $ cachingGetRetry wp wpKey
     case decode res of
       Just posts -> do let postsW = extractPostIds posts
                        Wordpress{..} <- use wpLens
                        let postsND = take (qlimit postsQuery)
                                      . noDuplicates requestPostSet $ postsW
                        addPostIds wpLens (map fst postsND)
                        T.concat <$>  mapM
                          (\n -> runTemplate tpl pth (postSubs extraFields n) lib)
                          (map snd postsND)
       Nothing -> return ""
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just postSet) = filter (\(i,_) -> IntSet.notMember i postSet)

-- maybe done
wpPostByPermalinkFill :: [Field s]
                        -> StateT s IO Text
                        -> WPLens b s
                        -> Fill s
wpPostByPermalinkFill extraFields getURI wpLens _m (pth, Template tpl) l =
  do uri <- getURI
     let mperma = parsePermalink uri
     case mperma of
       Nothing -> return ""
       Just (year, month, slug) ->
         do res <- wpGetPost wpLens (PostByPermalinkKey year month slug)
            case res of
              Just post -> do addPostIds wpLens [fst (extractPostId post)]
                              tpl pth (postSubs extraFields post) l
              _ -> return ""

-- maybe done
wpNoPostDuplicatesFill :: WPLens b s -> Fill s
wpNoPostDuplicatesFill wpLens _m _t _l =
  do w@Wordpress{..} <- use wpLens
     case requestPostSet of
       Nothing -> assign wpLens
                    w{requestPostSet = Just IntSet.empty}
       Just _ -> return ()
     return ""

-- maybe done
wpPageFill :: WPLens b s -> Fill s
wpPageFill wpLens =
  useAttrs (a "name" pageFill)
  where pageFill Nothing _ = text ""
        pageFill (Just slug) _ = \_m _t _l ->
         do res <- wpGetPost wpLens (PageKey slug)
            return $ case res of
                       Just page -> case M.lookup "content" page of
                                      Just (String c) -> c
                                      _ -> ""
                       _ -> ""

-- maybe done
postSubs :: [Field s] -> Object -> Substitutions s
postSubs extra object = fills (map (buildSplice object) (mergeFields postFields extra))
  where buildSplice o (F n) =
          (transformName n, text $ getText n o)
        buildSplice o (P n fill') =
          (transformName n, fill' $ getText n o)
        buildSplice o (N n fs) =
          (transformName n, fill $ fills
                            (map (buildSplice (unObj . M.lookup n $ o)) fs))
        buildSplice o (C n path) =
          (transformName n, text (getText (last path) . traverseObject (init path) $ o))
        buildSplice o (CN n path fs) =
          (transformName n, fill $ fills
                            (map (buildSplice (traverseObject path o)) fs))
        buildSplice o (M n fs) =
          (transformName n,
            mapFills (\oinner -> fills $ map (buildSplice oinner) fs)
                       (unArray . M.lookup n $ o))

        unObj (Just (Object o)) = o
        unObj _ = M.empty
        unArray (Just (Array v)) = map (unObj . Just) $ V.toList v
        unArray _ = []
        traverseObject pth o = foldl (\o x -> unObj . M.lookup x $ o) o pth
        getText n o = case M.lookup n o of
                        Just (String t) -> t
                        Just (Number i) -> T.pack $ show i
                        _ -> ""

-- * -- Internal -- * --

parseQueryNode :: [(Text, Text)] -> WPQuery
parseQueryNode attrs =
  mkPostsQuery (readSafe =<< lookup "limit" attrs)
               (readSafe =<< lookup "num" attrs)
               (readSafe =<< lookup "offset" attrs)
               (readSafe =<< lookup "page" attrs)
               (readSafe =<< lookup "tags" attrs)
               (readSafe =<< lookup "categories" attrs)
               (lookup "user" attrs)

mkPostsQuery :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
       -> Maybe (TaxSpecList TagType) -> Maybe (TaxSpecList CatType)
       -> Maybe Text
       -> WPQuery
mkPostsQuery l n o p ts cs us =
  WPPostsQuery{ qlimit = fromMaybe 20 l
              , qnum = fromMaybe 20 n
              , qoffset = fromMaybe 0 o
              , qpage = fromMaybe 1 p
              , qtags = fromMaybe (TaxSpecList []) ts
              , qcats = fromMaybe (TaxSpecList []) cs
              , quser = us
              }

wpPrefetch :: Wordpress b
           -> [Field s]
           -> StateT s IO Text
           -> WPLens b s
           -> Fill s
wpPrefetch wp extra uri wpLens _m t@(p, tpl) l = do
    Wordpress{..} <- use wpLens
    tagDict <- liftIO $ lookupTaxDict (TaxDictKey "post_tag") wp
    catDict <- liftIO $ lookupTaxDict (TaxDictKey "category") wp
    --wpKeys <- liftIO $ findPrefetchables tagDict catDict t
    --void $ liftIO $ concurrently $ map (cachingGet wp) wpKeys
    Larceny.runTemplate tpl p (prefetchSubs tagDict catDict) l
    Larceny.runTemplate tpl p (wordpressSubs wp extra uri wpLens) l

prefetchSubs tdict cdict =
  fills [ ("wpPosts", wpPostsPrefetch tdict cdict)
        , ("wpPage", wpPagePrefetch tdict cdict) ]

wpPostsPrefetch :: (TaxSpec TagType -> TaxSpecId TagType)
                -> (TaxSpec CatType -> TaxSpecId CatType)
                -> Fill s
wpPostsPrefetch tdict cdict =
  \_ _ _ -> do liftIO $ putStrLn "found a post"
               return ""


wpPagePrefetch :: (TaxSpec TagType -> TaxSpecId TagType)
                -> (TaxSpec CatType -> TaxSpecId CatType)
                -> Fill s
wpPagePrefetch tdict cdict =
  \_ _ _ -> do liftIO $ putStrLn "found a page"
               return ""

{--
findPrefetchables :: (TaxSpec TagType -> TaxSpecId TagType)
                  -> (TaxSpec CatType -> TaxSpecId CatType)
                  -> (Path, Larceny.Template s)
                  -> IO [WPKey]
findPrefetchables tdict cdict tpl = -- wpPosts
  do attrs <- undefined
     rest <- findPrefetchables tdict cdict tpl
     return $ [mkWPKey tdict cdict . parseQueryNode $ attrs] <> rest
findPrefetchables tdict cdict _ = -- wpPage
  case lookup "name" attrs' of
    Nothing -> return []
    Just _ -> do attrs <- undefined
                 return [PageKey . fromJust . lookup "name" $ attrs]
findPrefetchables tdict cdict tpl = -- other tags
  do rest <- mapM (findPrefetchables tdict cdict) children
     return $ concat rest
findPrefetchables _ _ _ = return [] -- text or comment --}

mkWPKey :: (TaxSpec TagType -> TaxSpecId TagType)
    -> (TaxSpec CatType -> TaxSpecId CatType)
    -> WPQuery
    -> WPKey
mkWPKey tagDict catDict WPPostsQuery{..} =
  let page = if qpage < 1 then 1 else qpage
      offset = qnum * (page - 1) + qoffset
      tags = map tagDict $ unTaxSpecList qtags
      cats = map catDict $ unTaxSpecList qcats
  in PostsKey (Set.fromList $ [ NumFilter qnum , OffsetFilter offset]
               ++ map TagFilter tags ++ map CatFilter cats ++ userFilter quser)
  where userFilter Nothing = []
        userFilter (Just u) = [UserFilter u]

parsePermalink :: Text -> Maybe (Text, Text, Text)
parsePermalink = either (const Nothing) Just . A.parseOnly parser . T.reverse
  where parser = do _ <- A.option ' ' (A.char '/')
                    guls <- A.many1 (A.letter <|> A.char '-')
                    _ <- A.char '/'
                    htnom <- A.count 2 A.digit
                    _ <- A.char '/'
                    raey <- A.count 4 A.digit
                    _ <- A.char '/'
                    return (T.reverse $ T.pack raey
                           ,T.reverse $ T.pack htnom
                           ,T.reverse $ T.pack guls)

wpGetPost :: (MonadState s m, MonadIO m) => WPLens b s -> WPKey -> m (Maybe Object)
wpGetPost wpLens wpKey =
  do wp <- use wpLens
     liftIO $ getPost wp wpKey

getPost :: Wordpress b -> WPKey -> IO (Maybe Object)
getPost Wordpress{..} wpKey = decodePost <$> cachingGetRetry wpKey
  where decodePost :: Text -> Maybe Object
        decodePost t =
          do post' <- decodeJson t
             case post' of
              Just (post:_) -> Just post
              _ -> Nothing

transformName :: Text -> Text
transformName = T.append "wp" . snd . T.foldl f (True, "")
  where f (True, rest) next = (False, T.snoc rest (toUpper next))
        f (False, rest) '_' = (True, rest)
        f (False, rest) '-' = (True, rest)
        f (False, rest) next = (False, T.snoc rest next)

-- Move this into Init.hs (should retrieve from Wordpress data structure)
addPostIds :: (MonadState s m, MonadIO m) => WPLens b s -> [Int] -> m ()
addPostIds wpLens ids =
  do w@Wordpress{..} <- use wpLens
     assign wpLens
            w{requestPostSet = (`IntSet.union` IntSet.fromList ids) <$> requestPostSet }
