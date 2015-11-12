{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Misc where

import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Test.Hspec
import           Web.Offset

shouldTransformTo :: Text -> Text -> Spec
shouldTransformTo from to =
  it (T.unpack ("should convert " <> from <> " to " <> to)) $ transformName from `shouldBe` to

-- NOTE(dbp 2014-11-07): We define equality that is 'good enough' for testing.
-- In truth, our definition is wrong because of the functions inside of 'P' variants.
instance (Functor m, Monad m) =>  Eq (Field m) where
  F t1 == F t2 = t1 == t2
  P t1 _ == P t2 _ = t1 == t2
  N t1 n1 == N t2 n2 = t1 == t2 && n1 == n2
  M t1 m1 == M t2 m2 = t1 == t2 && m1 == m2

tests = do
  describe "mergeFields" $ do
    it "should be able to right-bias merge two Field trees" $
      mergeFields [F "ID", F "status"] ([P "status" undefined] :: [Field IO])
        `shouldBe` [F "ID", P "status" undefined]
    it "should be able to override nested fields" $
      mergeFields [N "status" [F "foo"]] ([N "status" [P "foo" undefined]] :: [Field IO])
        `shouldBe` [N "status" [P "foo" undefined]]
    it "should be able to override nested fields, but not lose unaffected ones" $
      mergeFields [N "status" [F "foo", F "bar"]] ([N "status" [M "foo" []]] :: [Field IO])
        `shouldBe` [N "status" [M "foo" [], F "bar"]]
    it "should not change order of fields in left" $
      mergeFields [F "ID", F "status"] ([F "status", F "ID"] :: [Field IO])
        `shouldBe` [F "ID", F "status"]
    it "should be able to override nested with flat" $
      mergeFields [N "status" [F "foo", F "bar"]] ([F "status"] :: [Field IO])
        `shouldBe` [F "status"]
    it "should be able to override flat with nested" $
      mergeFields ([F "status"] :: [Field IO]) [N "status" [F "foo", F "bar"]]
        `shouldBe` [N "status" [F "foo", F "bar"]]
    it "should be able to add to elements nested" $
      mergeFields ([N "featured_image" [N "attachment_meta" [F "standard"]]] :: [Field IO])
                  [N "featured_image" [N "attachment_meta" [F "mag-featured"]]]
        `shouldBe` [N "featured_image" [N "attachment_meta" [F "standard"
                                                            ,F "mag-featured"]]]
  describe "transformName" $ do
    "ID" `shouldTransformTo` "wpID"
    "title" `shouldTransformTo` "wpTitle"
    "post_tag" `shouldTransformTo` "wpPostTag"
    "mag-featured" `shouldTransformTo` "wpMagFeatured"
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
      show (read "+foo-bar" :: TaxSpec TagType) `shouldBe` "+foo-bar"
    it "should round trip tag minus" $
      show (read "-foo-bar" :: TaxSpec TagType) `shouldBe` "-foo-bar"
    it "should add plus to bare tag plus when round tripping" $
      show (read "foo-bar" :: TaxSpec TagType) `shouldBe` "+foo-bar"
    it "should round trip list" $
      show (read "+foo-bar,-baz,-qux" :: TaxSpecList CatType) `shouldBe` "+foo-bar,-baz,-qux"
    it "should add plus to bare tag pluses in list roundtrip" $
      show (read "foo-bar,-baz,-qux" :: TaxSpecList TagType) `shouldBe` "+foo-bar,-baz,-qux"
