{-# LANGUAGE OverloadedStrings #-}

module Misc where

import           Control.Monad.State
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Test.Hspec
import           Web.Offset

shouldTransformTo :: Text -> Text -> Spec
shouldTransformTo from to =
  it (T.unpack ("should convert " <> from <> " to " <> to)) $ transformName from `shouldBe` to

tests :: Spec
tests = do
  describe "mergeFields" $ do
    it "should be able to right-bias merge two Field trees" $
      mergeFields [F "ID", F "status"] ([P "status" undefined] :: [Field (StateT () IO Text)])
        `shouldBe` [F "ID", P "status" undefined]
    it "should be able to override nested fields" $
      mergeFields [N "status" [F "foo"]] ([N "status" [P "foo" undefined]] :: [Field (StateT () IO Text)])
        `shouldBe` [N "status" [P "foo" undefined]]
    it "should be able to override nested fields, but not lose unaffected ones" $
      mergeFields [N "status" [F "foo", F "bar"]] ([N "status" [M "foo" []]] :: [Field (StateT () IO Text)])
        `shouldBe` [N "status" [M "foo" [], F "bar"]]
    it "should not change order of fields in left" $
      mergeFields [F "ID", F "status"] ([F "status", F "ID"] :: [Field (StateT () IO Text)])
        `shouldBe` [F "ID", F "status"]
    it "should be able to override nested with flat" $
      mergeFields [N "status" [F "foo", F "bar"]] ([F "status"] :: [Field (StateT () IO Text)])
        `shouldBe` [F "status"]
    it "should be able to override flat with nested" $
      mergeFields ([F "status"] :: [Field (StateT () IO Text)]) [N "status" [F "foo", F "bar"]]
        `shouldBe` [N "status" [F "foo", F "bar"]]
    it "should be able to add to elements nested" $
      mergeFields ([N "featured_image" [N "attachment_meta" [F "standard"]]] :: [Field (StateT () IO Text)])
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
    it "should parse a tag list" $
      attrToTaxSpecList ("tag","foo-bar,baz") `shouldBe` (TaxSpecList "tag" [TaxPlus "foo-bar", TaxPlus "baz"])
    it "should parse a tag list with mixed pluses and minuses" $
      attrToTaxSpecList ("tag","+foo-bar,-baz,-qux") `shouldBe`
        (TaxSpecList "tag" [TaxPlus "foo-bar", TaxMinus "baz", TaxMinus "qux"])
    it "should round trip tag plus" $
      show (attrToTaxSpecList ("tag","+foo-bar")) `shouldBe` "tag: +foo-bar"
    it "should round trip tag minus" $
      show (attrToTaxSpecList ("tag", "-foo-bar")) `shouldBe` "tag: -foo-bar"
    it "should add plus to bare tag plus when round tripping" $
      show (attrToTaxSpecList ("tag", "foo-bar")) `shouldBe` "tag: +foo-bar"
    it "should round trip list" $
      show (attrToTaxSpecList ("tag", "+foo-bar,-baz,-qux")) `shouldBe` "tag: +foo-bar,-baz,-qux"
    it "should add plus to bare tag pluses in list roundtrip" $
      show (attrToTaxSpecList ("tag", "foo-bar,-baz,-qux")) `shouldBe` "tag: +foo-bar,-baz,-qux"
