module Data.String.Monadic1Spec (spec) where

import Test.Hspec
import Parser.Monadic1

spec :: Spec
spec = do
  describe "Monadic1" $ do
    it "zero" $ do
	  zero "a" `shouldBe` []
    it "result" $ do
	  result "a" "abc" `shouldBe` [("a","abc")]
    it "item" $ do
	  item "abc" `shouldBe` [('a',"bc")]
