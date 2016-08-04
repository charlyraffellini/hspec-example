module Data.String.StripSpec (main, spec) where

import Test.Hspec
import Data.String.Strip
import Parser.Monadic1

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec = spec1 >> spec2

spec1 :: Spec
spec1 = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
	  strip "\t  foo bar\n" `shouldBe` "foo bar"

spec2 :: Spec
spec2 = do
  describe "Monadic1" $ do
    it "zero" $ do
	  zero "a" `shouldBe` []
    it "result" $ do
	  result "a" "abc" `shouldBe` [("a","abc")]
    it "item" $ do
    item "abc" `shouldBe` [('a',"bc")]
