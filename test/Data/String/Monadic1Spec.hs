module Data.String.Monadic1Spec (spec) where

import Prelude hiding (seq)
import Data.String
import Test.Hspec
import Parser.Monadic1

spec :: Spec
spec = do
  describe "Monadic1" $ do
    it "zero" $ do
	  zero "a" `shouldBe` ""
    it "result" $ do
	  result "a" "abc" `shouldBe` [("a","abc")]
    it "item" $ do
	  item "abc" `shouldBe` [('a',"bc")]
    it "seq" $ do
	  let actual = item `seq` item
	  actual "abc" `shouldBe` [(('a','b'),"c")]
    it "sat -> bind" $ do
	  let isWhatIWant = ((==) 'g')
	  sat isWhatIWant "gafghigg" `shouldBe` [('g',"afghigg")]
