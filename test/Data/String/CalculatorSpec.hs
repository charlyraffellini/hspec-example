module Data.String.CalculatorSpec (spec) where

import Test.Hspec
import Data.Char
import Parser.Calculator


spec :: Spec
spec = do
  describe "Calculator Parser" $ do
    it "some calculation" $ do
      let actual = eval $ run "35+10"
      actual `shouldBe` 45
    it "some Mul calculation" $ do
        let actual = eval $ run "35*10"
        actual `shouldBe` 350
