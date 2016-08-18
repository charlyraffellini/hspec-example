module Data.String.Monadic2Spec (spec) where

import Test.Hspec
import Parser.Monadic2

spec :: Spec
spec = do
  describe "Monadic2" $ do
    it "simple parse" $ do
      let simpleParser = Parser $ \s -> [(head s, tail s)]
      let actual = parse simpleParser "aListOfChar"
      actual `shouldBe` [('a', "ListOfChar")]
