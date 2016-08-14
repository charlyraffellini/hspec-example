module Data.String.Monadic2Spec (spec) where

import Test.Hspec
import Parser.Monadic2

p :: Parser Char
p = Parser $ \s -> [('a',s)]

spec :: Spec
spec = do
  describe "Monadic2" $ do
    it "string" $ do
      let actual = parse p "aListOfChar"
      actual `shouldBe` [('a', "aListOfChar")]
