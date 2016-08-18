module Data.String.Monadic2Spec (spec) where

import Test.Hspec
import Parser.Monadic2

simpleParser :: Parser Char
simpleParser = Parser $ \s -> [(head s, tail s)]

spec :: Spec
spec = do
  describe "Monadic2" $ do
    it "string" $ do
      let actual = parse simpleParser "aListOfChar"
      actual `shouldBe` [('a', "ListOfChar")]
