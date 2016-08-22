module Data.String.Monadic2Spec (spec) where

import Test.Hspec
import Parser.Monadic2
import Data.Char

spec :: Spec
spec = do
  describe "Monadic2" $ do
    it "simple parse" $ do
      let simpleParser = Parser $ \s -> [(head s, tail s)]
      let actual = parse simpleParser "aListOfChar"
      actual `shouldBe` [('a', "ListOfChar")]
    it "continuation" $ do
      let returnNext c = chr (ord c + 1)
      let first = Parser $ \s -> [(head s, tail s)]
      let transform = Parser $ \s -> [(returnNext $ head s, tail s)]
      let binded = first >>= (\c -> return $ returnNext c)
      let actual = parse binded "aListOfChar"
      actual `shouldBe` [('b', "ListOfChar")]
    it "fmap/2" $ do
      let aParser = Parser (\s -> [(reverse s,"")])
      let finalParser = fmap (map toUpper) aParser
      let actual = parse finalParser "abc"
      actual `shouldBe` [("CBA", "")]
    it "first functor law: fmap id = id" $ do
      let aParser = Parser (\s -> [(reverse s,"")])
      let fmapIdParser = fmap id aParser
      let actual = parse fmapIdParser "abc"
      let expected = parse aParser "abc"
      actual `shouldBe` expected
