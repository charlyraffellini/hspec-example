module Data.String.Monadic2Spec (spec) where

import Test.Hspec
import Parser.Monadic2
import Data.Char

spec :: Spec
spec = do
  describe "Monadic2" $ do
    it "is just simple parse" $ do
      let simpleParser = Parser $ \s -> [(head s, tail s)]
      let actual = parse simpleParser "aListOfChar"
      actual `shouldBe` [('a', "ListOfChar")]
    it "is a functor - fmap/2" $ do
      let aParser = Parser (\s -> [(reverse s,"")])
      let finalParser = fmap (map toUpper) aParser
      let actual = parse finalParser "abc"
      actual `shouldBe` [("CBA", "")]
    it "proof the first functor law: fmap id = id" $ do
      let aParser = Parser (\s -> [(reverse s,"")])
      let fmapIdParser = fmap id aParser
      let actual = parse fmapIdParser "abc"
      let expected = parse aParser "abc"
      actual `shouldBe` expected
    it "proof the second functor law: fmap (f . g) = fmap f . fmap g" $ do
      let upperString = map toUpper
      let aLazyParser = Parser (\s -> [(s,"")])
      let fmapUpperReverse = fmap (upperString . reverse) aLazyParser
      let compositeFmaps = ((fmap upperString) . (fmap reverse)) aLazyParser
      let actual = parse fmapUpperReverse "abcd"
      let expected = parse compositeFmaps "abcd"
      actual `shouldBe` expected
    it "is applicative - pure" $ do
      let otherParser = pure "asd"
      let actual = parse otherParser "1234"
      actual `shouldBe` [("asd", "1234")]
    it "is applicative - <*>" $ do
      let container = Parser (\s -> [((map toUpper),s)]) <*> Parser (\s -> [(reverse s,"")])
      let actual = runParser container "asd"
      actual `shouldBe` "DSA"
    it "is monadic - continuation" $ do
      let returnNext c = chr (ord c + 1)
      let first = Parser $ \s -> [(head s, tail s)]
      let transform = Parser $ \s -> [(returnNext $ head s, tail s)]
      let binded = first >>= (\c -> return $ returnNext c)
      let actual = parse binded "aListOfChar"
      actual `shouldBe` [('b', "ListOfChar")]
    it "proof the first monad law - Left identity - return x >>= f is the same as f x" $ do
      let computation =  (\x -> Parser (\y -> [([head y] ++ x,tail y)]))
      let dammThing = return "Stuff" >>= computation
      let actual = parse dammThing "aString"
      actual `shouldBe` parse (computation "Stuff") "aString" -- [("aStuff", "String")]
    it "proof the second monad law - Right identity - m >>= return is just m" $ do
      let firstParser = Parser (\y -> [([head y],tail y)])
      let computed =  firstParser >>= (\x -> return x)
      let actual = parse computed "asd"
      actual `shouldBe` parse firstParser "asd"
    it "proof the third monad law - Associativity" $ do --(m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)
      let computation =  (\x -> Parser (\y -> [([head y] ++ x,tail y)]))
      let otherComputation = (\x -> Parser (\y -> [((map toUpper y) ++ " then " ++ x, "")]))
      let dammThing = (return "Stuff" >>= computation) >>= otherComputation
      let actual = parse dammThing "aString"
      let secondTerm = (return "Stuff") >>= (\x -> (computation x) >>= otherComputation)
      let expected = parse secondTerm "aString"
      actual `shouldBe` expected --[("STRING then aStuff","")]
