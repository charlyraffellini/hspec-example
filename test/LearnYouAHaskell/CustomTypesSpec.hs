module LearnYouAHaskell.CustomTypesSpec (spec) where

import Test.Hspec


data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

spec :: Spec
spec = do
  describe "let's remember types" $ do
    it "a Person data type" $ do
	  let pepe = Person "pepe" "sarasa" 1 1.2 "12345" "aFlavor"
	  flavor pepe `shouldBe` "aFlavor"
