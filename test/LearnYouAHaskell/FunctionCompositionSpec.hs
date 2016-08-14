module LearnYouAHaskell.FunctionCompositionSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "functions composition " $ do
    it "should" $ do
	  let value = sum . replicate 5 . max 6.7 $ 8.9
	  let expec = sum (replicate 5 (max 6.7 8.9))
	  value `shouldBe` expec
