module LearnYouAHaskell.FunctionApplicationSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "functions applications " $ do
    it "help us to eliminate parentheses" $ do
	  let value = sum $ filter (> 10) $ map (*2) [2..10]
	  let expec = sum (filter (> 10) (map (*2) [2..10]))
	  value `shouldBe` expec
    it "is another function like everything in haskell" $ do
	  let actual = map ($ 3) [(4+), (10*), (^2), sqrt]
	  actual `shouldBe` [7.0,30.0,9.0,1.7320508075688772]
