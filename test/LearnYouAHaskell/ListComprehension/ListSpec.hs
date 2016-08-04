module LearnYouAHaskell.ListComprehension.ListSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "List" $ do
    it "[map | biding, condition]" $ do
	  [x*2 | x <- [1..10], x*2 >= 12] `shouldBe` [12,14,16,18,20]
    it "multiple bindings produce all combinations" $ do
	  [ x*y | x <- [2,5,10], y <- [8,10,11]] `shouldBe` [16,20,22,40,50,55,80,100,110]
    it "multiple binding with condition" $ do
	  [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50] `shouldBe` [55,80,100,110]
    it "final example" $ do
	  let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
	  [[ x | x <- xs, even x ] | xs <- xxs]  `shouldBe` [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
