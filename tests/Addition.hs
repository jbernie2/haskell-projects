module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  runHspec
  runQuickCheck

runHspec :: IO ()
runHspec = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 :: Int) + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 :: Int) + 2  `shouldBe` 4
    it "x+ 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

runQuickCheck :: IO ()
runQuickCheck = do
  quickCheck prop_additionGreater

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x


