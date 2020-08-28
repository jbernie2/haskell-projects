module LongestStreakSpec where

import Test.Hspec

--import qualified LongestStreak as LS

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "foo" $ do
    True `shouldBe` True
  --it "can find the longest sequence of consecitively increasing or decreasing numbers" $ do
    --LS.ls [1,2,2,2,3] `shouldBe` 5
  --it "can find the longest sequence of consecitively increasing or decreasing numbers" $ do
    --LS.ls [1,2,3,2] `shouldBe` 3
  --it "can find the longest sequence of consecitively increasing or decreasing numbers" $ do
    --LS.ls [3,2,1,3,3] `shouldBe` 3
  --it "can find the longest sequence of consecitively increasing or decreasing numbers" $ do
    --LS.ls [1,2,3] `shouldBe` 3
