module LongestDuplicateSpec where

import Test.Hspec

import qualified LongestDuplicate as LD

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can find the length of the longest repeated number sequence" $ do
    LD.ld [1,2,2,2,3] `shouldBe` 3
  it "can find the length of the longest repeated number sequence" $ do
    LD.ld [1,1,1,1,2,2,2,3] `shouldBe` 4
  it "can find the length of the longest repeated number sequence" $ do
    LD.ld [3,1,2,3,3] `shouldBe` 2
  it "can find the length of the longest repeated number sequence" $ do
    LD.ld [1,2,3] `shouldBe` 1
