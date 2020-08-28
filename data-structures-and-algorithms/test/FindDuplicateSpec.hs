module FindDuplicateSpec where

import Test.Hspec

import qualified Data.Map as Map

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can find the duplicate element in a list if there is one" $ do
    fd [1,2,3,2,4,5,6] `shouldBe` ([2] :: [Int])
  it "can find the duplicate elements in a list" $ do
    fd [1,2,3,2,4,5,1,6] `shouldBe` ([1,2] :: [Int])
  it "returns Nothing if there are no duplicates" $ do
    fd ([1,2,3,4,5,6] :: [Int]) `shouldBe` []

fd :: Ord a => [a] -> [a]
fd xs =
  fmap fst $ filter 
    (\(_, v) -> v > (1 :: Int) ) 
    ( Map.toList $
        Map.fromListWith (+) (zip xs (repeat 1))
    )

