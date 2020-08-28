module SumToZeroSpec where

import Test.Hspec

import qualified Data.Set as Set
import Data.Maybe (listToMaybe)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can find two numbers in an array that sum to zero" $ do
    sumToZero [-5,1,2,3,2,4,5,6] `shouldBe` (Just (-5,5))
  it "retuns nothing if there are no such numbers" $ do
    sumToZero [1,2,3,2,4,5,6] `shouldBe` Nothing

sumToZero :: [Int] -> Maybe (Int, Int)
sumToZero xs =
  listToMaybe $ foldr 
    (\x zs ->
      if Set.member (-1 * x) numSet then
        (x, -1 * x) : zs
      else
        zs
    )
    []
    xs
 where
  numSet = Set.fromList xs
    
