module LinkedListSpec where

import Test.Hspec

import qualified LinkedList as LL


main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can create an empty list - O(1)" $ do
    ((LL.toList LL.empty) :: [Int]) `shouldBe` []
  
  it "can have items appended - O(n)" $ do
    LL.toList (LL.append 5 $ (LL.append 4 LL.empty)) `shouldBe` ([4, 5] :: [Int])

  it "can have items pre-pended - O(1)" $ do
    LL.toList (LL.prepend 5 $ (LL.prepend 4 LL.empty)) `shouldBe` ([5, 4] :: [Int])

  it "can have items removed - O(1)" $ do
    (LL.toList <$> LL.removeFirst (LL.fromList [1,2,3])) `shouldBe` ((Just 1, [2,3]) :: (Maybe Int, [Int]))

  it "can be reversed - O(n)" $ do
    (LL.toList . LL.llReverse $ LL.fromList [1,2,3]) `shouldBe` ([3,2,1] :: [Int])
    
