module MismatchedParensSpec where

import Test.Hspec

import qualified MismatchedParens as MP

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can find the number of mis-matched parenthesis" $ do
    MP.mp "()" `shouldBe` 0
  it "can find the number of mis-matched parenthesis" $ do
    MP.mp ")()" `shouldBe` 1
  it "can find the number of mis-matched parenthesis" $ do
    MP.mp ")(" `shouldBe` 2
  it "can find the number of mis-matched parenthesis" $ do
    MP.mp "))(()" `shouldBe` 3
  it "can find the number of mis-matched parenthesis" $ do
    MP.mp "))(()(()" `shouldBe` 4


