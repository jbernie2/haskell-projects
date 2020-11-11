module MainSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can have tests that pass" $ do
    True `shouldBe` True
