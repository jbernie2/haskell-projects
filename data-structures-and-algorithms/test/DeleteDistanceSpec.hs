module DeleteDistanceSpec where

import Test.Hspec

import qualified DeleteDistance as DD

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can find the deletion distance between two strings" $ do
    DD.deleteDistance "cat" "bat" `shouldBe` 2
  it "can find the deletion distance between two strings" $ do
    DD.deleteDistance "dog" "frog" `shouldBe` 3
  it "can find the deletion distance between two strings" $ do
    DD.deleteDistance "some" "some" `shouldBe` 0
  it "can find the deletion distance between two strings" $ do
    DD.deleteDistance "some" "thing" `shouldBe` 9
  it "can find the deletion distance between two strings" $ do
    DD.deleteDistance "" "" `shouldBe` 0
  it "can find the deletion distance between two strings" $ do
    DD.deleteDistance "pextreme" "diemxtricemebat" `shouldBe` 9
  it "can find the deletion distance between two strings" $ do
    DD.deleteDistance "ooa" "bab" `shouldBe` 4
  it "can find the deletion distance between two strings" $ do
    DD.deleteDistance "yoba" "ybab" `shouldBe` 2
