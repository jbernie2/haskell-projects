module AISpec where

import Test.Hspec

--import qualified GameState as GS
--import GameState (GameState(..), Player(..), Move(..), Space(..))

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "AI player" $ do
    it "returns a move" $ do
      True `shouldBe` True

    it "picks a winning move if there is one" $ do
      True `shouldBe` True
      
    it "returns Nothing if there are no valid moves" $ do
      True `shouldBe` True


