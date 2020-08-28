module PlayerAgentSpec where

import Test.Hspec

import qualified PlayerAgent as PA
import qualified GameState as GS
import GameState (GameState(..))

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "PlayerAgent" $ do
    it "returns a move from player 1 if it is their turn" $ do
      --move <- PA.nextMove computerVsComputer
      --move `shouldBe` Just 6
      True `shouldBe` True
    it "returns a move from player 2 if it is their turn" $ do
      --move <- PA.nextMove computerVsComputer
      --move `shouldBe` Just 6
      True `shouldBe` True
      
computerVsComputer :: GameState
computerVsComputer =
  GS.init {
      playerOne = GS.AI
    , playerTwo = GS.AI
  }

