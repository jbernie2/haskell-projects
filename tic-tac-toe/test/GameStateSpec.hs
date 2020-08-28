module GameStateSpec where

import Test.Hspec

import qualified GameState as GS
import GameState (GameState(..), Player(..), Move(..), Space(..))

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "checking for win conditions" $ do
    it "fails for an empty board" $ do
      GS.someoneHasWon GS.init `shouldBe` False

    it "fails for a non winning sequence" $ do
      GS.someoneHasWon nonWinningBoard `shouldBe` False

    it "passes for a winning sequence" $ do
      GS.someoneHasWon winningBoard `shouldBe` True

nonWinningBoard :: GameState
nonWinningBoard =
  GS.init { moves = nonWinningMoves }

nonWinningMoves :: [Move]
nonWinningMoves =
  [ Move {player = PlayerOne, space = TopLeft}
  , Move {player = PlayerOne, space = TopCenter}
  , Move {player = PlayerTwo, space = TopRight}
  ]


winningBoard :: GameState
winningBoard =
  GS.init { moves = winningMoves }

winningMoves :: [Move]
winningMoves =
  [ Move {player = PlayerOne, space = TopLeft}
  , Move {player = PlayerOne, space = Center}
  , Move {player = PlayerOne, space = BottomRight}
  ]

