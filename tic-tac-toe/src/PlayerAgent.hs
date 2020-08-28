module PlayerAgent where

import Text.Read (readMaybe)

import GameState (GameState(..), Player(..), PlayerType(..))
import qualified AI as AI

nextMove :: GameState -> IO (Maybe Int)
nextMove gameState = case playerTurn gameState of
  PlayerOne ->
    case playerOne gameState of
      Human ->
        nextHumanMove
      AI ->
        nextAIMove gameState PlayerOne
  PlayerTwo ->
    case playerTwo gameState of
      Human ->
        nextHumanMove
      AI ->
        nextAIMove gameState PlayerTwo

nextHumanMove :: IO (Maybe Int)
nextHumanMove = readMaybe <$> getLine

nextAIMove :: GameState -> Player -> IO (Maybe Int)
nextAIMove gameState player = pure $ AI.nextMove gameState player

