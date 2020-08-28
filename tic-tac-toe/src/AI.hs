module AI where

import Data.List ((\\))
import Safe (lastMay)

import qualified GameState as GameState
import GameState (GameState(..), Player(..), Move(..))

nextMove :: GameState -> Player -> Maybe Int
nextMove gameState aiPlayer =
  fmap 
    GameState.moveToInt
    (firstNewMove $ 
      diffMoves
        gameState
        ( minimax
            (scoreState aiPlayer)
            gameState
        )
    )
 where

  diffMoves :: GameState -> GameState -> [Move]
  diffMoves gs gs' = (moves gs') \\ (moves gs)

  firstNewMove :: [Move] -> Maybe Move
  firstNewMove = lastMay

nextPossibleMoves :: GameState -> [GameState]
nextPossibleMoves gameState =
  fmap 
    (\m -> GameState.update (Just m) gameState)
    (nextMoves gameState)
 where
  nextMoves :: GameState -> [Move]
  nextMoves gameState' =
    fmap
      (\s -> Move {space = s, player = playerTurn gameState'} )
      (GameState.allSpaces \\ (space <$> moves gameState'))

scoreState :: Player -> GameState -> Int
scoreState aiPlayer gameState =
  case aiPlayer of
    PlayerOne ->
      if GameState.playerTwoHasWinningMoves gameState then
        -1
      else if GameState.playerOneHasWinningMoves gameState then
        1
      else
        0
    PlayerTwo ->
      if GameState.playerOneHasWinningMoves gameState then
        -1
      else if GameState.playerTwoHasWinningMoves gameState then
        1
      else
        0

minimax
  :: (GameState -> Int)
  -> GameState
  -> GameState
minimax scoreFunc gameState =
  snd $ minimaxMax scoreFunc gameState

minimaxMax
  :: (GameState -> Int)
  -> GameState
  -> (Int, GameState)
minimaxMax scoreFunc gameState =
  if GameState.gameIsOver gameState then
    (scoreFunc gameState, gameState)
  else
    foldl
      (\(bestScore, bestState) (score, state) -> 
        if bestScore < score then
          (score, state)
        else
          (bestScore, bestState)
      )
      (minBound, gameState)
      (fmap (minimaxMin scoreFunc) (nextPossibleMoves gameState))
  
minimaxMin
  :: (GameState -> Int)
  -> GameState
  -> (Int, GameState)
minimaxMin scoreFunc gameState =
  if GameState.gameIsOver gameState then
    (scoreFunc gameState, gameState)
  else
    foldl
      (\(bestScore, bestState) (score, state) -> 
        if bestScore > score then
          (score, state)
        else
          (bestScore, bestState)
      )
      (maxBound, gameState)
      (fmap (minimaxMax scoreFunc) (nextPossibleMoves gameState))

  
