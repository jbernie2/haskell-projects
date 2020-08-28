module Main where

import System.Exit (exitSuccess)

import qualified GameState as GameState
import GameState (GameState(..), Player(..))
import qualified PlayerAgent as PlayerAgent

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe"
  putStrLn "Moves are entered via the numbers 0 - 8"
  putStrLn "0 being the top left, 1 being top center and so on, with 8 being the nottom right"
  gameLoop GameState.init

gameLoop :: GameState -> IO ()
gameLoop gameState = do
  putStrLn $ show gameState
  if GameState.gameIsOver gameState then
    exitGame gameState
  else do
    move <- PlayerAgent.nextMove gameState
    gameLoop (GameState.update (GameState.moveFromInt gameState move) gameState)
    
exitGame :: GameState -> IO ()
exitGame gameState = do
  case GameState.winner gameState of
    Just PlayerOne ->
      putStrLn "Player One Wins!"
    Just PlayerTwo ->
      putStrLn "Player Two Wins!"
    Nothing ->
      putStrLn "It's a draw!"

  putStrLn "Thanks for playing!"
  exitSuccess
