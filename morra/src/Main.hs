module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Read (readMaybe)
import Data.List (find)
import System.Random

data GameState = GameState
  { player1 :: Player
  , player2 :: Player
  }

data Player = Player
  { name :: String
  , parity :: Parity
  , playerType :: PlayerType
  , guess :: Guess
  , moves :: [Guess]
  }

data PlayerType
  = Computer
  | Human
  deriving (Eq, Show)

data Parity
  = Even
  | Odd 
  deriving (Eq, Show)

data Guess
  = One
  | Two
  deriving (Eq, Show)

type GameStateT = StateT GameState IO ()


play' :: GameStateT
play' = do
  gameState <- get
  gameState' <- lift $ play gameState
  modify (\_ -> gameState')


play :: GameState -> IO GameState
play gameState = do
  p1Input <- getPlayerInput gameState (player1 gameState)
  _ <- interstitialScreen gameState
  p2Input <- getPlayerInput gameState (player2 gameState)
  let updatedGameState = gameState 
        { player1 = (player1 gameState)
                      { guess = p1Input 
                      , moves = p1Input : (moves $ player1 gameState)
                      }
        , player2 = (player2 gameState)
                      { guess = p2Input 
                      , moves = p2Input : (moves $ player2 gameState)
                      }
        }
  putStrLn (showWinner updatedGameState)
  return updatedGameState
  
interstitialScreen :: GameState -> IO ()
interstitialScreen gameState =
  if playerType (player1 gameState) == Human &&
     playerType (player2 gameState) == Human then
    putStrLn $ take 100 (repeat '\n')
  else
    pure ()

showSetup :: GameState -> String
showSetup gameState =
     "-- P1 is a " <> (show . playerType $ player1 gameState) <> "\n"
  <> "-- P2 is a " <> (show . playerType $ player2 gameState) <> "\n"
  <> "P1 is " <> (show . parity $ player1 gameState) <> ", "
  <> "P2 is " <> (show . parity $ player2 gameState)
  
getPlayerInput :: GameState -> Player -> IO Guess
getPlayerInput gameState player =
  case playerType player of
    Human ->
      getHumanPlayerInput player
    Computer ->
      getComputerPlayerInput gameState player

getHumanPlayerInput :: Player -> IO Guess
getHumanPlayerInput player = do
  _ <- putStrLn ((name player) <> ", please enter 1 or 2")
  i <- (readMaybe <$> getLine) :: IO (Maybe Integer)
  case i of
    Just 1 ->
      return One
    Just 2 ->
      return Two
    _ ->
      getHumanPlayerInput player

getComputerPlayerInput :: GameState -> Player -> IO Guess
getComputerPlayerInput gameState player =
  case smartGuess gameState player of
    Just move ->
      return move
    Nothing ->
      dumbGuess

dumbGuess :: IO Guess
dumbGuess = do
  x <- getStdRandom (randomR (False,True)) :: IO Bool
  return $ case x of 
    True ->
      One
    False ->
      Two

smartGuess :: GameState -> Player -> Maybe Guess
smartGuess gameState player =
  case otherPlayerNextMove of
    Just One ->
      case parity player of
        Even ->
          Just One
        Odd ->
          Just Two
    Just Two ->
      case parity player of
        Even ->
          Just Two
        Odd ->
          Just One
    Nothing ->
      Nothing
 
 where
  otherPlayerNextMove :: Maybe Guess
  otherPlayerNextMove =
    case lastTwoMoves otherPlayer of
      Just (m1, m2) ->
        case 
            find
              (\(x1, x2, _) -> m1 == x1 && m2 == x2)
              (allTriples $ moves otherPlayer)
        of
          Just (_, _, m) ->
            Just m
          Nothing ->
            Nothing
      Nothing ->
        Nothing

  otherPlayer :: Player
  otherPlayer =
    if name player /= name (player1 gameState) then
      player1 gameState
    else
      player2 gameState

  lastTwoMoves :: Player -> Maybe (Guess, Guess)
  lastTwoMoves p =
    case (take 2 $ moves p) of
      (m1 : m2 : []) ->
        Just (m1, m2)
      _ ->
        Nothing

  allTriples :: [a] -> [(a,a,a)]
  allTriples []                  = []
  allTriples (_  : [])           = []
  allTriples (_  : _  : [])      = []
  allTriples (x1 : x2 : x3 : xs) = (x1, x2, x3) : allTriples (x2 : x3 : xs)

showWinner :: GameState -> String
showWinner gameState = 
     "P1: " <> (show . guess $ player1 gameState) <> "\n"
  <> "P2: " <> (show . guess $ player2 gameState) <> "\n"
  <> (
    case ((guess $ player1 gameState) == (guess $ player2 gameState)
         , parity $ player1 gameState
         ) of
      (True, Even) ->
        "- P1 wins"
      (False, Odd) ->
        "- P1 wins"
      (True, Odd) ->
        "- P2 wins"
      (False, Even) ->
        "- P2 wins"
  )
  
main :: IO ()
main = do
  initialState <- initialGameState
  _ <- putStrLn (showSetup initialState)
  _ <- evalStateT playForever initialState
  putStrLn "Thanks for playing"
 where
  playForever = play' >> playForever

initialGameState :: IO GameState
initialGameState = do
  (player1Type, player2Type) <- selectPlayerTypes 
  (p1Parity, p2Parity) <- pickParity
  return $ GameState
    { player1 = Player
        { name = "P1"
        , parity = p1Parity
        , playerType = player1Type
        , guess = One
        , moves = []
        }
    , player2 = Player
        { name = "P2"
        , parity = p2Parity
        , playerType = player2Type
        , guess = One
        , moves = []
        }
    }

pickParity :: IO (Parity, Parity)
pickParity = do
  x <- getStdRandom (randomR (False,True)) :: IO Bool
  case x of 
    True ->
      return (Odd, Even)
    False ->
      return (Even, Odd)

selectPlayerTypes :: IO (PlayerType, PlayerType)
selectPlayerTypes =
  (,) <$> selectPlayerType "P1" <*> selectPlayerType "P2"

selectPlayerType :: String -> IO PlayerType
selectPlayerType playerName = do
  putStrLn $ "is " <> playerName <> " a human or computer player?"
  putStrLn "enter 1 for human, 2 for computer" 
  choice <- getLine
  case choice of
    "1" ->
      return Human
    "2" ->
      return Computer
    _ ->
      selectPlayerType playerName


