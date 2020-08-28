module GameState where

import Data.List (partition, find)
import Data.Maybe (catMaybes)

data GameState = GameState
  { playerTurn :: Player
  , playerOne :: PlayerType
  , playerTwo :: PlayerType
  , moves :: [Move]
  } deriving (Eq, Ord)

instance Show GameState where
  show gameState =
    "Turn: " ++ show (playerTurn gameState) ++ "\n"
    ++ "_______\n"
    ++ "|" ++ (showMove gameState TopLeft)
    ++ "|" ++ (showMove gameState TopCenter)
    ++ "|" ++ (showMove gameState TopRight)
    ++ "|\n"
    ++ "_______\n"
    ++ "|" ++ (showMove gameState CenterLeft)
    ++ "|" ++ (showMove gameState Center)
    ++ "|" ++ (showMove gameState CenterRight)
    ++ "|\n"
    ++ "_______\n"
    ++ "|" ++ (showMove gameState BottomLeft)
    ++ "|" ++ (showMove gameState BottomCenter)
    ++ "|" ++ (showMove gameState BottomRight)
    ++ "|\n"
    ++ "_______"
    
showMove :: GameState -> Space -> String
showMove gameState space' =
  case find (\m -> space m == space') (moves gameState) of
    Just move ->
      playerMark (player move)
    Nothing ->
      " "

playerMark :: Player -> String
playerMark PlayerOne = "O"
playerMark PlayerTwo = "X"
      

data Player = PlayerOne | PlayerTwo
  deriving (Eq, Show, Ord)
data PlayerType = Human | AI
  deriving (Eq, Show, Ord)

data Move = Move
  { space :: Space
  , player :: Player
  }
  deriving (Show, Eq, Ord)

data Space
  = TopLeft
  | TopCenter
  | TopRight
  | CenterLeft
  | Center
  | CenterRight
  | BottomLeft
  | BottomCenter
  | BottomRight 
  deriving (Eq, Show, Ord)

init :: GameState
init = GameState
  { playerTurn = PlayerOne
  , playerOne = Human
  , playerTwo = AI
  , moves = []
  }

gameIsOver :: GameState -> Bool
gameIsOver gameState =
  boardIsFull gameState || someoneHasWon gameState

boardIsFull :: GameState -> Bool
boardIsFull gameState =
  length (moves gameState) == 9

someoneHasWon :: GameState -> Bool
someoneHasWon gameState =
  playerOneHasWinningMoves gameState || 
    playerTwoHasWinningMoves gameState

playerOneHasWinningMoves :: GameState -> Bool
playerOneHasWinningMoves gameState = 
  winningMoves . fst $ playerMoves gameState

playerTwoHasWinningMoves :: GameState -> Bool
playerTwoHasWinningMoves gameState = 
  winningMoves . snd $ playerMoves gameState

playerMoves :: GameState -> ([Move], [Move])
playerMoves gameState =
  (partition
    (\m -> player m == PlayerOne)
    (moves gameState)
  )

winningMoves :: [Move] -> Bool
winningMoves moves' = 
  any id $ map
    (isSubList spaces)
    winningCombinations
 where
  spaces = space <$> moves'
  
winningCombinations :: [[Space]]
winningCombinations =
  [ [TopLeft, TopCenter, TopRight]
  , [CenterLeft, Center, CenterRight]
  , [BottomLeft, BottomCenter, BottomRight]
  , [TopLeft, CenterLeft, BottomLeft]
  , [TopCenter, Center, BottomCenter]
  , [TopRight, CenterRight, BottomRight]
  , [TopRight, Center, BottomLeft]
  , [TopLeft, Center, BottomRight]
  ]

isSubList :: Eq a => [a] -> [a] -> Bool
isSubList [] [] = True
isSubList _ [] = False
isSubList xs (y:[]) = elem y xs
isSubList xs (y:ys) = elem y xs && isSubList xs ys

update :: Maybe Move -> GameState -> GameState
update Nothing gameState = gameState
update (Just move) gameState = 
  if validMove move gameState then
    gameState {
      moves = move : (moves gameState)
    , playerTurn = otherPlayer (playerTurn gameState)
    }
  else
    gameState

validMove :: Move -> GameState -> Bool
validMove move gameState =
  not $ any (\m -> space m == space move) (moves gameState)
  
otherPlayer :: Player -> Player
otherPlayer PlayerOne = PlayerTwo
otherPlayer PlayerTwo = PlayerOne

winner :: GameState -> Maybe Player
winner gameState =
  if hasWinningMoves PlayerOne then
    Just PlayerOne
  else if hasWinningMoves PlayerTwo then
    Just PlayerTwo
  else
    Nothing

 where
  hasWinningMoves :: Player -> Bool
  hasWinningMoves player' =
    winningMoves $ filter (\m -> player m == player') (moves gameState)

moveFromInt :: GameState -> Maybe Int -> Maybe Move
moveFromInt _ Nothing = Nothing
moveFromInt gameState (Just i) =
  case spaceFromInt i of
    Just s -> Just $ Move
      { space = s
      , player = playerTurn gameState  
      }
    Nothing ->
      Nothing

spaceFromInt :: Int -> Maybe Space
spaceFromInt 0 = Just TopLeft
spaceFromInt 1 = Just TopCenter
spaceFromInt 2 = Just TopRight
spaceFromInt 3 = Just CenterLeft
spaceFromInt 4 = Just Center
spaceFromInt 5 = Just CenterRight
spaceFromInt 6 = Just BottomLeft
spaceFromInt 7 = Just BottomCenter
spaceFromInt 8 = Just BottomRight 
spaceFromInt _ = Nothing


moveToInt :: Move -> Int
moveToInt move = spaceToInt (space move)

spaceToInt :: Space -> Int
spaceToInt TopLeft      = 0
spaceToInt TopCenter    = 1
spaceToInt TopRight     = 2
spaceToInt CenterLeft   = 3
spaceToInt Center       = 4
spaceToInt CenterRight  = 5
spaceToInt BottomLeft   = 6
spaceToInt BottomCenter = 7
spaceToInt BottomRight  = 8

allSpaces :: [Space]
allSpaces = catMaybes $ spaceFromInt <$> [0..8]
