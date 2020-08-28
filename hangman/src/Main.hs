module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit ( exitSuccess)
import System.IO (BufferMode(NoBuffering)
                 ,hSetBuffering
                 ,stdout
                 )
import System.Random (randomRIO)

type WordList = [String]
type Guess = Char
type MysteryWord = String
type FilledInWord = [Maybe Char]
type AllGuesses = [Char]
type IncorrectGuesses = [Char]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 15

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
 where
  gameLength w =
    let l = length (w :: String)
    in 
    l >= minWordLength
    && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle MysteryWord FilledInWord AllGuesses IncorrectGuesses

instance Show Puzzle where
  show (Puzzle _ discovered guessed _) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s =
  Puzzle s (fmap (const Nothing) s) [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) c =
  elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) c =
  elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Guess -> Puzzle
fillInCharacter (Puzzle word filledInSoFar alreadyGuessed wg) currentGuess =
  Puzzle word newFilledInSoFar (currentGuess : alreadyGuessed) wg
 where
  newFilledInSoFar = zipWith (zipper currentGuess) word filledInSoFar

  zipper :: Char -> Char -> Maybe Char -> Maybe Char
  zipper guess wordChar guessChar =
    if wordChar == guess
    then Just wordChar
    else guessChar

wrongGuess :: Puzzle -> Guess -> Puzzle
wrongGuess (Puzzle mw fiw ag wrongGuesses) guess =
  Puzzle mw fiw (guess : ag ) (guess : wrongGuesses)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "The character wasn't in the word, try again."
      return (wrongGuess puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ guessedWrong) =
  if (length guessedWrong) > 7 then
    do putStrLn "You lose!"
       putStrLn $
         "the word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn $ "You win! The word is: " ++ word ++ "!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of 
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle


