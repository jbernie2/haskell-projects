module Main where

import System.IO (BufferMode(NoBuffering)
                 ,hSetBuffering
                 ,stdout
                 )

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson
  :: Name
  -> Age
  -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age 
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise = 
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age
 
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStr "Name: "
  name <- getLine
  putStr "Age: "
  age <- read <$> getLine
  case mkPerson name age of
    Right p ->
      putStrLn $ "Yay! Successfully got a person: " ++ show p
    Left err ->
      putStrLn $ "Oops something went wrong: " ++ show err
  return ()
