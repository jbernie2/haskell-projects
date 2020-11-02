module ParserCombinators.LearnParsers where

import Text.Trifecta
import Control.Applicative ((<|>))

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testEofParse :: Parser Char -> IO ()
testEofParse p =
  print $ parseString (p >> eof) mempty "12"

p123 :: String -> IO ()
p123 s = 
  print $ parseString 
    (string "123" <|> string "12" <|> string "1")
    mempty 
    s

p123' :: String -> IO ()
p123' s = 
  print $ parseString 
    (string' "123" <|> string' "12" <|> string' "1")
    mempty 
    s

string' :: String -> Parser String
string' (x : xs) = char x >> string' xs >> return (x : xs)
string' []       = return []
  

pNL :: String -> IO ()
pNL s =
  putStrLn ('\n' : s )

main :: IO ()
main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one
  
  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'
  

