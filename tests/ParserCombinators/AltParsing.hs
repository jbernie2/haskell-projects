{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

type NumberOrString = Either Integer String

a :: String
a = "blah"

b :: String
b = "123"

c :: String
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf "\n")
  v <-    (Left <$> integer)
       <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

main :: IO ()
main = do
  let p f i = parseString f mempty i
  print $ p (some parseNos) eitherOr

  --print $ p (some letter) a
  --print $ p integer b

  --print $ p parseNos a
  --print $ p parseNos b

  --print $ p (many parseNos) c
  --print $ p (some parseNos) c
