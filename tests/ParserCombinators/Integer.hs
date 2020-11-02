module ParserCombinators.Integer where

import Control.Applicative
import Text.Trifecta
import Test.Hspec
import Data.Char (digitToInt)

parseDigit :: Parser Char
parseDigit = oneOf "1234567890" <?> "digit (0-9)"

data Sign = POS | NEG
base10Integer' :: Parser Integer
base10Integer' = do
  sign <- (try ((char '-') >> return NEG) ) <|> return POS
  case sign of
    NEG ->
      (* (-1)) <$> base10Integer
    POS ->
      base10Integer

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit <?> "integer"
  return $
    foldl (\i d -> i*10 + (toInteger $ digitToInt d)) 0 digits

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Integer Parsing" $ do
    it "can parse a digit" $ do
      let m  = parseString parseDigit mempty "3"
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just '3')
      
    it "can parse an integer" $ do
      let m  = parseString base10Integer' mempty "30"
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just 30)

    it "fails on non-integers" $ do
      let m  = parseString base10Integer' mempty "abc"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Nothing

    it "parses initial integer part of input" $ do
      let m  = parseString base10Integer' mempty "123abc"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just 123

    it "can parse negative numbers" $ do
      let m  = parseString base10Integer' mempty "-123abc"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (-123)


