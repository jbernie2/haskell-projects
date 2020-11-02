
module ParserCombinators.PhoneNumber where

import Control.Applicative
import Text.Trifecta
import Test.Hspec

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber
    NumberingPlanArea
    Exchange
    LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- try (char '1' >> char '-') <|> return ' '
  areaCode <- parseNumberingPlanArea
  exchange <- parseExchange
  lineNo   <- parseLineNumber
  return (PhoneNumber areaCode exchange lineNo)

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea =
  try (char '(' >> (getDigits 3) <* char ')')
  <|> try (getDigits 3)

parseExchange :: Parser Exchange
parseExchange = do
  _ <- try (oneOf " -") <|> return ' '
  getDigits 3

parseLineNumber :: Parser LineNumber
parseLineNumber = do
  _ <- try (oneOf " -") <|> return ' '
  getDigits 4

getDigits :: Int -> Parser Int
getDigits i =
  read <$> count i digit

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "US/Canada PhoneNumber Parsing" $ do
    it "can parse with dashes" $ do
      let m  = parseString parsePhone mempty "123-456-7890"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (PhoneNumber 123 456 7890)

    it "can parse with no spacing" $ do
      let m  = parseString parsePhone mempty "1234567890"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (PhoneNumber 123 456 7890)
      
    it "can parse with parens" $ do
      let m  = parseString parsePhone mempty "(123) 456-7890"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (PhoneNumber 123 456 7890)

    it "can parse with country code and dashes" $ do
      let m  = parseString parsePhone mempty "1-123-456-7890"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (PhoneNumber 123 456 7890)
