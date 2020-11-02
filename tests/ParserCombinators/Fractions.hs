module ParserCombinators.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction :: String
badFraction = "1/0"

alsoBad :: String
alsoBad = "10"

shouldWork :: String
shouldWork = "1/2"

shouldAlsoWork :: String
shouldAlsoWork = "2/1"

type DecimalOrFraction = Either Double Rational

parseDecimalOrFraction :: Parser DecimalOrFraction
parseDecimalOrFraction =
      (Left <$> try parseDecimal)
  <|> (Right <$> try parseFraction)

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = double

main :: IO ()
main = do
  --let parseFraction' =
        --parseString parseFraction mempty

  let parseDecimalOrFraction' =
        parseString parseDecimalOrFraction mempty

  --print $ parseFraction' badFraction
  --print $ parseFraction' alsoBad

  --print $ parseFraction' shouldWork
  --print $ parseFraction' shouldAlsoWork

  print $ parseDecimalOrFraction' "5/4"
  print $ parseDecimalOrFraction' "1.4"


