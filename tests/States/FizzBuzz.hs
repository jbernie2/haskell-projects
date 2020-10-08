module States.FizzBuzz where

fizzbuzz :: Integer -> String
fizzbuzz i
  | i `mod` 15 == 0 = "FizzBuzz"
  | i `mod` 5  == 0 = "Buzz"
  | i `mod` 3  == 0 = "Fizz"
  | otherwise       = show i

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to
  | from > to = []
  | otherwise = fizzbuzz from : (fizzbuzzFromTo (from + 1) to)

main :: IO ()
main = 
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
