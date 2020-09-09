module BoxPrintSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can box print" $ do
    boxPrint ([1, 2, 3, 4, 5] :: [Int])
    True `shouldBe` True

boxPrint :: Show a => [a] -> IO ()
boxPrint xs = boxPrint' 0 (length xs) xs

boxPrint' :: Show a => Int -> Int -> [a] -> IO ()
boxPrint' _ _ [] = pure()
boxPrint' curr end (x : xs)
  | curr == end = pure ()
  | otherwise = do
      putStr $ foldMap show (x : xs)
      putStrLn ""
      boxPrint' (curr + 1) end (xs <> [x])

