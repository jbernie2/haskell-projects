module LongestStreak where

ls :: [Int] -> Int
ls [] = 0
ls (i:is) = maximum $ ls' i is [1]

ls' :: Int -> [Int] -> [Int] -> [Int]
ls' i1 (i2 : is) (r : rs) 
  | i1 == i2 = ls' i2 is (r+1 : rs)
  | i1 /= i2 = ls' i2 is ( 1 : r : rs)
ls' _ [] rs = rs
ls' _ _ [] = []
ls' _ (_:_) (_:_) = []
