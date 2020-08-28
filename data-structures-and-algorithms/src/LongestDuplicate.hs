module LongestDuplicate where

ld :: [Int] -> Int
ld [] = 0
ld (i:is) = maximum $ ld' i is [1]

ld' :: Int -> [Int] -> [Int] -> [Int]
ld' i1 (i2 : is) (r : rs) 
  | i1 == i2 = ld' i2 is (r+1 : rs)
  | i1 /= i2 = ld' i2 is ( 1 : r : rs)
ld' _ [] rs = rs
ld' _ _ [] = []
ld' _ (_:_) (_:_) = []
