module Cipher where

import Data.Char

caesar :: Int -> [Char] -> [Char]
caesar cipher msg = fmap (rotateChar cipher) msg 

uncaesar :: Int -> [Char] -> [Char]
uncaesar cipher msg = fmap (rotateChar (26 - cipher)) msg  

rotateChar :: Int -> Char -> Char
rotateChar cipher x
  | x >= 'A' && x <= 'Z' = rotate upperBase x
  | x >= 'a' && x <= 'z' = rotate lowerBase x
  where
    upperBase = ord 'A'
    lowerBase = ord 'a'
    rotate :: Int -> Char -> Char
    rotate base char =
      chr $ (((ord char) - base + cipher) `mod` 26) + base

