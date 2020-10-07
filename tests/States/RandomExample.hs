module States.RandomExample where

import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1 - 6 integer: " ++ show x

rollDieThreeTimes :: Int -> (Die, Die, Die)
rollDieThreeTimes n = do
  let 
    s = mkStdGen n
    (d1, s1) = randomR (1, 6) s    
    (d2, s2) = randomR (1, 6) s1
    (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

