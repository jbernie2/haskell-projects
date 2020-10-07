module States.RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import States.RandomExample

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN l g = go l 0 0 g
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go limit sum' count gen
      | sum' >= limit = count
      | otherwise =
        let (die, newGen) = randomR (1, 6) gen
        in go limit (sum' + die) (count + 1) newGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged l g = go l 0 (0, []) g
  where
    go :: Int -> Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go limit sum' (count, rolls) gen
      | sum' >= limit = (count, rolls)
      | otherwise =
        let (die, newGen) = randomR (1, 6) gen
        in go limit (sum' + die) (count + 1, (intToDie die) : rolls) newGen

