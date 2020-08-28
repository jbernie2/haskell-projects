module Applicatives where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import TestDataTypes

newtype Identity' a = Identity' a
  deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap fn (Identity' a) = Identity' $ fn a

instance Applicative Identity' where
  pure a = Identity' a
  (Identity' fn) <*> ida = fmap fn ida

newtype Constant' a b
  = Constant' { getConstant' :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
  fmap _ (Constant' a) = Constant' a

instance Monoid a => Applicative (Constant' a) where
  pure _ = Constant' mempty 
  (Constant' a) <*> (Constant' a') = Constant' $ a <> a'

testPair :: IO ()
testPair =
  let
    mkPair :: Pair (String, Int, String)
    mkPair = undefined
  in
  quickBatch $ applicative mkPair

testTwo :: IO ()
testTwo =
  let
    trigger :: Two String (String, Int, String)
    trigger = undefined
  in
  quickBatch $ applicative trigger

testThree :: IO ()
testThree =
  let
    trigger :: Three String [Int] (String, Int, String)
    trigger = undefined
  in
  quickBatch $ applicative trigger

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos l1 l2 l3 = liftA3 (,,) l1 l2 l3

main :: IO ()
main = do
  testPair
  testTwo
  testThree
