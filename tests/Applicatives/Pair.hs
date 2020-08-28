module Pair where

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap fn (Pair a a') = Pair (fn a) (fn a')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

