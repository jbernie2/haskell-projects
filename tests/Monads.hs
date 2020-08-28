module Monads where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import TestDataTypes

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (pure . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= (\a -> l1 (f a) mb)

ap :: Monad m => m a -> m (a -> b) -> m b
ap = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (a : as) f = f a >>= (\b -> ((:) b) <$> (meh as f))

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id


newtype Id a = Id a
  deriving (Eq, Ord, Show)

instance Functor Id where
  fmap f (Id a) = Id $ f a

instance Applicative Id where
  pure = Id
  Id f <*> Id a  = Id $ f a

instance Monad Id where
  return = pure
  Id a >>= f = f a

instance Arbitrary a => Arbitrary (Id a) where
  arbitrary = Id <$> arbitrary

instance Eq a => EqProp (Id a) where
  (=-=) = eq

testNope :: IO ()
testNope =
  let
    trigger :: Nope (String, Int, String)
    trigger = undefined
  in
  quickBatch $ monad trigger

testBahEither :: IO ()
testBahEither = do
  quickBatch $ monad trigger
 where
  trigger :: BahEither String (Int, Int, Int)
  trigger = undefined

testId :: IO ()
testId = do
  quickBatch $ monad trigger
 where
  trigger :: Id (Int, String, Int)
  trigger = undefined

testList :: IO ()
testList = do
  quickBatch $ monad trigger
 where
  trigger :: List (Int, String, Int)
  trigger = undefined

main :: IO ()
main = do
  testNope
  testBahEither
  testId
  testList

