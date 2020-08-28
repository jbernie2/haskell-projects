module Functors where

import Test.QuickCheck
import TestDataTypes

main :: IO ()
main = do
  runQuickCheck

runQuickCheck :: IO ()
runQuickCheck = do
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck (functorCompose show ( <> "hello" ) :: Identity Int -> Bool)

  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose show ( <> "hello" ) :: Pair Int -> Bool)

  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck (functorCompose show ( <> "hello" ) :: Two String Int -> Bool)

  quickCheck (functorIdentity :: Three Bool String Int -> Bool)
  quickCheck (functorCompose show ( <> "hello" ) :: Three Bool String Int -> Bool)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose
  :: ( Eq (f c)
     , Functor f
     )
  => (a -> b)
  -> (b -> c)
  -> f a
  -> Bool
functorCompose f g x = (fmap (g . f) x) == (fmap g $ fmap f x)


