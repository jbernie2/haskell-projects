module Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a
  = Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure a = Success' a
  Failure' e <*> Failure' e' = Failure' $ e <> e'
  Failure' e <*> Success' _  = Failure' e
  Success' _ <*> Failure' e  = Failure' e
  Success' f <*> Success' a  = fmap f (Success' a)
    
instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency
                [ (1, Success' <$> arbitrary)
                , (1, Failure' <$> arbitrary)
                ]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq


main :: IO ()
main =
  let
    trigger :: Validation String (String, Int, String)
    trigger = undefined
  in
  quickBatch $ applicative trigger

