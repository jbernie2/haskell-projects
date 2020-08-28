module SemigroupsAndMonoids where

import Test.QuickCheck
import Data.Monoid

import TestDataTypes

main :: IO ()
main = do
  runQuickCheck

runQuickCheck :: IO ()
runQuickCheck = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj  -> Bool)

  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  quickCheck (semigroupAssoc :: OrAssoc (Sum Int) String)

  quickCheck (combineAssoc :: CombineAssoc String String)
  quickCheck (combineLeftIdentity :: Combine String String -> String -> Bool)
  quickCheck (combineRightIdentity :: Combine String String -> String -> Bool)

  quickCheck (compAssoc :: CompAssoc String)
  quickCheck (compLeftIdentity :: Comp String -> String -> Bool)
  quickCheck (compRightIdentity :: Comp String -> String -> Bool)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

combineAssoc :: (Eq b, Semigroup b)  => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineAssoc a b c d =
  unCombine (a <> (b <> c)) d == unCombine ((a <> b) <> c) d

compAssoc :: (Eq a)  => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc a b c d =
  unComp (a <> (b <> c)) d == unComp ((a <> b) <> c) d

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

combineLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool 
combineLeftIdentity a b  = unCombine (mempty <> a) b == unCombine a b

combineRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool 
combineRightIdentity a b  = unCombine (a <> mempty) b == unCombine a b

compLeftIdentity :: (Eq a) => Comp a -> a -> Bool 
compLeftIdentity a b = unComp (mempty <> a) b == unComp a b

compRightIdentity :: (Eq a) => Comp a -> a -> Bool 
compRightIdentity a b = unComp (a <> mempty) b == unComp a b


