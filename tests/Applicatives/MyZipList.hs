module MyZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
   where
    xs' = let (ZipList' l) = xs
          in take 3000 l
    ys' = let (ZipList' l) = ys
          in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat a
  ZipList' fs' <*> ZipList' xs' =
    ZipList' $ zip' fs' xs'
    where
     zip' :: [a -> b] -> [a] -> [b] 
     zip' (f : fs) (x : xs) = f x : zip' fs xs
     zip' [] _ = []
     zip' _ [] = []

main :: IO ()
main =
  let zl :: ZipList' (String, String, String)
      zl = undefined
  in quickBatch $ applicative zl
