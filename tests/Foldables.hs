module Foldables where

import Test.Hspec

import Data.Monoid

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "sum" $ do
    it "works on ints" $ do
      sum' [1,2,3,4] `shouldBe` (10 :: Int)
    it "works on Maybe" $ do
      sum' (Just 1) `shouldBe` (1 :: Int)

  describe "product" $ do
    it "works on ints" $ do
      product' [1,2,3,4] `shouldBe` (24 :: Int)
    it "works on Maybe" $ do
      product' (Just 1) `shouldBe` (1 :: Int)

  describe "elem" $ do
    it "works on ints" $ do
      elem' (1 :: Int) [1,2,3,4] `shouldBe` True
    it "works on Maybe" $ do
      elem' (1 ::Int) (Just 1) `shouldBe` True
    it "works on Maybe" $ do
      elem' (2 :: Int) (Just 1) `shouldBe` False
    it "works on Maybe" $ do
      elem' (2 :: Int) Nothing `shouldBe` False

  describe "minimum" $ do
    it "works on ints" $ do
      minimum' [1,2,3,4] `shouldBe` Just (1 :: Int)
    it "works on ints" $ do
      minimum' ([] :: [Int]) `shouldBe` Nothing 
    it "works on Maybe" $ do
      minimum' (Just 'a') `shouldBe` Just 'a'

  describe "null" $ do
    it "works on lists" $ do
      null' ([] :: [Int]) `shouldBe` True
    it "works on lists" $ do
      null' ([1,2,3] :: [Int]) `shouldBe` False
    it "works on Maybe" $ do
      null' Nothing `shouldBe` True
    it "works on Maybe" $ do
      null' (Just 'a') `shouldBe` False

  describe "length" $ do
    it "works on lists" $ do
      length' ([] :: [Int]) `shouldBe` 0
    it "works on lists" $ do
      length' ([1,2,3] :: [Int]) `shouldBe` 3
    it "works on Maybe" $ do
      length' Nothing `shouldBe` 0
    it "works on Maybe" $ do
      length' (Just 'a') `shouldBe` 1

  describe "toList" $ do
    it "works on List" $ do
      toList' ([1,2,3] :: [Int]) `shouldBe` [1,2,3]
    it "works on Maybe" $ do
      toList' (Nothing :: Maybe Int) `shouldBe` []
    it "works on Maybe" $ do
      toList' (Just 'a') `shouldBe` ['a']
    it "works on Either" $ do
      toList' (Left "hello" :: Either String Int) `shouldBe` []
    it "works on Either" $ do
      toList' (Right 'a') `shouldBe` ['a']

  describe "fold" $ do
    it "works on List" $ do
      fold' (Sum <$> [1,2,3]) `shouldBe` (Sum (6 :: Int))
    it "works on Maybe" $ do
      fold' (Just (Product 3)) `shouldBe` (Product (3 :: Int))

  describe "foldMap" $ do
    it "works on List" $ do
      foldMap' Sum [1,2,3] `shouldBe` (Sum (6 :: Int))
    it "works on Maybe" $ do
      foldMap' Product (Just (3 :: Int)) `shouldBe` (Product (3 :: Int))

  describe "filterF" $ do
    it "filters values from a Foldable instance" $ do
      filterF ((<) 2) [1,2,3,4] `shouldBe` (First (Just (3 :: Int)))


sum' :: (Foldable t, Num a) => t a -> a
sum' t = getSum $ foldMap Sum t


product' :: (Foldable t, Num a) => t a -> a
product' t = getProduct $ foldMap Product t


elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (Any . ((==) x)) xs


minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs =
  foldr 
    (\x mAcc -> 
      case mAcc of
        Just acc -> Just $ min x acc 
        Nothing -> Just $ x
    )
    Nothing
    xs


null' :: (Foldable t) => t a -> Bool
null' t = length' t == 0


length' :: (Foldable t) => t a -> Int
length' t = foldr (\_ acc -> acc + 1) 0 t


toList' :: (Foldable t) => t a -> [a]
toList' t = foldr (\x l -> x : l) [] t


fold' :: (Foldable t, Monoid m) => t m -> m
fold' t = foldMap id t


foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f t = foldr (\x acc -> f x <> acc) mempty t 

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

filterF
  :: ( Applicative f
     , Foldable t
     , Monoid (f a))
  => (a -> Bool)
  -> t a
  -> f a
filterF fn t = foldMap (\x -> if fn x then pure x else mempty) t
