module Traversables where

--import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import TestDataTypes

testId :: IO ()
testId = do
  quickBatch $ traversable trigger
 where
  trigger :: Identity (Int, Int, [Int])
  trigger = undefined

testConst :: IO ()
testConst = do
  quickBatch $ traversable trigger
 where
  trigger :: (Constant String) (Int, Int, [Int])
  trigger = undefined

testMaybe :: IO ()
testMaybe = do
  quickBatch $ traversable trigger
 where
  trigger :: Optional (Int, Int, [Int])
  trigger = undefined

testList :: IO ()
testList = do
  quickBatch $ traversable trigger
 where
  trigger :: List (Int, Int, [Int])
  trigger = undefined

testThree :: IO ()
testThree = do
  quickBatch $ traversable trigger
 where
  trigger :: (Three String String) (Int, Int, [Int])
  trigger = undefined

testTwo :: IO ()
testTwo = do
  quickBatch $ traversable trigger
 where
  trigger :: (Two String) (Int, Int, [Int])
  trigger = undefined

testBig :: IO ()
testBig = do
  quickBatch $ traversable trigger
 where
  trigger :: (Big String) (Int, Int, [Int])
  trigger = undefined

testBigger :: IO ()
testBigger = do
  quickBatch $ traversable trigger
 where
  trigger :: (Bigger String) (Int, Int, [Int])
  trigger = undefined

testS :: IO ()
testS = do
  quickBatch $ traversable trigger
 where
  trigger :: (S Maybe) (Int, Int, [Int])
  trigger = undefined

testTree :: IO ()
testTree = do
  quickBatch $ traversable trigger
 where
  trigger :: Tree (Int, Int, [Int])
  trigger = undefined

main :: IO ()
main = do
  testId
  testConst
  testMaybe
  testList
  testThree
  testTwo
  testBig
  testBigger
  testS
  testTree

