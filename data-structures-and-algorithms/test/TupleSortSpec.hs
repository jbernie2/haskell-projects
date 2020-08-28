module TupleSortSpec where

import Test.Hspec

import qualified Data.List as List
import Data.List.Split (splitOn)
import Data.Char (toLower)
import Safe (lastMay)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can do a case insensitive string sort" $ do
    insensitiveSort
      ["Fred", "bob", "Tom", "Mark", "john", "Steve"]
    `shouldBe`
      ["bob", "Fred", "john", "Mark", "Steve", "Tom"]

  it "can sort emails by domain" $ do
    domainSort
      [ "Fred@cmu.edu"
      , "Bob@yahoo.com"
      , "Tom@gmail.com"
      , "Mark@ucla.edu"
      , "John@pit.edu"
      , "Steve@microsoft.com"
      ]
    `shouldBe`
      [ "Fred@cmu.edu"
      , "Tom@gmail.com"
      , "Steve@microsoft.com"
      , "John@pit.edu"
      , "Mark@ucla.edu"
      , "Bob@yahoo.com"
      ]
    

insensitiveSort :: [String] -> [String]
insensitiveSort xs =
  fmap fst $ List.sortOn
    snd
    (fmap (\x -> (x, toLower <$> x)) xs)

domainSort :: [String] -> [String]
domainSort xs = 
  fmap fst $ List.sortOn snd
    (fmap 
      (\x -> 
        ( x
        , maybe "" id (lastMay $ splitOn "@" x)
        ) 
      )
      xs
    )
