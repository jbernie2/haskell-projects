module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123)) , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate dbis = foldr getDbTime [] dbis
  where
    getDbTime :: DatabaseItem -> [UTCTime] -> [UTCTime]
    getDbTime (DbDate time) times = time : times
    getDbTime _ times = times

filterDbNumber :: [DatabaseItem]
             -> [Integer]
filterDbNumber dbis = foldr getDbInt [] dbis
  where
    getDbInt :: DatabaseItem -> [Integer] -> [Integer]
    getDbInt (DbNumber i) is = i : is
    getDbInt _ is = is

mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent dbis = foldr max (head dates) dates
  where dates = filterDbDate dbis

sumDb :: [DatabaseItem]
      -> Integer
sumDb dbis = foldr (+) 0 (filterDbNumber dbis)

avgDb :: [DatabaseItem]
      -> Double
avgDb dbis = 
  (fromIntegral $ sumDb dbis) / (fromIntegral . length $ filterDbNumber dbis)
