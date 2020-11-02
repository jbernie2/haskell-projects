{-# LANGUAGE QuasiQuotes #-}

module ParserCombinators.Schedule where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ
import Test.Hspec
import Data.Map (Map)
import Data.List (sort, sortOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
--import Debug.Trace (trace)

schedule :: String
schedule = [r|
--1whee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
|]

emptyDailySchedule :: String
emptyDailySchedule = [r|
# 2025-02-05
|]

dailySchedule :: String
dailySchedule = [r|
--2whee a comment
# 2025-02-05
--3whee a comment
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
]

schedule' :: String
schedule' = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

parsedDailySchedule :: DailySchedule
parsedDailySchedule = DailySchedule
  (Date 2025 2 5)
  [ (Time 8 0,  Activity "Eating")
    , (Time 9 0,  Activity "Sanitizing moisture collector")
    , (Time 11 0, Activity "Exercising in high-grav gym")
    , (Time 12 0, Activity "Eating")
    , (Time 13 0, Activity "Something else")
  ]

isomorphicSchedule :: String
isomorphicSchedule = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
# 2025-02-07
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
22:00 Sleep
|]

parsedSchedule :: Schedule
parsedSchedule = Schedule $ 
  [ DailySchedule
      (Date 2025 2 5)
      [ (Time 8 0,  Activity "Eating")
        , (Time 9 0,  Activity "Sanitizing moisture collector")
        , (Time 11 0, Activity "Exercising in high-grav gym")
        , (Time 12 0, Activity "Eating")
        , (Time 13 0, Activity "Something else")
      ]
  , DailySchedule
      (Date 2025 2 7)
      [ (Time 8 0,  Activity "Eating")
        , (Time 9 0,  Activity "Sanitizing moisture collector")
        , (Time 11 0, Activity "Exercising in high-grav gym")
        , (Time 12 0, Activity "Eating")
        , (Time 13 0, Activity "Something else")
      ]
  ]

prop_ScheduleParseShowIsomorphism :: Schedule -> Bool
prop_ScheduleParseShowIsomorphism s =
  maybeSuccess (parseString parseSchedule mempty (show s)) == Just s

prop_DailyScheduleParseShowIsomorphism :: DailySchedule -> Bool
prop_DailyScheduleParseShowIsomorphism s =
  maybeSuccess (parseString parseDailySchedule mempty (show s)) == Just s

prop_DateParseShowIsomorphism :: Date -> Bool
prop_DateParseShowIsomorphism s =
  maybeSuccess (parseString parseDate mempty (show s)) == Just s

prop_TimeParseShowIsomorphism :: Time -> Bool
prop_TimeParseShowIsomorphism s =
  maybeSuccess (parseString parseTime mempty (show s)) == Just s

prop_ActivityParseShowIsomorphism :: Activity -> Bool
prop_ActivityParseShowIsomorphism s =
  maybeSuccess (parseString parseActivity mempty (show s)) == Just s

data Schedule =
  Schedule [DailySchedule]
  deriving (Eq)
instance Show Schedule where
  show (Schedule dailies) =
    foldl (\acc ds -> acc ++ (show ds)) "" dailies
instance QC.Arbitrary Schedule where
  arbitrary = Schedule <$> QC.arbitrary

data DailySchedule =
  DailySchedule Date [(Time, Activity)]
  deriving (Eq)
instance Show DailySchedule where
  show (DailySchedule date logLines) =
    foldl (\acc ((time), (activity)) ->
      acc ++ (show time) ++ " " ++ (show activity)
    ) ((show date) ++ "\n") logLines
instance QC.Arbitrary DailySchedule where
  arbitrary = DailySchedule <$> QC.arbitrary <*> QC.arbitrary

data TimeChart =
  TimeChart Date [(Time, Activity)]
  deriving (Eq, Show, Ord)
  
data Activity =
  Activity String
  deriving (Eq, Ord)
instance Show Activity where
  show (Activity a) = a ++ "\n"
instance QC.Arbitrary Activity where
  arbitrary = Activity . ("a" ++ ) <$> QC.listOf (QC.elements ['a'..'z'])

data Time =
  Time Integer Integer
  deriving (Eq, Ord)
instance Show Time where
  show (Time h m) =
    zeroPad h ++ ":" ++ zeroPad m
   where
    zeroPad = (\x -> if x < 10 then ("0" ++ show x) else show x)
instance QC.Arbitrary Time where
  arbitrary = Time 
    <$> QC.choose (0,23)
    <*> QC.choose (0, 60) 

data Date =
  Date Integer Integer Integer
  deriving (Eq, Ord)
instance Show Date where
  show (Date y m d) =
    "# " ++ show y ++ "-" ++ zeroPad m ++ "-" ++ zeroPad d
   where
    zeroPad = (\x -> if x < 10 then ("0" ++ show x) else show x)
instance QC.Arbitrary Date where
  arbitrary = Date 
    <$> QC.choose (1900,2100)
    <*> QC.choose (1,12)
    <*> QC.choose(1,31)

timePerActivityPerDay :: Schedule -> [TimeChart]
timePerActivityPerDay (Schedule daily) =
  sort (map timePerActivity daily)

timePerActivity :: DailySchedule -> TimeChart
timePerActivity (DailySchedule date activities) =
  toTimeChart date $ timePerActivity' Map.empty activities
 where
  timePerActivity' :: Map Activity Time -> [(Time, Activity)] -> Map Activity Time
  timePerActivity' acc ( (t1,a1) : (t2,a2) : as) = 
    timePerActivity' (
      case Map.lookup a1 acc of
        Just t ->
          Map.insert a1 (addTime t (elapsedTime t1 t2)) acc
        Nothing ->
          Map.insert a1 (elapsedTime t1 t2) acc
      )
      ((t2,a2) : as)

  timePerActivity' acc ( (t1,a1) : []) =
    case Map.lookup a1 acc of
      Just t ->
        Map.insert a1 (addTime t (elapsedTime t1 endOfDay)) acc
      Nothing ->
        Map.insert a1 (elapsedTime t1 endOfDay) acc

  timePerActivity' acc [] = acc

  toTimeChart :: Date -> Map Activity Time -> TimeChart
  toTimeChart d m = TimeChart d $ 
    fmap (\(a,t) -> (t, a)) (sortOn snd (Map.toList m))

endOfDay :: Time
endOfDay =
  Time 23 59

addTime :: Time -> Time -> Time
addTime (Time h1 m1) (Time h2 m2) =
  Time 
    (h1 + h2 + ((m1 + m2) `div` 60))
    ((m1 + m2) `mod` 60)

elapsedTime :: Time -> Time -> Time
elapsedTime (Time h1 m1) (Time h2 m2) =
  Time 
    (minutes `div` 60)
    (minutes `mod` 60)
 where
  minutes = ((h2 - h1) * 60) + (m2 - m1)

parseSchedule :: Parser Schedule
parseSchedule =
  --(Schedule . sortBy
    --(\(DailySchedule t1 _) (DailySchedule t2 _) -> compare t1 t2) 
  --) <$> many parseDailySchedule
  Schedule <$> many parseDailySchedule

parseDailySchedule :: Parser DailySchedule
parseDailySchedule = do
  _ <- skipJunk
  date <- parseDate
  activities  <- many parseActivityLog
  return (DailySchedule date activities)

skipJunk :: Parser ()
skipJunk =
      ((try skipComments) >> skipJunk)
  <|> ((try newline) >> skipJunk)
  <|> return ()

skipComments :: Parser ()
skipComments =
  try (char '-' >> char '-' >> some (noneOf "\n") >> newline >> return ())

parseDate :: Parser Date
parseDate = do
  _ <- char '#' >> spaces
  year <- integer
  _ <- char '-'
  month <- integer
  _ <- char '-'
  day <- integer
  _ <- try spaces <|> return ()
  _ <- try skipComments <|> return ()
  return (Date year month day)

parseActivityLog :: Parser (Time, Activity)
parseActivityLog = do
  time <- parseTime
  activity <- parseActivity
  return (time, activity)

parseTime :: Parser Time
parseTime = do
  hour <- integer
  _ <- char ':'
  minute <- integer
  return (Time hour minute)

parseActivity :: Parser Activity
parseActivity =
  Activity . T.unpack . T.strip . T.pack <$> parseActivityText
 where
  parseActivityText :: Parser String
  parseActivityText =
        try (
          (some $ noneOf "-\n") 
          <* char '-' 
          <* char '-' 
          <* (some $ noneOf "\n")
          <* newline 
          )
    <|> try parseWithDash 
    <|> try (some $ noneOf "\n") <* newline

  parseWithDash = do
    preDash <- (some (noneOf "-\n"))
    dash <- char '-'
    postDash <- parseActivityText
    return (preDash ++ [dash] ++ postDash)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Parsing a Schedule" $ do
    it "can parse a schedule" $ do
      let m  = parseString parseSchedule mempty schedule
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Schedule
        [ DailySchedule (Date 2025 2 5)
            [ (Time 8 0,  Activity "Breakfast")
            , (Time 9 0,  Activity "Sanitizing moisture collector")
            , (Time 11 0, Activity "Exercising in high-grav gym")
            , (Time 12 0, Activity "Lunch")
            ]
        , DailySchedule (Date 2025 2 7)
            [ (Time 8 0,   Activity "Breakfast")
            , (Time 9 0,   Activity "Bumped head, passed out")
            , (Time 13 36, Activity "Wake up, headache")
            ]
        ])

  describe "Parsing a daily schedule" $ do
    it "can parse" $ do
      let m  = parseString parseDailySchedule mempty dailySchedule
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ DailySchedule
          (Date 2025 2 5)
          [ (Time 8 0,  Activity "Breakfast")
            , (Time 9 0,  Activity "Sanitizing moisture collector")
            , (Time 11 0, Activity "Exercising in high-grav gym")
            , (Time 12 0, Activity "Lunch")
          ]
        )

    it "can parse an empty schedule" $ do
      let m  = parseString parseDailySchedule mempty emptyDailySchedule
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ DailySchedule (Date 2025 2 5) [])


  describe "Parsing a Date" $ do
    it "can parse a date" $ do
      let m  = parseString parseDate mempty "# 2020-02-05\n"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Date 2020 2 5)
    it "can parse a date with comments" $ do
      let m  = parseString parseDate mempty "# 2020-02-05 --comments blh blah\n"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Date 2020 2 5)

  describe "Parsing an activity log line" $ do
    it "can parse" $ do
      let m  = parseString parseActivityLog mempty "08:00 Breakfast\n"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Time 8 0, Activity "Breakfast")

    it "can parse with a comment" $ do
      let m  = parseString parseActivityLog mempty "08:00 Breakfast --balh bl\n"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Time 8 0, Activity "Breakfast")

    it "can parse with dashes" $ do
      let m  = parseString parseActivityLog mempty "08:00 Breakfast -balh bl\n"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Time 8 0, Activity "Breakfast -balh bl")

    it "can parse with dashes and comments" $ do
      let m  = parseString parseActivityLog mempty "08:00 Breakfast -balh --bl\n"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Time 8 0, Activity "Breakfast -balh")

    it "only parses one line" $ do
      let m  = parseString parseActivityLog mempty "08:00 A\n08:01 B\n -balh --bl\n"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Time 8 0, Activity "A")

  describe "skipping junk" $ do
    it "skips comments" $ do
      let m  = parseString (skipComments >> char 'a') mempty "--cbdsjk\na\n"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ('a')

    it "skips comments newlines and spaces" $ do
      let m  = parseString (skipJunk >> char 'a') mempty "\n\n--cbdsjk\n\na"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ('a')

  describe "Aggregate stats" $ do
    it "can calculate the average time per activity on a given day" $ do
      let r' = timePerActivity parsedDailySchedule
      r' `shouldBe` (TimeChart
            (Date 2025 2 5)
            [   (Time 1 0, Activity "Exercising in high-grav gym")
              , (Time 2 0,  Activity "Eating")
              , (Time 2 0,  Activity "Sanitizing moisture collector")
              , (Time 10 59, Activity "Something else")
            ]
          )

    it "can calculate the average time per activity on each day" $ do
      let r' = timePerActivityPerDay parsedSchedule
      r' `shouldBe` (
          [ TimeChart
              (Date 2025 2 5)
              [   (Time 1 0, Activity "Exercising in high-grav gym")
                , (Time 2 0,  Activity "Eating")
                , (Time 2 0,  Activity "Sanitizing moisture collector")
                , (Time 10 59, Activity "Something else")
              ]
         , TimeChart
              (Date 2025 2 7)
              [   (Time 1 0, Activity "Exercising in high-grav gym")
                , (Time 2 0,  Activity "Eating")
                , (Time 2 0,  Activity "Sanitizing moisture collector")
                , (Time 10 59, Activity "Something else")
              ]
        ])

  describe "show" $ do
    it "shows the same as the file format" $ do
      let m  = parseString parseSchedule mempty "" -- isomorphicSchedule
          r' = maybeSuccess m
      print m
      (show <$> r') `shouldBe` Just ""
  
    it "is isomorphic with parse" $ do
      QC.quickCheck prop_ActivityParseShowIsomorphism
      QC.quickCheck prop_TimeParseShowIsomorphism
      QC.quickCheck prop_DateParseShowIsomorphism
      QC.quickCheck prop_DailyScheduleParseShowIsomorphism
      QC.quickCheck prop_ScheduleParseShowIsomorphism

