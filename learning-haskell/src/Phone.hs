module Phone where

import Data.List (find)

type Digit = Char

type Presses = Int

data DaPhone = DaPhone
  { char :: Char
  , actions :: [(Digit, Presses)]
  }

phoneConfig :: [DaPhone]
phoneConfig =
  fmap mkConfig 
  [ ('a', [('2', 1)]) 
  , ('b', [('2', 2)])
  , ('c', [('2', 3)])
  , ('2', [('2', 4)])
  , ('d', [('3', 1)])
  , ('e', [('3', 2)])
  , ('f', [('3', 3)])
  , ('3', [('3', 4)])
  , ('g', [('4', 1)])
  , ('h', [('4', 2)])
  , ('i', [('4', 3)])
  , ('4', [('4', 4)])
  , ('j', [('5', 1)])
  , ('k', [('5', 2)])
  , ('l', [('5', 3)])
  , ('5', [('5', 4)])
  , ('m', [('6', 1)])
  , ('n', [('6', 2)])
  , ('o', [('6', 3)])
  , ('6', [('6', 4)])
  , ('p', [('7', 1)])
  , ('q', [('7', 2)])
  , ('r', [('7', 3)])
  , ('s', [('7', 4)])
  , ('7', [('7', 5)])
  , ('t', [('8', 1)])
  , ('u', [('8', 2)])
  , ('v', [('8', 3)])
  , ('8', [('8', 4)])
  , ('w', [('9', 1)])
  , ('x', [('9', 2)])
  , ('y', [('9', 3)])
  , ('z', [('9', 4)])
  , ('9', [('9', 5)])
  , ('*', [('a', 1)])
  , ('^', [('a', 2)])
  , (' ', [('0', 1)])
  , ('+', [('0', 2)])
  , ('_', [('0', 3)])
  , ('0', [('0', 4)])
  , ('.', [('b', 1)])
  , (',', [('b', 2)])
  , ('#', [('b', 3)])
  , ('A', [('2', 1), ('a', 1)]) 
  , ('B', [('2', 2), ('a', 1)])
  , ('C', [('2', 3), ('a', 1)])
  , ('D', [('3', 1), ('a', 1)])
  , ('E', [('3', 2), ('a', 1)])
  , ('F', [('3', 3), ('a', 1)])
  , ('G', [('4', 1), ('a', 1)])
  , ('H', [('4', 2), ('a', 1)])
  , ('I', [('4', 3), ('a', 1)])
  , ('J', [('5', 1), ('a', 1)])
  , ('K', [('5', 2), ('a', 1)])
  , ('L', [('5', 3), ('a', 1)])
  , ('M', [('6', 1), ('a', 1)])
  , ('N', [('6', 2), ('a', 1)])
  , ('O', [('6', 3), ('a', 1)])
  , ('P', [('7', 1), ('a', 1)])
  , ('Q', [('7', 2), ('a', 1)])
  , ('R', [('7', 3), ('a', 1)])
  , ('S', [('7', 4), ('a', 1)])
  , ('T', [('8', 1), ('a', 1)])
  , ('U', [('8', 2), ('a', 1)])
  , ('V', [('8', 3), ('a', 1)])
  , ('W', [('9', 1), ('a', 1)])
  , ('X', [('9', 2), ('a', 1)])
  , ('Y', [('9', 3), ('a', 1)])
  , ('Z', [('9', 4), ('a', 1)])
  ]
 where
  mkConfig :: (Char, [(Digit, Presses)]) -> DaPhone
  mkConfig (a,b) =
    DaPhone
    { char = a
    , actions = b
    }

reverseTaps :: [DaPhone] -> Char -> [(Digit, Presses)]
reverseTaps configs c =
  let
    mConfig = find (\cfg -> (char cfg) == c) configs
  in
    case mConfig of
      Just config ->
        actions config
      Nothing ->
        [('0', 0)]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol OK. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "OK. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

cellPhonesDead :: [DaPhone] -> String -> [(Digit, Presses)]
cellPhonesDead config s =
  foldr (++) [] $
    map (reverseTaps config) s 

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps =
  foldl (\acc (d,p) -> acc + p) (0 :: Presses)

