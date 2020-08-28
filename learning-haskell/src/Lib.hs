{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.List (sort, intersperse)
import Data.Char as Char

someFunc :: IO ()
someFunc = putStrLn "Hello!"

sayHello :: String -> IO ()
sayHello x =
  putStrLn("Hello, " ++ x ++ "!")

triple :: Int -> Int
triple x = 
  x * 3


data Mood = Blah | Woot deriving (Show, Eq)
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

settleDown :: Mood -> Mood
settleDown x = 
  if x == Woot
    then Blah
    else x

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if ( x < y ) then fstString x else sndString y
       where x = "Singin"
             y = "Somewhere"

data DayOfWeek = 
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

instance Eq DayOfWeek where
 (==) Mon Mon   = True
 (==) Tue Tue   = True
 (==) Weds Weds = True
 (==) Thu Thu   = True
 (==) Fri Fri   = True
 (==) Sat Sat   = True
 (==) Sun Sun   = True
 (==) _ _       = False

data Date =
  Date DayOfWeek Int
  deriving Show

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn int) (TisAn int') =
    int == int'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') =
    a == a' && b == b'

data StringOrInt = 
    TisAnInt Int
  | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') =
    a == a'
  (==) (TisAString a) (TisAString a') =
    a == a'
  (==) _ _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') =
    x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') =
    x == x' && y == y'

data Which a =
    ThisOne a 
  | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') =
    x == x'
  (==) (ThatOne x) (ThatOne x') =
    x == x'
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') =
    x == x'
  (==) (Goodbye x) (Goodbye x') =
    x == x'
  (==) _ _ = False

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

data Age = Age Integer
  deriving (Eq, Show)
instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

data Year = Year Integer
  deriving (Eq, Show)
instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: (Numberish a, Numberish b) => a -> b -> a
sumNumberish a a' = fromNumber $ (+) (toNumber a) (toNumber a')


data Person = Person Bool
  deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


type Subject = String
type Verb = String 
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String deriving  (Eq, Show)
data Yeah = 
  Yeah Bool deriving (Eq, Show)
data Papu =
  Papu Rocks Yeah
  deriving (Eq,Show)

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)


mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)


chk :: Eq b => (a -> b) -> a -> b -> Bool
chk a2b a b = a2b a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith a2b i a = (a2b a) + (fromInteger i)

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1


functionC x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0


tensDigit :: Integral a => a -> a
tensDigit x = snd . (`divMod` 10) $ fst (x `divMod` 10)

hunsD :: Integral a => a -> a
hunsD x = snd . (`divMod` 10) $ fst (x `divMod` 100)

foldBool :: a -> a -> Bool -> a
foldBool a b c = case c of
  False -> a
  True -> b

foldBool2 :: a -> a -> Bool -> a
foldBool2 a b c
  | c == False = a
  | c == True = b


roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
mySum n = n + (mySum (n - 1))

myMult :: (Integral a) => a -> a -> a
myMult x y
  | y < 0  = (0 - x) + (myMult x (y + 1))
  | y == 0 = 0
  | otherwise = x + (myMult x (y - 1))


myDiv :: (Integral a) => a -> a -> Maybe a
myDiv x y
    | y == 0         = Nothing
    | x < 0 && y < 0 = Just (go (abs x) (abs y))
    | x < 0 || y < 0 = Just (0 - go (abs x) (abs y))
    | otherwise      = Just (go x y)
  where 
    go x y 
      | x == 0 = 0
      | x < y  = 0 
      | otherwise = 1 + go (x-y) y
  
mc91 :: (Integral a) => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ n + 11


digitToWord :: Int -> String 
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "wat"

digits :: Int -> [Int]
digits n = go (abs n)
    where
  go n
   | n `div` 10 == 0 = [n]
   | otherwise       = (go (n `div` 10)) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

eftA :: (Ord a, Enum a) => a  -> a -> [a]
eftA from to
  | from == to = [from]
  | from < to = from : eftA (succ from) to
  | from > to = []

splitOn :: Char -> String -> [String]
splitOn s [] = []
splitOn s x = takeWhile (/=s) x : ( splitOn s $ dropWhile (==s) $ dropWhile (/=s) x)

myWords :: [Char] -> [[Char]]
myWords x = splitOn ' ' x

myLines :: String -> [String]
myLines x = splitOn '\n' x

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

areTheyEqual :: Bool
areTheyEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" 
  ] == (myLines sentences)


myZip :: [a] -> [b] -> [(a,b)]
myZip xs ys = myZipWith (\x y -> (x,y)) xs ys

myZipWith :: ( a -> b -> c) -> [a] -> [b] -> [c]
myZipWith fn (x : xs) (y : ys) = (fn x y) : myZipWith fn xs ys
myZipWith _ _ _ = []
  

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fn (x:xs) = fn x ++ squishMap fn xs
squishMap fn [] = []

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy fn (x1 : x2 : xs)
  | fn x1 x2 == GT = myMaximumBy fn (x1 : xs)
  | otherwise      = myMaximumBy fn (x2 : xs)
myMaximumBy fn (x1 : []) = x1

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy fn (x1 : x2 : xs)
  | fn x1 x2 == LT = myMinimumBy fn (x1 : xs)
  | otherwise      = myMinimumBy fn (x2 : xs)
myMinimumBy fn (x1 : []) = x1

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare


fibs = 1 : scanl (+) 1 fibs

facts = scanl (*) 1 [2..]


stops = "pbtdkg"
vowels = "aeiou"


combine3P :: [Char] -> [Char] -> [(Char,Char,Char)]
combine3P xs ys =
  filter (\(a,_,_) -> a == 'p') (combine3 xs ys)


combine3 :: [Char] -> [Char] -> [(Char,Char,Char)]
combine3 xs ys =
  map mkTuple (combineMany [xs, ys, xs])
 where
  mkTuple (c1:c2:c3:cs) = (c1,c2,c3)
  mkTuple _ = ('z','z','z')  


combineMany :: [[a]] -> [[a]]
combineMany (x1:x2:[]) = combine x1 x2
combineMany (x1:x2:xs) =
  foldr (++) [] (map (\x -> combos' (combine x1 x2) x) xs)
combineMany (x1:[]) = [x1]
combineMany [] = []
  

combine :: [a] -> [a] -> [[a]]
combine xs ys =
  combos xs (split ys)
 where
  split :: [a] -> [[a]]
  split (z:zs) = [z] : split zs
  split [] = []


combos :: [a] -> [[a]] -> [[a]]
combos xs yss =
  foldr (++) [] ( map (\ys -> map (\x -> x : ys) xs) yss)


combos' :: [[a]] -> [a] -> [[a]]
combos' yss xs =
  foldr (++) [] ( map (\ys -> map (\x -> ys ++ [x]) xs) yss)
  
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny fn = foldr ((||) . fn) False


myElem :: Eq a => a -> [a] -> Bool
myElem elem = foldr (\a b -> b || elem==a) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' elem = myAny (elem==)

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap fn = foldr (\a b -> (fn a) : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fn = foldr (\a b -> if fn a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' fn = foldr (\a b -> fn a ++ b) []

squishAgain' :: [[a]] -> [a]
squishAgain' = squishMap' id

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' fn xs = foldl (\a b -> if fn a b == GT then a else b) (head xs) xs

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' fn xs = foldl (\a b -> if fn a b == LT then a else b) (head xs) xs


class TooMany a where 
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype MyTup = MyTup (Int, String) deriving (Eq, Show)
newtype MyInts = MyInts (Int, Int) deriving (Eq, Show)
--newtype MyOtherType = (Num a, TooMany a) => MyOtherType (a, a) deriving (Eq, Show)

instance TooMany MyTup  where
  tooMany (MyTup (i,s)) = tooMany i

instance TooMany MyInts  where
  tooMany (MyInts (i1,i2)) = tooMany (i1+i2)

instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (a,b) = tooMany (a+b)

data FlowerType
  = Gardenia
  | Daisy
  | Rose 
  | Lilac
  deriving Show

type Gardener = String

data Garden = 
  Garden Gardener FlowerType
  deriving Show

data Gardenia' = Gardenia' deriving Show
data Daisy' = Daisy' deriving Show
data Rose' = Rose' deriving Show
data Lilac' = Lilac' deriving Show


data OperatingSystem
  = Linux
  | BSD
  | Mac
  | Windows
  deriving (Eq, Show, Enum)

data ProgLang 
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show, Enum)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  }
  deriving (Show)

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- [Linux .. Windows], y <- [Haskell .. PureScript]]


data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

convert :: Quantum -> Bool
convert Yes = True
convert No = True
convert Both = True

convert' :: Quantum -> Bool
convert' Yes = True
convert' No = True
convert' Both = False

convert'' :: Quantum -> Bool
convert'' Yes = True
convert'' No = False
convert'' Both = True

convert''' :: Quantum -> Bool
convert''' Yes = False
convert''' No = True
convert''' Both = True

convert'''' :: Quantum -> Bool
convert'''' Yes = False
convert'''' No = True
convert'''' Both = False

convert''''' :: Quantum -> Bool
convert''''' Yes = False
convert''''' No = False
convert''''' Both = True

convert'''''' :: Quantum -> Bool
convert'''''' Yes = False
convert'''''' No = False
convert'''''' Both = False

convert''''''' :: Quantum -> Bool
convert''''''' Yes = False
convert''''''' No = False
convert''''''' Both = True


data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert'
  :: Ord a
  => a
  -> BinaryTree a
  -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree
  :: (a -> b)
  -> BinaryTree a
  -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
      1
      (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
  Node (Node Leaf 4 Leaf)
      2
      (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup OK!"
  else error "test failed!"


testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
      2
      (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node right b left) = [b] ++ (preorder right) ++ (preorder left)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node right b left) = (preorder right) ++ [b] ++ (preorder left)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node right b left) = (preorder right) ++ (preorder left) ++ [b]

testPreorder :: String
testPreorder =
  if preorder testTree == [2,1,3]
  then "Preorder fine!"
  else "Bad news bears"

testInorder :: String
testInorder =
  if inorder testTree == [1,2,3]
  then "Inorder fine!"
  else "Bad news bears"

testPostorder :: String
testPostorder =
  if postorder testTree == [1,3,2]
  then "Postorder fine!"
  else "Bad news bears"

foldTree
  :: (a -> b -> b)
  -> b
  -> BinaryTree a
  -> b
foldTree _ acc Leaf = acc
foldTree fn acc (Node left b right) =
  foldTree fn
    (fn b
      (foldTree fn acc left)
    )
    right

testFoldTree :: String
testFoldTree =
  if foldTree (\i s -> s ++ (show i))  ""  testTree == "123"
  then "Yay, you did it!"
  else "you've got some work to do"

isSubseqOf
  :: (Eq a)
  => [a]
  -> [a]
  -> Bool
isSubseqOf [] _ = True
isSubseqOf (x:xs) [] = False
isSubseqOf b@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf b ys
  

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

replaceThe :: String -> String
replaceThe s =
  foldr (++) [] $
    map (++ " ") $
      map replaceThe' (words s)
 where
  replaceThe' :: String -> String
  replaceThe' x =
    case notThe x of
      Nothing -> "a"
      Just a -> a

data WordType = The | Vowel | Other

mkWordType :: String -> WordType
mkWordType str =
  case str of
    "the" -> The
    (x:_) -> if elem x ['a','e','i','o','u'] then Vowel else Other

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str =
  countTheBeforeVowel' $ map mkWordType (words str)
 where
  countTheBeforeVowel' :: [WordType] -> Integer
  countTheBeforeVowel' wts =
    case wts of
      [] -> 0
      (_:[]) -> 0
      (The : Vowel : xs) -> 1 + countTheBeforeVowel' xs
      ( _ : xs) -> 0 + countTheBeforeVowel' xs

countVowels :: String -> Int
countVowels str =
  length $ filter isVowel str
 where
  isVowel :: Char -> Bool
  isVowel c = elem c ['a','e','i','o','u']

newtype Word' = Word' String deriving (Eq, Show)
mkWord :: String -> Maybe Word'
mkWord str =
  if vowels > consonants then Nothing else Just (Word' str)
 where
  vowels = countVowels str
  consonants = length str - vowels

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat int
  | int < 0 = Nothing
  | int == 0 = Just Zero
  | otherwise = Just $ foldr (\_ acc -> Succ acc) Zero [1..int]

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes maybes = map (\(Just x) -> x) $ filter isJust maybes

lefts' :: [Either a b] -> [a]
lefts' =
  foldr (\e acc -> 
    case e of
      (Left x) -> x : acc
      (Right _) -> acc
    ) []


rights' :: [Either a b] -> [b]
rights' =
  foldr (\e acc -> 
    case e of
      (Left _) -> acc
      (Right x) -> x : acc
    ) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' =
  foldr (\e (ls,rs) -> 
    case e of
      (Left l) ->  (l : ls, rs)
      (Right r) -> (ls, r : rs)
    ) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' b2c eab =
  case eab of
    Left a -> Nothing
    Right b -> Just $ b2c b

either' :: (a->c) -> (b->c) -> Either a b -> c
either' a2c b2c eab =
  case eab of
    Left a -> a2c a
    Right b -> b2c b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' b2c eab =
  either (\_ -> Nothing) (Just . b2c) eab

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Nothing -> []
    Just (a,b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x' -> Just (x', f x')) x

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing -> Leaf
    Just (ll,r,rl) -> Node (unfold f ll) r (unfold f rl)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold (treeBuildFunc n) 0
 where
  treeBuildFunc :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
  treeBuildFunc limit i
    | i == limit = Nothing
    | otherwise = Just (i+1, i, i+1)
