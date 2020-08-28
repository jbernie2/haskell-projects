module DeleteDistance where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe

deleteDistance :: String -> String -> Int
deleteDistance str1 str2 = 
 case fromCache 0 0 cache of
    Just i -> i
    Nothing -> maxBound
 where
  cache :: Map Int (Map Int Int)
  cache =
    d str1 0 str2 0 initCache

  initCache :: Map Int (Map Int Int)
  initCache = Map.empty

d :: String -> Int -> String -> Int -> Map Int (Map Int Int) -> Map Int (Map Int Int)
d []         i []         j cache = toCache i j 0 cache
d []         i (_ : s2s)  j cache = 
  case fromCache i j cache of
    Just _ ->
      cache
    Nothing ->
      minDeleteDistance i j False $ d [] i s2s (j+1) cache

d (_ : s1s) i []         j cache =
  case fromCache i j cache of
    Just _ ->
      cache
    Nothing ->
      minDeleteDistance i j False $ d s1s i [] j cache

d (s1 : s1s) i (s2 : s2s) j cache =
  case fromCache i j cache of
    Just _ ->
      cache
    Nothing ->
      minDeleteDistance i j (s1 == s2) 
        ((d s1s        (i+1) (s2 : s2s) j     )
        . (d (s1 : s1s) i     s2s        (j+1) )
        $ (d s1s        (i+1) s2s        (j+1) cache))

minDeleteDistance :: Int -> Int -> Bool -> Map Int (Map Int Int) -> Map Int (Map Int Int)
minDeleteDistance i j equal cache =
  toCache i j getMin cache
 where
  getMin :: Int
  getMin =
    if equal then
      minimum $ Maybe.catMaybes
        [ fromCache (i+1) j cache
        , fromCache i (j+1) cache
        , fromCache (i+1) (j+1) cache
        ]
    else
      minimum $ Maybe.catMaybes
        [ (+1) <$> (fromCache (i+1) j cache)
        , (+1) <$> (fromCache i (j+1) cache)
        , (+2) <$> (fromCache (i+1) (j+1) cache)
        ]

fromCache :: Int -> Int -> Map Int (Map Int Int) -> Maybe Int
fromCache i j cache =
  case Map.lookup i cache of
    Just subCache ->
      Map.lookup j subCache
    Nothing -> 
      Nothing

toCache :: Int -> Int -> Int -> Map Int (Map Int Int) -> Map Int (Map Int Int)
toCache i j v cache =
  case Map.lookup i cache of
    Just subCache ->
      Map.insert i (Map.insert j v subCache) cache
    Nothing -> 
      Map.insert i (Map.insert j v Map.empty) cache
  



