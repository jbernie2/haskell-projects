module LinkedList
  ( empty
  , toList
  , fromList
  , append
  , prepend
  , removeFirst
  , llReverse
  )
where

data LinkedList a = ListNext a (LinkedList a) | ListEmpty

empty :: LinkedList a
empty = ListEmpty

-- O(n)
toList :: LinkedList a -> [a]
toList ListEmpty       = []
toList (ListNext a ll) = a : toList ll

-- O(n)
fromList :: [a] -> LinkedList a
fromList []     = ListEmpty
fromList (x:xs) = ListNext x (fromList xs)

-- O(n)
append :: a -> LinkedList a -> LinkedList a
append i ListEmpty            = ListNext i (ListEmpty)
append i (ListNext a ll)      = ListNext a $ append i ll

-- O(1)
prepend :: a -> LinkedList a -> LinkedList a
prepend i ListEmpty = ListNext i (ListEmpty)
prepend i ll        = ListNext i ll

-- O(1)
removeFirst :: LinkedList a -> (Maybe a, LinkedList a)
removeFirst ListEmpty = (Nothing, ListEmpty)
removeFirst (ListNext a ll) = (Just a, ll)

-- O(n)
llReverse :: LinkedList a -> LinkedList a
llReverse ll = reverse2 empty ll

reverse2 :: LinkedList a -> LinkedList a -> LinkedList a
reverse2 newList oldList =
  case removeFirst oldList of
    (Just i, rest) ->
      reverse2 (prepend i newList) rest
    (Nothing, _) ->
      newList
   

