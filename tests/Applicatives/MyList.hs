module MyList where

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap fn (Cons a list) = Cons (fn a) (fmap fn list)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons fn fnList) list  = fmap fn list <> (fnList <*> list)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Semigroup (List a) where
  (<>) Nil Nil = Nil
  (<>) list Nil = list
  (<>) Nil list = list
  (<>) (Cons a list) list' = Cons a (list <> list')
