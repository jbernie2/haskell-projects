module ComposingTypes.Bifunctor where

class Bifunctor p where

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second f = bimap id f

data Deux a b = Deux a b
  deriving Show

instance Bifunctor Deux where
  first f (Deux a b)  = Deux (f a) b
  second f (Deux a b)  = Deux a (f b)

data Const a b = Const a
  deriving Show

instance Bifunctor Const where
  first f (Const a)  = Const (f a)
  second _ (Const a)  = Const a

data Drei a b c = Drei a b c
  deriving Show

instance Bifunctor (Drei a) where
  first f (Drei a b c) = Drei a (f b) c
  second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b
  deriving Show

instance Bifunctor (SuperDrei a) where
  first f (SuperDrei a b) = SuperDrei a (f b)
  second _ (SuperDrei a b) = SuperDrei a b

data SemiDrei a b c = SemiDrei a
  deriving Show

instance Bifunctor (SemiDrei a) where
  first _ (SemiDrei a)  = SemiDrei a
  second _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
  deriving Show

instance Bifunctor (Quadriceps a b) where
  first f (Quadzzz a b c d) = Quadzzz a b (f c) d
  second f (Quadzzz a b c d) = Quadzzz a b c (f d)

data Either' a b
  = Left' a
  | Right' b
  deriving (Show)

instance Bifunctor Either' where
  first f (Left' a) = Left' (f a)
  first _ (Right' b) = Right' b
  second _ (Left' a) = Left' a
  second f (Right' b) = Right' (f b)

