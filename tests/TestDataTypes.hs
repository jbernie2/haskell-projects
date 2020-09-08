{-# LANGUAGE FlexibleInstances #-}

module TestDataTypes where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = 
  Trivial -> Trivial -> Trivial -> Bool


newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

type IdentityAssoc a = 
  Identity a -> Identity a -> Identity a -> Bool


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Eq a => EqProp (Pair a) where
  (=-=) = eq



data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = (Two mempty b)
  Two a f <*> Two a' b = Two (a <> a') (f b) 

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

instance Traversable (Two a) where
  traverse f (Two a b) = Two a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary 
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq
  
type TwoAssoc a b = 
  Two a b -> Two a b -> Two a b -> Bool



data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  Three a b f <*> Three a' b' c = Three (a <> a') (b <> b') (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary 
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b = Four' a b b b deriving (Eq, Show)
instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary 
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z


newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
  BoolConj a <> BoolConj b = BoolConj ( a && b )

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjAssoc = 
  BoolConj -> BoolConj -> BoolConj -> Bool


newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
  BoolDisj a <> BoolDisj b = BoolDisj ( a || b )

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjAssoc = 
  BoolDisj -> BoolDisj -> BoolDisj -> Bool


data Or a b = Fst a | Snd b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Fst _ <> Fst b = Fst b
  Fst _ <> Snd b = Snd b
  Snd a <> Fst _ = Snd a
  Snd a <> Snd _ = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary 
    y <- arbitrary
    oneof
      [ return $ Fst x
      , return $ Snd y
      ]
  
type OrAssoc a b = 
  Or a b -> Or a b -> Or a b -> Bool


newtype Combine a b =
  Combine { unCombine :: (a -> b) }
instance Show (Combine a b) where
  show _ = "Showing Combine: what does this even mean?"

instance Semigroup b => Semigroup (Combine a b) where
  f <> g = Combine { unCombine = \x -> (unCombine f) x <> (unCombine g) x}

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine { unCombine = \_ -> mempty }
  mappend = (<>)
 
instance (Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    x <- arbitrary
    return $ Combine {unCombine = (\_ -> x) }

type CombineAssoc a b = 
  Combine a b -> Combine a b -> Combine a b -> a -> Bool


newtype Comp a =
  Comp { unComp :: (a -> a) }
instance Show (Comp a)  where
  show _ = "Showing Comp: what does this even mean?"

-- should this be composition or should it be like Combine? I have no idea.
instance Semigroup (Comp a) where
  a <> b = Comp { unComp = unComp a . unComp b }

instance Monoid (Comp a) where
  mempty = Comp { unComp = id }
  mappend = (<>)

instance Arbitrary a => Arbitrary (Comp a) where
  arbitrary =  do
    x <- arbitrary
    return $ Comp {unComp = (\_ -> x) }

type CompAssoc a = 
  Comp a -> Comp a -> Comp a -> a -> Bool


newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  x <> y = Mem { runMem = \z -> 
      ( fst (runMem x $ z) <> fst (runMem y $ z)
      , snd . (runMem y) . snd $ (runMem x) z
      )
    }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem { runMem = \x -> (mempty, x) }
  mappend = (<>)

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap fn (Yeppers a) = Yeppers $ fn a

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap fn (Second b) = Second $ fn b
  fmap _ (First a) = First a

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant m ) where
  fmap _ (Constant v) = Constant v

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


data Optional a
  = Nada
  | Yep a
  deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency
                [ (1, pure Nada)
                , (1, Yep <$> arbitrary)
                ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap g (Wrap fa) = Wrap (fmap g fa)


data Company a c b
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c ) = DeepBlue a c


data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant x) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap fn (Bloor b) = Bloor $ fn b


newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)
newtype K a b = K a

instance Functor (Flip K a) where
  fmap fn (Flip (K b)) = Flip (K (fn b))


data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap fn (GoatyConst b) = GoatyConst $ fn b


data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap fn (LiftItOut fa) = LiftItOut $ fmap fn fa

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap fn (DaWrappa fa ga) = DaWrappa (fmap fn fa) (fmap fn ga)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap fn (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap fn gb

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap fn (Notorious go ga gt) = Notorious go ga $ fmap fn gt


data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap fn (OneGoat a) = OneGoat $ fn a
  fmap fn (MoreGoats gl1 gl2 gl3) = MoreGoats 
                                      (fmap fn gl1)
                                      (fmap fn gl2)
                                      (fmap fn gl3)

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap fn (Print s a) = Print s $ fn a
  fmap fn (Read fn') = Read (fn . fn')


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq


data BahEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a)  = PLeft $ f a

instance Applicative (BahEither b) where
  pure = PLeft
  PRight b <*> _        = PRight b
  _        <*> PRight b = PRight b
  PLeft  f <*> PLeft a  = PLeft $ f a

instance Monad (BahEither b) where
  return = pure
  PRight b  >>= _ = PRight b
  PLeft  a  >>= f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
  arbitrary = frequency
                [ (1, PRight <$> arbitrary)
                , (1, PLeft  <$> arbitrary)
                ]

instance (Eq b, Eq a) => EqProp (BahEither b a) where
  (=-=) = eq


data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) Nil Nil = Nil
  (<>) list Nil = list
  (<>) Nil list = list
  (<>) (Cons a list) list' = Cons a (list <> list')

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap fn (Cons a (rest)) = Cons (fn a) (fmap fn rest)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons fn fnList) list = fmap fn list <> (fnList <*> list)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons a list >>= f = f a <> (list >>= f)

instance Foldable List where
  foldMap _ Nil           = mempty
  foldMap f (Cons a list) = f a <> foldMap f list

instance Traversable List where
  traverse _ Nil           = pure Nil
  traverse f (Cons a list) = Cons <$> f a <*> traverse f list

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency
                [ (1, Cons <$> arbitrary <*> arbitrary)
                , (1, pure Nil)
                ]

instance Eq a => EqProp (List a) where
  (=-=) = eq


data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq


data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq


data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> (traverse f na) <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance
  (Applicative n
  , Testable (n Property)
  , Eq a
  , Eq (n a) , EqProp a)
  => EqProp (S n a)
  where 
    (=-=) = eq


data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node leftTree a rightTree) =
    Node (fmap f leftTree) (f a) (fmap f rightTree)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node leftTree a rightTree) =
    (foldMap f leftTree) <> f a <> (foldMap f rightTree)

  foldr _ acc Empty = acc
  foldr f acc (Leaf a) = f a acc
  foldr f acc (Node lt a rt) = foldr f (f a (foldr f acc rt) ) lt

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node lt a rt) = Node <$> traverse f lt <*> f a <*> traverse f rt

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency
                [ (1, pure Empty)
                , (1, Leaf <$> arbitrary)
                , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
                ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq
