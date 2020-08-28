module EitherMonad where

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _  (First  a) = First a
  fmap fn (Second b) = Second $ fn b

instance Applicative (Sum a) where
  pure b = Second b 
  First a   <*> _        = First a
  _         <*> First a  = First a
  Second fn <*> Second b = Second $ fn b

instance Monad (Sum a) where
  return = pure
  First a  >>= _  = First a
  Second b >>= fn = fn b
