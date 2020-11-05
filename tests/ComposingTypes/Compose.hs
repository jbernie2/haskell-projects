{-# LANGUAGE InstanceSigs #-}
module ComposingTypes.Compose where

--import Control.Applicative (liftA2)
--import Data.Foldable

newtype Compose f g a = 
  Compose { getCompose :: f (g a) }
  deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose . pure $ pure a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose $ 
    (<*>) <$> fgab <*> fga -- or do liftA2 (<*>) fgab fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m 
          => (a -> m) 
          -> Compose f g a 
          -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga
    --foldr (\ga acc -> (foldMap f ga) <> acc) mempty fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative h
           => (a -> h b) 
           -> Compose f g a
           -> h (Compose f g b)
  traverse ahb (Compose fga) = Compose <$> (traverse . traverse) ahb fga
    --fmap Compose $ sequenceA (fmap (traverse ahb) fga)

-- provably impossible 
--instance (Monad f, Monad g) => Monad (Compose f g) where
  --return = pure
  --(>>=) :: Compose f g a
        -- -> (a -> Compose f g b)
        -- -> Compose f g b
  --(>>=) (Compose fga) aCfgb = Compose $
    --fga >>= 
      --(\ga -> ga >>= 
        --(\a -> aCfgb a ))
    
  
