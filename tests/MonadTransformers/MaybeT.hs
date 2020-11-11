{-# LANGUAGE InstanceSigs #-}

module MonadTransformers.MaybeT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ (fmap . fmap) f m

instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT . pure $ pure a
  (MaybeT fab) <*> (MaybeT ma) = MaybeT $ (<*>) <$> fab <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT mma) >>= f = MaybeT $
    mma >>= (\ma ->
      case ma of
        Just a ->
          runMaybeT $ f a
        Nothing ->
          return Nothing
    )

instance MonadTrans MaybeT where
  lift :: (Monad m) => m a -> MaybeT m a
  lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = lift . liftIO
