{-# LANGUAGE InstanceSigs #-}

module MonadTransformers.ReaderT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT . pure $ pure a
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $
    (<*>) <$> rmab <*> rma
 
instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $
    (\r -> rma r >>= 
      (\a -> (runReaderT $ f a) r)
    )

instance MonadTrans (ReaderT r) where
  lift :: (Monad m) => m a -> ReaderT r m a
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO
