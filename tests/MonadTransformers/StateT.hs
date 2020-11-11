{-# LANGUAGE InstanceSigs #-}
module MonadTransformers.StateT where

import Data.Bifunctor (first)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b)
       -> StateT s m a
       -> StateT s m b
  fmap f (StateT sma) = StateT $ (fmap . fmap . first) f sma

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ (\s -> pure (a, s))

  (<*>) :: (StateT s m (a -> b))
        -> (StateT s m a)
        -> (StateT s m b)
  (StateT smab) <*> (StateT sma) = StateT $ 
    (\s ->
      (smab s) >>= 
        (\(ab, s') -> 
          (sma s') >>= 
            (\(a, s'') -> pure (ab a, s''))
        )
    )

instance (Monad m) => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    (runStateT $ f a) s'
    --(\s ->
      --(sma s) >>= (\(a, s') ->
        --(runStateT $ f a) s'
      --)
    --)

instance MonadTrans (StateT s) where
  lift :: (Monad m) => m a -> StateT s m a
  lift ma = StateT $ \s -> fmap (flip (,) $ s) ma

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO = lift . liftIO
