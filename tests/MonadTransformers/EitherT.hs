module MonadTransformers.EitherT where

import Control.Monad.Trans.Class

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT . pure $ pure a
  (EitherT fab) <*> (EitherT mea) = EitherT $ (<*>) <$> fab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f = EitherT $
    mea >>= (\ea ->
      case ea of
        Left e ->
          return $ Left e
        Right a ->
          runEitherT $ f a
    )

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right


swapEither :: Either e a -> Either a e
swapEither eEA =
  case eEA of
    Left e ->
      Right e
    Right a ->
      Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT amc bmc (EitherT me) =
  me >>= (\e ->
    case e of
      Left a ->
        amc a
      Right b ->
        bmc b
  )
