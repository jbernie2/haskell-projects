module MonadTransformers.SameDifference where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader 

-- MaybeT :: m (Maybe a) -> MaybeT m a
-- ReaderT :: (r -> ma ) -> ReaderT r m a
--
-- not really the types, but you basically have a Monad
-- Transformer wrapped around a function that takes one parameter
-- and returns a `Maybe a`
-- MaybeT (Reader String) a ~ MaybeT (\r -> Just a)
-- ReaderT String Maybe a ~ ReaderT (\r -> Just a)

maybeTReader :: a -> MaybeT (Reader String) a
maybeTReader a = MaybeT $ return (Just a)

readerTMaybe :: a -> ReaderT String Maybe a
readerTMaybe a = ReaderT $ return (Just a)

isOne :: MaybeT (Reader String) Int -> MaybeT (Reader String) Bool
isOne = fmap (==1)  

getVal :: MaybeT (Reader String) Bool -> Maybe Bool
getVal (MaybeT rma) = runReader rma ""

isOne' :: ReaderT String Maybe Int -> ReaderT String Maybe Bool
isOne' = fmap (==1) 

getVal' :: ReaderT String Maybe Bool -> Maybe Bool
getVal' (ReaderT rma) = rma ""

main :: IO ()
main = do
  putStrLn . show . getVal . isOne $ maybeTReader 1
  putStrLn . show . getVal . isOne $ maybeTReader 0

  putStrLn . show . getVal' . isOne' $ readerTMaybe 1
  putStrLn . show . getVal' . isOne' $ readerTMaybe 0
