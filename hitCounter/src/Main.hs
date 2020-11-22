{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
import Network.Wai (Response)

data Config = Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text 
}

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  (newMap, newVal)
 where
  newMap = M.insertWith (+) k 1 m
  newVal = M.findWithDefault 1 k newMap

app :: Scotty ()
app = 
  get "/:key" $ do
    prefix' <- lift $ prefix <$> ask
    unprefixed <- param "key"
    let key' = mappend prefix' unprefixed

    counterRef <- lift $ counts <$> ask
    (newMap, newInteger) <- liftIO $ bumpBoomp key' <$> (readIORef counterRef)
    _ <- liftIO $ writeIORef counterRef newMap
 
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty

  scottyT 3000 (runR $ config prefixArg counter) app

 where
  config :: [Char] -> IORef (M.Map Text Integer) -> Config
  config prefixArg counter = Config
    { counts = counter
    , prefix = TL.pack prefixArg
    }

  runR :: Config -> ReaderT Config IO Response -> IO Response
  runR conf x = runReaderT x conf

