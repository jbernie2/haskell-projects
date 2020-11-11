{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import Control.Monad.Trans.Class

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do 
    beam <- param "word"
    lift $ putStrLn "hello"
    html $
      mconcat ["<h1>Scotty, "
              , beam
              , " me up!</h1>"]

