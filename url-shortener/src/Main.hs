{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit) 

shortURLGen :: IO [Char]
shortURLGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.setnx shortURI uri
    >>= 
      (\e -> pure $ 
        fmap (\bool -> if bool then R.Ok else (R.Status "Key collision")) e
      )

getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply
              (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

linkShortURL :: String -> String
linkShortURL shortURL =
  concat
  [ "<a href=\""
  , shortURL
  , "\">Copy and paste your short URL</a>"
  ]

shortURLCreated :: Show a
              => a
              -> String
              -> TL.Text
shortURLCreated resp shortURL =
  TL.concat [ TL.pack (show resp)
            , " shortURL is: "
            , TL.pack (linkShortURL shortURL)
            ]

badURI :: TL.Text -> TL.Text
badURI uri =
  TL.concat
    [ uri
    , " wasn't a url,"
    , " did you forget http://?"
    ]

shortURLFound :: TL.Text -> TL.Text
shortURLFound tbs =
  TL.concat
    [ "<a href=\""
    , tbs, "\">"
    , tbs, "</a>"
    ]

app :: R.Connection
    -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    let parsedURI :: Maybe URI
        parsedURI =
          parseURI (TL.unpack uri)

    case parsedURI of
      Just _ -> do
        shortURL <- liftIO shortURLGen
        let shortURL' = BC.pack shortURL
            uri' = encodeUtf8 (TL.toStrict uri)

        resp <- liftIO (saveURI rConn shortURL' uri')

        html (shortURLCreated resp shortURL)

      Nothing ->
          text (badURI uri)

  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)

    case uri of
      Left reply ->
        text (TL.pack (show reply))

      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"

        Just bs -> html (shortURLFound tbs)
          where tbs :: TL.Text
                tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
