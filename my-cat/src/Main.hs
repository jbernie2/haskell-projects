module Main where

import CLI (runCLIParser, CatArgs(..))
import StreamProcessor

main :: IO ()
main = processFile =<< runCLIParser

processFile :: CatArgs -> IO ()
processFile config = do
  streamProcessor <- StreamProcessor.init . lines <$> readFile (argFile config)
  processAndPrint config streamProcessor

processAndPrint :: CatArgs -> StreamProcessor -> IO ()
processAndPrint _ (StreamProcessor [] [] _) = pure ()
processAndPrint config (StreamProcessor i (o:os) ln) = do
  putStrLn o
  processAndPrint config $ StreamProcessor i os ln
processAndPrint config streamProcessor = do
  processAndPrint config $ StreamProcessor.process config streamProcessor

