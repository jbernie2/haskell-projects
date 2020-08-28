module StreamProcessor where

import CLI (CatArgs(..))
import Data.Char (isSpace)

data StreamProcessor = StreamProcessor
  { input :: [String]
  , output :: [String]
  , lineNumber :: Int
  }

init :: [String] -> StreamProcessor
init i = StreamProcessor
  { input = i
  , output = []
  , lineNumber = 1
  }

process :: CatArgs -> StreamProcessor -> StreamProcessor
process config streamProcessor =
  (numberLines config)
    (condenseLines config streamProcessor)

numberLines :: CatArgs -> StreamProcessor -> StreamProcessor
numberLines _config sp@(StreamProcessor [] _ _) = sp
numberLines config (StreamProcessor (i:is) o ln) =
  if argNumberNonEmptyLines config then
    if isEmptyLine i then
      StreamProcessor
        { input = is
        , output = o ++ [i]
        , lineNumber = ln
        }
    else
      StreamProcessor
        { input = is
        , output = o ++ [(addLineNumber ln i)]
        , lineNumber = nextLineNumber ln
        }
  else if argNumberLines config then
    StreamProcessor
      { input = is
      , output = o ++ [(addLineNumber ln i)]
      , lineNumber = nextLineNumber ln
      }
  else 
    StreamProcessor
      { input = is
      , output = o ++ [i]
      , lineNumber = ln
      }
 where
  addLineNumber :: Int -> String -> String
  addLineNumber ln' str = show ln' ++ "\t" ++ str

  nextLineNumber :: Int -> Int
  nextLineNumber ln' = ln' + 1

condenseLines :: CatArgs -> StreamProcessor -> StreamProcessor
condenseLines config sp@(StreamProcessor (i1:i2:is) o ln) =
  if argSqueeze config &&  isEmptyLine i1 && isEmptyLine i2 then
    condenseLines
      config
      StreamProcessor
        { input = i1 : is
        , output = o
        , lineNumber = ln
        }
  else
    sp
condenseLines _config sp = sp

isEmptyLine :: String -> Bool
isEmptyLine = all isSpace

