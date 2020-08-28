module CLI 
  ( runCLIParser
  , CatArgs(..)
  )
where

import Options.Applicative

data CatArgs = CatArgs
  { argFile                :: FilePath
  , argNumberLines         :: Bool
  , argNumberNonEmptyLines :: Bool
  , argSqueeze             :: Bool
  }

runCLIParser :: IO CatArgs
runCLIParser = 
  execParser opts
 where
  opts = info (catArgs <**> helper)
    ( fullDesc
   <> progDesc "Print files to stdout"
   <> header "my-cat print files to stdout" )


catArgs :: Parser CatArgs
catArgs = CatArgs
      <$> strOption
          ( long "file"
         <> short 'f'
         <> metavar "FILENAME"
         <> help "file to process" )
      <*> switch
          ( short 'n'
         <> help "number of lines (starting at 1)" )
      <*> switch
          ( short 'b'
         <> help "number of non-empty lines (starting at 1)" )
      <*> switch
          ( short 's'
         <> help "condense adjacent empty lines into single empty line" )
          
