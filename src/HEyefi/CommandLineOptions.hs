module HEyefi.CommandLineOptions where

import Options.Applicative
import Options.Applicative.Types

opts :: ParserInfo (Maybe ())
opts = info (helper <*> parser)
       ( fullDesc
         <> progDesc "Print a greeting for TARGET"
         <> header "hello - a test for optparse-applicative" )

parser :: Parser (Maybe ())
parser =
  flag' Nothing
    (long "version" <> short 'v' <> hidden)
    <|> (NilP (Just (Just ())))
