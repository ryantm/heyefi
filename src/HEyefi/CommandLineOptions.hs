module HEyefi.CommandLineOptions where

import HEyefi.Prelude
import HEyefi.Strings

import Data.Monoid ((<>))
import Data.Version (showVersion)
import Options.Applicative
import Options.Applicative.Types
import Paths_heyefi (version)


opts :: ParserInfo (Maybe ())
opts = info (helper <*> parser)
       ( fullDesc
         <> progDesc programDescription
         <> header programHeaderDescription )

parser :: Parser (Maybe ())
parser =
  flag' Nothing
    (long "version" <> short 'v' <> help "Show version")
    <|> NilP (Just (Just ()))

maybeOnlyPrintVersion :: IO () -> Maybe () -> IO ()
maybeOnlyPrintVersion _ Nothing = putStrLn ("heyefi " ++ showVersion version)
maybeOnlyPrintVersion continueWith _ = continueWith

handleOptionsThenMaybe :: IO () -> IO ()
handleOptionsThenMaybe continueWith =
  execParser opts >>= maybeOnlyPrintVersion continueWith
