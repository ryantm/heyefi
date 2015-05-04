module HEyefi.Config where

import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, retry)
import Data.HashMap.Strict ()
import Data.Text (Text)

import Data.Configurator (display, load, Worth (Required))

type MacAddress = Text
type UploadKey = Text

data CardConfig = HashMap MacAddress UploadKey

data Config = Config {
  cardMap :: CardConfig,
  uploadDirectory :: FilePath
}

waitForWake :: TVar (Maybe Int) -> IO ()
waitForWake wakeSig = atomically (
  do state <- readTVar wakeSig
     case state of
      Just _ -> writeTVar wakeSig Nothing
      Nothing -> retry)

monitorConfig :: FilePath -> TVar (Maybe Int) -> IO ()
monitorConfig configPath wakeSignal = do
  putStrLn "loading"
  config <- load [Required configPath]
  display config
  waitForWake wakeSignal
