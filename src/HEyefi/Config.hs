{-# LANGUAGE OverloadedStrings #-}

module HEyefi.Config where

import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, retry)
import Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)

import Data.Configurator (display, load, Worth (Required), getMap)

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

-- Example config:
-- cards = [["0012342de4ce","e7403a0123402c9159892a062"],["1234562d5678","12345e4e7403a0586702c9123452a062"]]
-- upload_dir = "/data/annex/doxie/unsorted"
monitorConfig :: FilePath -> TVar (Maybe Int) -> IO ()
monitorConfig configPath wakeSignal = do
  putStrLn "loading"
  config <- load [Required configPath]
  display config
  configMap <- getMap config -- C.lookup config "cards" :: IO (Maybe [Text])
  let cards = HM.lookup "cards" configMap
  putStrLn (show cards)
  waitForWake wakeSignal
