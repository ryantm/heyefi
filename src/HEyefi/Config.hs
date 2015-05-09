{-# LANGUAGE OverloadedStrings #-}

module HEyefi.Config where

import HEyefi.Log (logInfo)

import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, retry)
import Control.Exception.Base (finally, catch)
import Control.Exception (SomeException (..))
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

reloadConfig :: FilePath -> IO ()
reloadConfig configPath = do
  logInfo ("Loading configuration at " ++ configPath)
  catch (
    do
      config <- load [Required configPath]
      display config
      configMap <- getMap config -- C.lookup config "cards" :: IO (Maybe [Text])
      let cards = HM.lookup "cards" configMap
      case cards of
       Nothing -> do
         logInfo ("Configuration file at " ++
                  configPath ++
                  " is missing a definition for `cards`.")
       Just l -> do
         putStrLn (show l))
    handleError
  where
    handleError (SomeException _) =
      logInfo ("Could not find configuration file at " ++ configPath)

-- Example config:
-- cards = [["0012342de4ce","e7403a0123402ca062"],["1234562d5678","12342a062"]]
-- upload_dir = "/data/annex/doxie/unsorted"
monitorConfig :: FilePath -> TVar (Maybe Int) -> IO ()
monitorConfig configPath wakeSignal =
  finally
    (reloadConfig configPath)
    (waitForWake wakeSignal)
