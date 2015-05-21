{-# LANGUAGE OverloadedStrings #-}

module HEyefi.Config where

import HEyefi.Log (logInfo)

import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, retry)
import Control.Exception (finally, catches, Handler (..))
import Control.Exception (SomeException (..))
import Control.Monad (void)
import Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)

import Data.Configurator (display, load, Worth (Required), getMap)
import Data.Configurator.Types (ConfigError (ParseError))
import qualified Data.Configurator.Types as CT

type MacAddress = Text
type UploadKey = Text

type CardConfig = HM.HashMap MacAddress UploadKey

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

reloadConfig :: FilePath -> IO Config
reloadConfig configPath = do
  logInfo ("Trying to load configuration at " ++ configPath)
  catches (
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
         return emptyConfig
       Just l -> do
         putStrLn (show l)
         let (CT.List innerList) = l
         let (CT.List [CT.String macAddress, CT.String key]) = head innerList
         return Config { cardMap = HM.fromList [(macAddress, key)], uploadDirectory = ""}
    )
    [Handler (\(ParseError p msg) -> do
                 logInfo ("Error parsing configuration file at " ++
                          p ++
                          " with message: " ++
                          msg)
                 return emptyConfig),
     Handler (\(SomeException _) -> do
                 logInfo ("Could not find configuration file at " ++ configPath)
                 return emptyConfig)]
  where
    emptyConfig = Config { cardMap = HM.empty, uploadDirectory = ""}

-- Example config:
-- cards = [["0012342de4ce","e7403a0123402ca062"],["1234562d5678","12342a062"]]
-- upload_dir = "/data/annex/doxie/unsorted"
monitorConfig :: FilePath -> TVar (Maybe Int) -> IO ()
monitorConfig configPath wakeSignal =
  finally
    (void (reloadConfig configPath))
    (waitForWake wakeSignal)
