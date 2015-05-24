{-# LANGUAGE OverloadedStrings #-}

module HEyefi.Config where

import HEyefi.Log (logInfo)

import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, retry)
import Control.Exception (finally, catches, Handler (..))
import Control.Exception (SomeException (..))
import Control.Monad (void)
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)

import Data.Configurator (load, Worth (Required), getMap)
import Data.Configurator.Types (Value, ConfigError (ParseError))
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

convertCardList :: Value -> Either String [(Text, Text)]
convertCardList (CT.List innerList) =
  Right (mapMaybe extractTuple innerList)
  where
    extractTuple :: Value -> Maybe (Text, Text)
    extractTuple (CT.List [CT.String macAddress, CT.String key]) =
      Just (macAddress, key)
    extractTuple _ = Nothing
convertCardList _ = Left "Format of cards does not match [[MacAddress, Key],[MacAddress, Key],...]."

reloadConfig :: FilePath -> IO Config
reloadConfig configPath = do
  logInfo ("Trying to load configuration at " ++ configPath)
  catches (
    do
      config <- load [Required configPath]
      configMap <- getMap config
      let cards = HM.lookup "cards" configMap
      case cards of
       Nothing -> do
         logInfo ("Configuration file at " ++
                  configPath ++
                  " is missing a definition for `cards`.")
         return emptyConfig
       Just l -> do
         case (convertCardList l) of
          (Right cardList) ->
            return Config {
              cardMap = HM.fromList cardList, uploadDirectory = ""}
          (Left msg) -> do
            logInfo msg
            return emptyConfig
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
