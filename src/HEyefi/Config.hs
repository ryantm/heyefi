{-# LANGUAGE OverloadedStrings #-}

module HEyefi.Config where

import HEyefi.Log (logInfo)
import HEyefi.Constant hiding (configPath)

import Control.Concurrent.STM (TVar, readTVar, newTVar, writeTVar, atomically, retry)
import Control.Exception (finally, catches, Handler (..))
import Control.Exception (SomeException (..))
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HM

import Data.Text (Text, unpack)

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
convertCardList _ = Left cardsFormatDoesNotMatch

getCardConfig :: HM.HashMap CT.Name CT.Value -> IO CardConfig
getCardConfig configMap = do
  let cards = HM.lookup "cards" configMap
  case cards of
   Nothing -> do
     logInfo missingCardsDefinition
     return HM.empty
   Just l -> do
     case convertCardList l of
      Left msg -> do
        logInfo msg
        return HM.empty
      Right cardList ->
        return (HM.fromList cardList)

convertUploadDirectory :: Value -> Either String FilePath
convertUploadDirectory (CT.String uploadDir) =
  Right (unpack uploadDir)
convertUploadDirectory _ =
  Left uploadDirFormatDoesNotMatch

getUploadDirectory :: HM.HashMap CT.Name CT.Value -> IO FilePath
getUploadDirectory configMap = do
  let uploadDir = HM.lookup "upload_dir" configMap
  case uploadDir of
   Nothing -> do
     logInfo missingUploadDirDefinition
     return ""
   Just uD -> do
     case convertUploadDirectory uD of
      Left msg -> do
        logInfo msg
        return ""
      Right path ->
        return path

reloadConfig :: FilePath -> IO Config
reloadConfig configPath = do
  logInfo ("Trying to load configuration at " ++ configPath)
  catches (
    do
      config <- load [Required configPath]
      configMap <- getMap config
      cardConfig <- getCardConfig configMap
      uploadDir <- getUploadDirectory configMap
      return Config {
        cardMap = cardConfig,
        uploadDirectory = uploadDir }
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

emptyConfig :: Config
emptyConfig = Config { cardMap = HM.empty, uploadDirectory = ""}

newConfig :: IO (TVar Config)
newConfig = atomically (newTVar emptyConfig)

-- Example config:
-- cards = [["0012342de4ce","e7403a0123402ca062"],["1234562d5678","12342a062"]]
-- upload_dir = "/data/annex/doxie/unsorted"
monitorConfig :: FilePath -> TVar Config -> TVar (Maybe Int) -> IO ()
monitorConfig configPath sharedConfig wakeSignal =
  finally
    (do
        config <- reloadConfig configPath
        atomically (writeTVar sharedConfig config))
    (waitForWake wakeSignal)
