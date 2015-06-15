{-# LANGUAGE OverloadedStrings #-}

module HEyefi.Config where

import           HEyefi.Types (Config(..), CardConfig, SharedConfig(..), LogLevel(Info), cardMap, uploadDirectory, logLevel)
import           HEyefi.Log (logInfo)
import           HEyefi.Constant hiding (configPath)

import           Control.Concurrent.STM (TVar, readTVar, newTVar, writeTVar, atomically, retry)
import           Control.Exception (SomeException (..))
import           Control.Exception (finally, catches, Handler (..))
import           Data.Configurator (load, Worth (Required), getMap)
import           Data.Configurator.Types (Value, ConfigError (ParseError))
import qualified Data.Configurator.Types as CT
import           Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (mapMaybe)
import           Data.Text (Text, unpack, pack)

insertCard :: Text -> Text -> Config -> Config
insertCard macAddress uploadKey c = do
  Config {
    cardMap = HM.insert macAddress uploadKey (cardMap c),
    uploadDirectory = uploadDirectory c
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

getCardConfig :: LogLevel -> HM.HashMap CT.Name CT.Value -> IO CardConfig
getCardConfig globalLogLevel configMap = do
  let cards = HM.lookup "cards" configMap
  case cards of
   Nothing -> do
     logInfo globalLogLevel missingCardsDefinition
     return HM.empty
   Just l -> do
     case convertCardList l of
      Left msg -> do
        logInfo globalLogLevel msg
        return HM.empty
      Right cardList ->
        return (HM.fromList cardList)

convertUploadDirectory :: Value -> Either String FilePath
convertUploadDirectory (CT.String uploadDir) =
  Right (unpack uploadDir)
convertUploadDirectory _ =
  Left uploadDirFormatDoesNotMatch

getUploadDirectory :: LogLevel -> HM.HashMap CT.Name CT.Value -> IO FilePath
getUploadDirectory globalLogLevel configMap = do
  let uploadDir = HM.lookup "upload_dir" configMap
  case uploadDir of
   Nothing -> do
     logInfo globalLogLevel missingUploadDirDefinition
     return ""
   Just uD -> do
     case convertUploadDirectory uD of
      Left msg -> do
        logInfo globalLogLevel msg
        return ""
      Right path ->
        return path

reloadConfig :: LogLevel -> FilePath -> IO Config
reloadConfig globalLogLevel configPath = do
  logInfo globalLogLevel ("Trying to load configuration at " ++ configPath)
  catches (
    do
      config <- load [Required configPath]
      configMap <- getMap config
      cardConfig <- getCardConfig globalLogLevel configMap
      uploadDir <- getUploadDirectory globalLogLevel configMap
      logInfo globalLogLevel "Loaded configuration"
      return Config {
        cardMap = cardConfig,
        uploadDirectory = uploadDir }
    )
    [Handler (\(ParseError p msg) -> do
                 logInfo
                   globalLogLevel
                   ("Error parsing configuration file at " ++
                          p ++
                          " with message: " ++
                          msg)
                 return emptyConfig),
     Handler (\(SomeException _) -> do
                 logInfo
                   globalLogLevel
                   ("Could not find configuration file at " ++ configPath)
                 return emptyConfig)]

emptyConfig :: Config
emptyConfig = Config { cardMap = HM.empty
                     , uploadDirectory = ""
                     , logLevel = Info }

newConfig :: IO SharedConfig
newConfig = atomically (newTVar emptyConfig)

-- Example config:
-- cards = [["0012342de4ce","e7403a0123402ca062"],["1234562d5678","12342a062"]]
-- upload_dir = "/data/annex/doxie/unsorted"
monitorConfig :: LogLevel ->
                 FilePath -> SharedConfig -> TVar (Maybe Int) -> IO ()
monitorConfig globalLogLevel configPath sharedConfig wakeSignal =
  finally
    (do
        config <- reloadConfig globalLogLevel configPath
        atomically (writeTVar sharedConfig config))
    (waitForWake wakeSignal)

getUploadKeyForMacaddress :: Config -> String -> Maybe String
getUploadKeyForMacaddress c mac =
  (fmap unpack (HM.lookup (pack mac) (cardMap c)))
