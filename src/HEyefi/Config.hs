module HEyefi.Config where

import           HEyefi.Log (logInfo)
import           HEyefi.Prelude
import           HEyefi.Types (
    Config(..)
  , CardConfig
  , SharedConfig
  , LogLevel(Info)
  , cardMap
  , uploadDirectory
  , logLevel
  , HEyefiM(..))

import           Control.Concurrent.STM (
    TVar
  , readTVar
  , newTVar
  , writeTVar
  , atomically
  , retry)
import           Control.Monad.Catch (
    finally
  , catches
  , Handler (..)
  , SomeException (..))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Lazy (modify, get, runStateT)
import           Data.Configurator (load, Worth (Required), getMap)
import           Data.Configurator.Types (Value, ConfigError (ParseError))
import qualified Data.Configurator.Types as CT
import           Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

insertCard :: Text -> Text -> Config -> Config
insertCard macAddress uploadKey c =
  Config {
    cardMap = HM.insert macAddress uploadKey (cardMap c),
    uploadDirectory = uploadDirectory c,
    logLevel = logLevel c,
    lastSNonce = lastSNonce c
    }


waitForWake :: TVar (Maybe Int) -> HEyefiM ()
waitForWake wakeSig = liftIO (atomically (
  do state <- readTVar wakeSig
     case state of
      Just _ -> writeTVar wakeSig Nothing
      Nothing -> retry))

convertCardList :: Value -> Either Text [(Text, Text)]
convertCardList (CT.List innerList) =
  Right (mapMaybe extractTuple innerList)
  where
    extractTuple :: Value -> Maybe (Text, Text)
    extractTuple (CT.List [CT.String macAddress, CT.String key]) =
      Just (macAddress, key)
    extractTuple _ = Nothing
convertCardList _ = Left cardsFormatDoesNotMatch

getCardConfig :: HM.HashMap CT.Name CT.Value -> HEyefiM CardConfig
getCardConfig configMap = do
  let cards = HM.lookup "cards" configMap
  case cards of
   Nothing -> do
     logInfo missingCardsDefinition
     return HM.empty
   Just l ->
     case convertCardList l of
      Left msg -> do
        logInfo msg
        return HM.empty
      Right cardList ->
        return (HM.fromList cardList)

convertUploadDirectory :: Value -> Either Text FilePath
convertUploadDirectory (CT.String uploadDir) =
  Right (T.unpack uploadDir)
convertUploadDirectory _ =
  Left uploadDirFormatDoesNotMatch

getUploadDirectory :: HM.HashMap CT.Name CT.Value -> HEyefiM FilePath
getUploadDirectory configMap = do
  let uploadDir = HM.lookup "upload_dir" configMap
  case uploadDir of
   Nothing -> do
     logInfo missingUploadDirDefinition
     return ""
   Just uD ->
     case convertUploadDirectory uD of
      Left msg -> do
        logInfo msg
        return ""
      Right path ->
        return path

reloadConfig :: FilePath -> HEyefiM Config
reloadConfig configPath = do
  logInfo (tryingToLoadConfiguration (T.pack configPath))
  catches (
    do
      config <- liftIO (load [Required configPath])
      configMap <- liftIO (getMap config)
      cardConfig <- getCardConfig configMap
      uploadDir <- getUploadDirectory configMap
      logInfo loadedConfiguration
      return Config {
        cardMap = cardConfig,
        uploadDirectory = uploadDir,
        logLevel = Info,
        lastSNonce = "" } -- TODO: Careful, we might be erasing something here.
    )
    [Handler (\(ParseError p msg) -> do
                 logInfo (errorParsingConfigurationFile (T.pack p) (T.pack msg))
                 return emptyConfig),
     Handler (\(SomeException _) -> do
                 logInfo (couldNotFindConfigurationFile (T.pack configPath))
                 return emptyConfig)]

runWithConfig :: Config -> HEyefiM a -> IO (a,Config)
runWithConfig c m = runStateT (runHeyefi m) c

runWithEmptyConfig :: HEyefiM a -> IO (a,Config)
runWithEmptyConfig = runWithConfig emptyConfig

emptyConfig :: Config
emptyConfig = Config { cardMap = HM.empty
                     , uploadDirectory = ""
                     , logLevel = Info
                     , lastSNonce = ""}

newConfig :: IO SharedConfig
newConfig = atomically (newTVar emptyConfig)

-- Example config:
-- cards = [["0012342de4ce","e7403a0123402ca062"],["1234562d5678","12342a062"]]
-- upload_dir = "/data/annex/doxie/unsorted"
monitorConfig :: FilePath -> SharedConfig -> TVar (Maybe Int) -> HEyefiM ()
monitorConfig configPath sharedConfig wakeSignal =
  finally
    (do
        config <- reloadConfig configPath
        liftIO (atomically (writeTVar sharedConfig config)))
    (waitForWake wakeSignal)

getUploadKeyForMacaddress :: Text -> HEyefiM (Maybe Text)
getUploadKeyForMacaddress mac = do
  c <- get
  return (HM.lookup mac (cardMap c))

putSNonce :: Text -> HEyefiM ()
putSNonce snonce =
  modify (\ s ->
            s { lastSNonce = snonce })
