module HEyefi.Log where
import           HEyefi.Types (HEyefiM, LogLevel(..), logLevel)
import           HEyefi.Prelude

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Lazy (get)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.ISO8601 (formatISO8601Millis)

log' :: LogLevel -> Text -> IO ()
log' ll s = do
  t <- getCurrentTime
  T.putStrLn (T.unwords [
                 "[" <> T.pack (formatISO8601Millis t) <> "]"
               , "[" <> T.pack (show ll) <> "]"
               , s])

logInfoIO :: Text -> IO ()
logInfoIO = log' Info

logInfo ::  Text -> HEyefiM ()
logInfo = liftIO . logInfoIO

logDebug :: Text -> HEyefiM ()
logDebug s = do
  config <- get
  case logLevel config of
   Debug -> liftIO (log' Debug s)
   _ -> return ()
