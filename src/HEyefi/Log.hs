module HEyefi.Log where

import HEyefi.Types (HEyefiM, LogLevel(..), logLevel)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Time.Clock (getCurrentTime)
import Data.Time.ISO8601 (formatISO8601Millis)

log' :: LogLevel -> String -> IO ()
log' ll s = do
  t <- getCurrentTime
  putStrLn (unwords [
                 "[" ++ formatISO8601Millis t ++ "]"
               , "[" ++ show ll ++ "]"
               , s])

logInfo ::  String -> HEyefiM ()
logInfo s = liftIO (log' Info s)

logDebug :: String -> HEyefiM ()
logDebug s = do
  config <- ask
  let ll = logLevel config
  case ll of
   Debug -> liftIO (log' Debug s)
   _ -> return ()
