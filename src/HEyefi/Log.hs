module HEyefi.Log where

import HEyefi.Types (HEyefiM, LogLevel(..), logLevel)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (get)
import Data.Time.Clock (getCurrentTime)
import Data.Time.ISO8601 (formatISO8601Millis)

log' :: LogLevel -> String -> IO ()
log' ll s = do
  t <- getCurrentTime
  putStrLn (unwords [
                 "[" ++ formatISO8601Millis t ++ "]"
               , "[" ++ show ll ++ "]"
               , s])

logInfoIO :: String -> IO ()
logInfoIO = log' Info

logInfo ::  String -> HEyefiM ()
logInfo s = liftIO (log' Info s)

logDebug :: String -> HEyefiM ()
logDebug s = do
  config <- get
  case logLevel config of
   Debug -> liftIO (log' Debug s)
   _ -> return ()
