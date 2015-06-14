module HEyefi.Log where

import Data.Time.Clock (getCurrentTime)
import Data.Time.ISO8601 (formatISO8601Millis)

data LogLevel = Info | Debug
              deriving (Eq, Show)

log' :: LogLevel -> String -> IO ()
log' ll s = do
  t <- getCurrentTime
  putStrLn (unwords [
                 "[" ++ formatISO8601Millis t ++ "]"
               , "[" ++ show ll ++ "]"
               , s])

logInfo :: LogLevel -> String -> IO ()
logInfo _ s = log' Info s

logDebug :: LogLevel -> String -> IO ()
logDebug Debug s = log' Debug s
logDebug _ _ = return ()
