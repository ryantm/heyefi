module HEyefi.Log where

import Data.Time.Clock (getCurrentTime)
import Data.Time.ISO8601 (formatISO8601Millis)

logInfo :: String -> IO ()
logInfo s = do
  t <- getCurrentTime
  putStrLn (unwords ["[" ++ formatISO8601Millis t ++ "]", "[INFO]", s])
