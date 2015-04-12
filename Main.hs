{-# LANGUAGE OverloadedStrings #-}

module Main where

import HEyefi.Constant

import Data.Time.Clock
import Data.Time.ISO8601
import Network.Wai ( responseLBS
                   , Application
                   , requestMethod
                   , requestHeaders )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (Method (..))



logInfo :: String -> IO ()
logInfo s = do
  t <- getCurrentTime
  putStrLn (unwords ["[" ++ formatISO8601Millis t ++ "]", "[INFO]", s])

main = do
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req f
  | requestMethod req == "POST" =
      logInfo (show (requestHeaders req)) >>
      (f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!")
