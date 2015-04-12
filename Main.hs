{-# LANGUAGE OverloadedStrings #-}

module Main where

import HEyefi.Constant

import Network.Wai ( responseLBS
                   , Application
                   , requestMethod
                   , requestHeaders )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (Method (..))


main = do
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req f
  | requestMethod req == "GET" =
      putStrLn (show (requestHeaders req)) >>
      (f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!")
  | requestMethod req == "POST" =
      putStrLn (show (requestHeaders req)) >>
      (f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!")
