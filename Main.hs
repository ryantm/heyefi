{-# LANGUAGE OverloadedStrings #-}

module Main where

import HEyefi.Constant

import Data.ByteString.UTF8 (toString)
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Data.Time.Clock
import Data.Time.ISO8601
import Network.Wai ( responseLBS
                   , Application
                   , Request
                   , pathInfo
                   , requestBody
                   , requestMethod
                   , requestHeaders )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType, Header)
import Network.HTTP.Types.Method (Method (..))



logInfo :: String -> IO ()
logInfo s = do
  t <- getCurrentTime
  putStrLn (unwords ["[" ++ formatISO8601Millis t ++ "]", "[INFO]", s])

main = do
    logInfo ("Listening on port " ++ show port)
    run port app


                -- self.send_response(200)
                -- self.send_header('Date', self.date_time_string())
                -- self.send_header('Pragma','no-cache')
                -- self.send_header('Server','Eye-Fi Agent/2.0.4.0 (Windows XP SP2)')
                -- self.send_header('Content-Type','text/xml; charset="utf-8"')
                -- self.send_header('Content-Length', contentLength)

data SoapAction = StartSession
                  deriving (Show, Eq)

headerIsSoapAction :: Header -> Bool
headerIsSoapAction ("SOAPAction",_) = True
headerIsSoapAction _ = False

soapAction :: Request -> Maybe SoapAction
soapAction req =
  case find headerIsSoapAction (requestHeaders req) of
   Just (_,"\"urn:StartSession\"") -> Just StartSession
   Just (_,sa) -> error ((show sa) ++ " is not a defined SoapAction yet")
   _ -> Nothing

dispatchRequest :: Application
dispatchRequest req f
  | requestMethod req == "POST" &&
    isJust (soapAction req) &&
    fromJust (soapAction req) == StartSession = do
      logInfo "Got StartSession request"
      f (responseLBS status200 [(hContentType, "text/plain")] "Hello world!")

app :: Application
app req f = do
  body <- requestBody req
  logInfo (show (pathInfo req))
  logInfo (show (requestHeaders req))
  logInfo (show (toString body))
  dispatchRequest req f
