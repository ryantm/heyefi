{-# LANGUAGE OverloadedStrings #-}

module Main where

import HEyefi.Constant

import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.UTF8 (toString, fromString)
import Data.ByteString.UTF8 (toString, fromString)
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Data.Time.Clock
import Data.Time.Format (formatTime)
import Data.Time.ISO8601
import System.Locale (rfc822DateFormat, defaultTimeLocale)
import Network.Wai ( responseLBS
                   , Application
                   , Request
                   , pathInfo
                   , requestBody
                   , requestMethod
                   , requestHeaders )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType,
                                  hServer,
                                  hContentLength,
                                  hDate,
                                  Header)
import Network.HTTP.Types.Method (Method (..))
import Text.XML.HXT.Core (runX, readString, getText, (/>), mkelem, sattr, txt, yes, withIndent, root, writeDocumentToString)
import Text.HandsomeSoup (css)
import Control.Arrow ((>>>),  (&&&))
import Data.Hex
import Data.Hash.MD5 (md5s, Str (..))
import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI


logInfo :: String -> IO ()
logInfo s = do
  t <- getCurrentTime
  putStrLn (unwords ["[" ++ formatISO8601Millis t ++ "]", "[INFO]", s])

main = do
    logInfo ("Listening on port " ++ show port)
    run port app

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

startSessionResponse :: String -> String -> String -> String -> IO String
startSessionResponse macaddress cnonce transfermode transfermodetimestamp = do
  bcs <- binaryCredentialString
  let document =
        mkelem "SOAP-ENV:Envelope"
        [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
        [ mkelem "SOAP-ENV:Body" []
          [ mkelem "StartSessionResponse"
            [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
            [ mkelem "credential" [] [ txt bcs ]
            , mkelem "snonce" [] [ txt "340282366920938463463374607431768211456" ]
            , mkelem "transfermode" [] [ txt transfermode ]
            , mkelem "transfermodetimestamp" [] [ txt transfermodetimestamp ]
            , mkelem "upsyncallowed" [] [ txt "true" ]
            ]
          ]
        ]
  result <- (runX (root [] [document] >>> writeDocumentToString []))
  return (head result)
  where
    credential bcs = md5s (Str (bcs))
    binaryCredentialString = unhex credentialString
    credentialString :: String
    credentialString = macaddress ++ cnonce ++ upload_key_0

dispatchRequest :: String -> Application
dispatchRequest body req f
  | requestMethod req == "POST" &&
    isJust (soapAction req) &&
    fromJust (soapAction req) == StartSession = do
      logInfo "Got StartSession request"
      -- ["fileid","filename","filesize","filesignature"]
      let xmlDocument = readString [] body
      let getTagText = \ s -> runX (xmlDocument >>> css s /> getText)
      macaddress <- getTagText "macaddress"
      cnonce <- getTagText "cnonce"
      transfermode <- getTagText "transfermode"
      transfermodetimestamp <- getTagText "transfermodetimestamp"
      logInfo (show macaddress)
      logInfo (show transfermodetimestamp)
      responseBody <- (startSessionResponse
                       (head macaddress)
                       (head cnonce)
                       (head transfermode)
                       (head transfermodetimestamp))
      logInfo (show responseBody)
      t <- getCurrentTime
      f (responseLBS
         status200
         [ (hContentType, "text/xml; charset=\"utf-8\"")
         , (hDate, fromString (formatTime defaultTimeLocale rfc822DateFormat t))
         , (CI.mk "Pragma", "no-cache")
         , (hServer, "Eye-Fi Agent/2.0.4.0 (Windows XP SP2)")
         , (hContentLength, fromString (show (length responseBody)))] (fromStrict (fromString responseBody)))

app :: Application
app req f = do
  body <- requestBody req
  logInfo (show (pathInfo req))
  logInfo (show (requestHeaders req))
  logInfo (show (toString body))
  dispatchRequest (toString body) req f
