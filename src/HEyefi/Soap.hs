{-# LANGUAGE OverloadedStrings #-}

module HEyefi.Soap
       ( handleSoapAction
       , soapAction
       , mkResponse )
       where

import HEyefi.Config (SharedConfig)
import HEyefi.StartSession (startSessionResponse)
import HEyefi.GetPhotoStatus (getPhotoStatusResponse)
import HEyefi.Log (logInfo)
import HEyefi.MarkLastPhotoInRoll (markLastPhotoInRollResponse)


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.List (find)
import Data.ByteString.Lazy.UTF8 (toString)
import Text.HandsomeSoup (css)
import Control.Arrow ((>>>))
import Text.XML.HXT.Core ( runX
                         , readString
                         , getText
                         , (/>))
import Control.Concurrent.STM (atomically, readTVar)
import Data.Time.Format (formatTime, rfc822DateFormat, defaultTimeLocale)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types.Header (hContentType,
                                  hServer,
                                  hContentLength,
                                  hDate,
                                  Header,
                                  HeaderName)
import Network.HTTP.Types (status200)
import Network.Wai ( responseLBS
                   , Request
                   , Application
                   , Response
                   , requestHeaders )
import Data.Time.Clock (getCurrentTime, UTCTime)


data SoapAction = StartSession
                | GetPhotoStatus
                | MarkLastPhotoInRoll
                deriving (Show, Eq)

headerIsSoapAction :: Header -> Bool
headerIsSoapAction ("SOAPAction",_) = True
headerIsSoapAction _ = False

soapAction :: Request -> Maybe SoapAction
soapAction req =
  case find headerIsSoapAction (requestHeaders req) of
   Just (_,"\"urn:StartSession\"") -> Just StartSession
   Just (_,"\"urn:GetPhotoStatus\"") -> Just GetPhotoStatus
   Just (_,"\"urn:MarkLastPhotoInRoll\"") -> Just MarkLastPhotoInRoll
   Just (_,sa) -> error ((show sa) ++ " is not a defined SoapAction yet")
   _ -> Nothing

mkResponse :: String -> IO Response
mkResponse responseBody = do
  t <- getCurrentTime
  return (responseLBS
          status200
          (defaultResponseHeaders t (length responseBody))
          (fromStrict (fromString responseBody)))

defaultResponseHeaders :: UTCTime ->
                          Int ->
                          [(HeaderName, B.ByteString)]
defaultResponseHeaders time size =
  [ (hContentType, "text/xml; charset=\"utf-8\"")
  , (hDate, fromString (formatTime defaultTimeLocale rfc822DateFormat time))
  , (CI.mk "Pragma", "no-cache")
  , (hServer, "Eye-Fi Agent/2.0.4.0 (Windows XP SP2)")
  , (hContentLength, fromString (show size))]

handleSoapAction :: SoapAction -> SharedConfig -> BL.ByteString -> Application
handleSoapAction StartSession config body _ f = do
  logInfo "Got StartSession request"
  let xmlDocument = readString [] (toString body)
  let getTagText = \ s -> runX (xmlDocument >>> css s /> getText)
  macaddress <- getTagText "macaddress"
  cnonce <- getTagText "cnonce"
  transfermode <- getTagText "transfermode"
  transfermodetimestamp <- getTagText "transfermodetimestamp"
  logInfo (show macaddress)
  logInfo (show transfermodetimestamp)
  config' <- atomically (readTVar config)
  responseBody <- (startSessionResponse
                   config'
                   (head macaddress)
                   (head cnonce)
                   (head transfermode)
                   (head transfermodetimestamp))
  logInfo (show responseBody)
  response <- mkResponse responseBody
  f response
handleSoapAction GetPhotoStatus _ _ _ f = do
  logInfo "Got GetPhotoStatus request"
  -- TODO: Check card credential here!
  responseBody <- getPhotoStatusResponse
  response <- mkResponse responseBody
  f response
handleSoapAction MarkLastPhotoInRoll _ _ _ f = do
  logInfo "Got MarkLastPhotoInRoll request"
  responseBody <- markLastPhotoInRollResponse
  response <- mkResponse responseBody
  f response
