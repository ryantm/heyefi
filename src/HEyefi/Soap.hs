{-# LANGUAGE OverloadedStrings #-}

module HEyefi.Soap
       ( handleSoapAction
       , soapAction
       , mkResponse )
       where

import           HEyefi.GetPhotoStatus (getPhotoStatusResponse)
import           HEyefi.Log (logInfo)
import           HEyefi.MarkLastPhotoInRoll (markLastPhotoInRollResponse)
import           HEyefi.StartSession (startSessionResponse)
import           HEyefi.Types (HEyefiM, HEyefiApplication)


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import           Control.Arrow ((>>>))
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Lazy (fromStrict)
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.ByteString.UTF8 (fromString)
import qualified Data.CaseInsensitive as CI
import           Data.List (find)
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Data.Time.Format (formatTime, rfc822DateFormat, defaultTimeLocale)
import           Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType,
                                  hServer,
                                  hContentLength,
                                  hDate,
                                  Header,
                                  HeaderName)
import Network.Wai ( responseLBS
                   , Request
                   , Response
                   , requestHeaders )
import           Text.HandsomeSoup (css)
import Text.XML.HXT.Core ( runX
                         , readString
                         , getText
                         , (/>))


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

mkResponse :: String -> HEyefiM Response
mkResponse responseBody = do
  t <- liftIO getCurrentTime
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

handleSoapAction :: SoapAction -> BL.ByteString -> HEyefiApplication
handleSoapAction StartSession body _ f = do
  logInfo "Got StartSession request"
  let xmlDocument = readString [] (toString body)
  let getTagText = \ s -> liftIO (runX (xmlDocument >>> css s /> getText))
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
  response <- mkResponse responseBody
  f response
handleSoapAction GetPhotoStatus _ _ f = do
  logInfo "Got GetPhotoStatus request"
  -- TODO: Check card credential here!
  responseBody <- getPhotoStatusResponse
  response <- mkResponse responseBody
  f response
handleSoapAction MarkLastPhotoInRoll _ _ f = do
  logInfo "Got MarkLastPhotoInRoll request"
  responseBody <- markLastPhotoInRollResponse
  response <- mkResponse responseBody
  f response
