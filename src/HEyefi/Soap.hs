module HEyefi.Soap where

import           Control.Arrow ((>>>))
import           Control.Monad (join)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Lazy (get)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.ByteString.UTF8 (fromString)
import qualified Data.CaseInsensitive as CI
import           Data.List (find)
import           Data.Maybe (fromJust, isJust)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Data.Time.Format (
    formatTime
  , rfc822DateFormat
  , defaultTimeLocale )
import           HEyefi.Config (getUploadKeyForMacaddress)
import           HEyefi.Hex (unhex)
import           HEyefi.Log (logInfo, logDebug)
import           HEyefi.Prelude
import           HEyefi.SoapResponse (
    markLastPhotoInRollResponse
  , getPhotoStatusResponse )
import           HEyefi.StartSession (startSessionResponse)
import           HEyefi.Types
import           Network.HTTP.Types (status200, unauthorized401)
import           Network.HTTP.Types.Header (
    hContentType
  , hServer
  , hContentLength
  , hDate
  , Header
  , HeaderName )
import           Network.Wai (
    responseLBS
  , Request
  , Response
  , requestHeaders )
import           Text.HandsomeSoup (css)
import           Text.XML.HXT.Core (
    getText
  , (/>)
  , runLA
  , xreadDoc )

soapActionHeaderName :: HeaderName
soapActionHeaderName = CI.mk "SoapAction"

headerToSoapAction :: Header -> Maybe SoapAction
headerToSoapAction h |
  h == (soapActionHeaderName, "\"urn:StartSession\"") = Just StartSession
headerToSoapAction h |
  h == (soapActionHeaderName, "\"urn:GetPhotoStatus\"") = Just GetPhotoStatus
headerToSoapAction h |
  h == (soapActionHeaderName, "\"urn:MarkLastPhotoInRoll\"") =
    Just MarkLastPhotoInRoll
headerToSoapAction _ = Nothing

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = join . find isJust . map f

soapAction :: Request -> Maybe SoapAction
soapAction req = firstJust headerToSoapAction (requestHeaders req)

mkResponse :: Text -> HEyefiM Response
mkResponse responseBody = do
  t <- liftIO getCurrentTime
  return (responseLBS
          status200
          (defaultResponseHeaders t (tlength responseBody))
          (fromStrict (encodeUtf8 responseBody)))

mkUnauthorizedResponse :: Response
mkUnauthorizedResponse = responseLBS unauthorized401 [] ""

defaultResponseHeaders :: UTCTime ->
                          Int ->
                          [(HeaderName, B.ByteString)]
defaultResponseHeaders time size =
  [ (hContentType, "text/xml; charset=\"utf-8\"")
  , (hDate, fromString (formatTime defaultTimeLocale rfc822DateFormat time))
  , (CI.mk "Pragma", "no-cache")
  , (hServer, "Eye-Fi Agent/2.0.4.0 (Windows XP SP2)")
  , (hContentLength, fromString (show size))]

firstTag :: BL.ByteString ->
            Text ->
            Text
firstTag body tagName =
  pack (head (runLA (xreadDoc >>> css (unpack tagName) /> getText)
                (toString body)))

handleSoapAction :: SoapAction -> BL.ByteString -> HEyefiApplication
handleSoapAction StartSession body _ f = do
  logDebug gotStartSessionRequest
  let tag = firstTag body
  let macaddress = tag "macaddress"
  let cnonce = tag "cnonce"
  let transfermode = tag "transfermode"
  let transfermodetimestamp = tag "transfermodetimestamp"
  logDebug macaddress
  logDebug transfermodetimestamp
  responseBody <- startSessionResponse
                   macaddress
                   cnonce
                   transfermode
                   transfermodetimestamp
  logDebug responseBody
  response <- mkResponse responseBody
  liftIO (f response)
handleSoapAction GetPhotoStatus body _ f = do
  logDebug gotGetPhotoStatusRequest
  credentialGood <- checkCredential body
  if credentialGood then do
    response <- mkResponse getPhotoStatusResponse
    liftIO (f response)
  else
    liftIO (f mkUnauthorizedResponse)
handleSoapAction MarkLastPhotoInRoll _ _ f = do
  logDebug gotMarkLastPhotoInRollRequest
  response <- mkResponse markLastPhotoInRollResponse
  liftIO (f response)

checkCredential :: BL.ByteString -> HEyefiM Bool
checkCredential body = do
  let tag = firstTag body
  let macaddress = tag "macaddress"
  let credential = tag "credential"
  state <- get
  let snonce = lastSNonce state
  upload_key_0 <- getUploadKeyForMacaddress macaddress
  case upload_key_0 of
   Nothing -> do
     logInfo (noUploadKeyInConfiguration macaddress)
     return False
   Just upload_key_0' -> do
     let credentialString = macaddress <> upload_key_0' <> snonce
     let binaryCredentialString = unhex credentialString
     let expectedCredential = tmd5 (fromJust binaryCredentialString)
     if credential /= expectedCredential then do
       logInfo (invalidCredential expectedCredential credential)
       return False
     else
       return True
