module HEyefi.App where

import           HEyefi.Config (runWithConfig)
import           HEyefi.Log (logDebug)
import           HEyefi.Soap (handleSoapAction, soapAction)
import           HEyefi.Strings
import           HEyefi.Types (SharedConfig, HEyefiApplication)
import           HEyefi.UploadPhoto (handleUpload)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (isJust, fromJust, isNothing)
import           Network.Wai (
    Application
  , Request
  , pathInfo
  , requestBody
  , requestMethod
  , requestHeaders )
import           Control.Concurrent.STM (
    atomically
  , writeTVar
  , readTVar )

app :: SharedConfig -> Application
app sharedConfig req f = do
  config <- atomically (readTVar sharedConfig)
  body <- getWholeRequestBody req
  (result, config') <- runWithConfig config (do
                  logDebug (show (pathInfo req))
                  logDebug (show (requestHeaders req))
                  dispatchRequest (BL.fromStrict body) req f)
  atomically (writeTVar sharedConfig config')
  return result

dispatchRequest :: BL.ByteString -> HEyefiApplication
dispatchRequest body req f
  | requestMethod req == "POST" &&
    pathInfo req == ["api","soap","eyefilm","v1","upload"] &&
    isNothing (soapAction req) =
      handleUpload body req f
dispatchRequest body req f
  | requestMethod req == "POST" &&
    isJust (soapAction req) =
      handleSoapAction (fromJust (soapAction req)) body req f
dispatchRequest _ _ _ = error didNotMatchDispatch

getWholeRequestBody :: Request -> IO B.ByteString
getWholeRequestBody request = do
  r <- requestBody request
  if r == B.empty
    then return B.empty
    else do
     rest <- getWholeRequestBody request
     return (B.append r rest)
