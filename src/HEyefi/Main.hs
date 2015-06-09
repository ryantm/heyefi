{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HEyefi.Constant
import           HEyefi.Log (logInfo)
import           HEyefi.Config (SharedConfig, monitorConfig, newConfig)
import           HEyefi.UploadPhoto (handleUpload)
import           HEyefi.Soap (handleSoapAction, soapAction)


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import           Data.ByteString.Lazy (fromStrict)
import           Data.Maybe (isJust, fromJust, isNothing)
import           Network.Wai ( Application
                   , Request
                   , pathInfo
                   , requestBody
                   , requestMethod
                   , requestHeaders )
import           Network.Wai.Handler.Warp (run)

import           Control.Monad (forever)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (newTVar, atomically, writeTVar, TVar)
import           System.Posix.Signals (installHandler, sigHUP, Handler( Catch ))

handleHup :: TVar (Maybe Int) -> IO ()
handleHup wakeSig = atomically (writeTVar wakeSig (Just 1))

main :: IO ()
main = do
  wakeSig <- atomically (newTVar Nothing)
  sharedConfig <- newConfig
  _ <- installHandler sigHUP (Catch $ handleHup wakeSig) Nothing
  _ <- forkIO (forever (monitorConfig configPath sharedConfig wakeSig))

  logInfo ("Listening on port " ++ show port)
  run port (app sharedConfig)

app :: SharedConfig -> Application
app config req f = do
  body <- getWholeRequestBody req
  logInfo (show (pathInfo req))
  logInfo (show (requestHeaders req))
  -- logInfo (show (toString body))
  dispatchRequest config (fromStrict body) req f

dispatchRequest :: SharedConfig -> BL.ByteString -> Application
dispatchRequest config body req f
  | requestMethod req == "POST" &&
    pathInfo req == ["api","soap","eyefilm","v1","upload"] &&
    isNothing (soapAction req) =
      handleUpload config body req f
dispatchRequest config body req f
  | requestMethod req == "POST" &&
    isJust (soapAction req) =
      handleSoapAction (fromJust (soapAction req)) config body req f
dispatchRequest _ _ _ _ = error "did not match dispatch"

getWholeRequestBody :: Request -> IO B.ByteString
getWholeRequestBody request = do
  r <- requestBody request
  if r == B.empty
    then return B.empty
    else do
     rest <- getWholeRequestBody request
     return (B.append r rest)
