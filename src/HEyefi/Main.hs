{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HEyefi.Config (monitorConfig, newConfig)
import           HEyefi.Constant
import           HEyefi.Log (logInfoIO, logInfo)
import           HEyefi.Soap (handleSoapAction, soapAction)
import           HEyefi.Types (runHeyefi, SharedConfig, HEyefiApplication)
import           HEyefi.UploadPhoto (handleUpload)


import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (newTVar, atomically, writeTVar, TVar, readTVar)
import           Control.Monad (forever)
import           Control.Monad.Reader (runReaderT)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (isJust, fromJust, isNothing)
import           Network.Wai ( Application
                   , Request
                   , pathInfo
                   , requestBody
                   , requestMethod
                   , requestHeaders )
import           Network.Wai.Handler.Warp (run)
import           System.Posix.Signals (installHandler, sigHUP, Handler( Catch ))

handleHup :: TVar (Maybe Int) -> IO ()
handleHup wakeSig = atomically (writeTVar wakeSig (Just 1))

main :: IO ()
main = do
  wakeSig <- atomically (newTVar Nothing)
  sharedConfig <- newConfig
  _ <- installHandler sigHUP (Catch $ handleHup wakeSig) Nothing

  _ <- forkIO (forever
               (do
                   c <- atomically (readTVar sharedConfig)
                   (runReaderT
                    (runHeyefi (do
                        (monitorConfig configPath sharedConfig wakeSig)))
                    c)))

  logInfoIO ("Listening on port " ++ show port)
  run port (app sharedConfig)

app :: SharedConfig -> Application
app config req f = do
  config' <- atomically (readTVar config)
  body <- getWholeRequestBody req
  (runReaderT (runHeyefi (do
                  logInfo (show (pathInfo req))
                  logInfo (show (requestHeaders req))
                  dispatchRequest (fromStrict body) req f))
   config')

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
dispatchRequest _ _ _ = error "did not match dispatch"

getWholeRequestBody :: Request -> IO B.ByteString
getWholeRequestBody request = do
  r <- requestBody request
  if r == B.empty
    then return B.empty
    else do
     rest <- getWholeRequestBody request
     return (B.append r rest)
