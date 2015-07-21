{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HEyefi.CommandLineOptions
import           HEyefi.Config (monitorConfig, newConfig, runWithConfig)
import           HEyefi.Constant (port, configPath)
import           HEyefi.Log (logInfoIO, logDebug)
import           HEyefi.Soap (handleSoapAction, soapAction)
import           HEyefi.Strings
import           HEyefi.Types (SharedConfig, HEyefiApplication)
import           HEyefi.UploadPhoto (handleUpload)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (
    newTVar
  , atomically
  , writeTVar
  , TVar
  , readTVar )
import           Control.Monad (forever)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (isJust, fromJust, isNothing)
import           Network.Wai (
    Application
  , Request
  , pathInfo
  , requestBody
  , requestMethod
  , requestHeaders )
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative
import           System.Posix.Signals (installHandler, sigHUP, Handler( Catch ))

handleHup :: TVar (Maybe Int) -> IO ()
handleHup wakeSig = atomically (writeTVar wakeSig (Just 1))

greet :: Maybe () -> IO ()
greet Nothing = putStrLn "version"
greet (Just ()) = runHeyefi

runHeyefi :: IO ()
runHeyefi = do
  wakeSig <- atomically (newTVar Nothing)
  sharedConfig <- newConfig
  _ <- installHandler sigHUP (Catch $ handleHup wakeSig) Nothing

  _ <- forkIO (forever
               (do
                   c <- atomically (readTVar sharedConfig)
                   runWithConfig c (
                     monitorConfig configPath sharedConfig wakeSig)))

  logInfoIO (listeningOnPort (show port))
  run port (app sharedConfig)

main :: IO ()
main = execParser opts >>= greet

app :: SharedConfig -> Application
app sharedConfig req f = do
  config <- atomically (readTVar sharedConfig)
  body <- getWholeRequestBody req
  (result, config') <- runWithConfig config (do
                  logDebug (show (pathInfo req))
                  logDebug (show (requestHeaders req))
                  dispatchRequest (fromStrict body) req f)
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
