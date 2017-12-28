module Main where

import HEyefi.App (app)
import HEyefi.CommandLineOptions (handleOptionsThenMaybe)
import HEyefi.Config (monitorConfig, newConfig, runWithConfig)
import HEyefi.Constant (port, configPath)
import HEyefi.Log (logInfoIO)
import HEyefi.Prelude
import HEyefi.Types (SharedConfig)

import Control.Concurrent (forkIO)
import           Control.Concurrent.STM (
    newTVar
  , atomically
  , writeTVar
  , TVar
  , readTVar )
import Control.Monad (forever, void)
import Network.Wai.Handler.Warp (run)
import System.IO
import System.Posix.Signals (installHandler, sigHUP, Handler( Catch ))


handleHup :: TVar (Maybe Int) -> IO ()
handleHup wakeSig = atomically (writeTVar wakeSig (Just 1))

hangupSignal :: IO (TVar (Maybe Int))
hangupSignal = do
  wakeSig <- atomically (newTVar Nothing)
  _ <- installHandler sigHUP (Catch (handleHup wakeSig)) Nothing
  return wakeSig

readAndMonitorSharedConfig :: TVar (Maybe Int) ->
                              SharedConfig ->
                              IO ()
readAndMonitorSharedConfig wakeSig sharedConfig =
  void (forkIO (forever
          (do
              c <- atomically (readTVar sharedConfig)
              runWithConfig c (
                monitorConfig (unpack configPath) sharedConfig wakeSig))))

runHeyefi :: IO ()
runHeyefi = do
  wakeSig <- hangupSignal
  sharedConfig <- newConfig

  readAndMonitorSharedConfig wakeSig sharedConfig

  logInfoIO (listeningOnPort (tshow port))
  run port (app sharedConfig)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  handleOptionsThenMaybe runHeyefi
