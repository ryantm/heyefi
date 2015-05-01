module HEyefi.Config where

import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, retry)

waitForWake :: TVar (Maybe Int) -> IO ()
waitForWake wakeSig = atomically (
  do state <- readTVar wakeSig
     case state of
      Just _ -> writeTVar wakeSig Nothing
      Nothing -> retry)

monitorConfig :: FilePath -> TVar (Maybe Int) -> IO ()
monitorConfig path wakeSig = do
  putStrLn path
  waitForWake wakeSig
