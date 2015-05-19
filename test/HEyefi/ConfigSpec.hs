{-# LANGUAGE ScopedTypeVariables #-}

module HEyefi.ConfigSpec where

import Test.Hspec

import Control.Exception (catch, SomeException, throwIO)
import System.IO.Silently (capture_)
import System.FilePath ((</>))
import System.Directory (getTemporaryDirectory)

import HEyefi.Config

import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e


spec :: Spec
spec = do
  describe "reloadConfig" (do
    (it "should report an error for a non-existent configuration file"
     (do
         tempdir <- catch getTemporaryDirectory (\(_::SomeException) -> return ".")
         let file = tempdir </> "heyefi.config"
         removeIfExists file
         output <- capture_ (reloadConfig file)
         output `shouldContain` "Could not find configuration file at " ++ file))
    (it "should report an error for an unparsable configuration file"
     (do
         tempdir <- catch getTemporaryDirectory (\(_::SomeException) -> return ".")
         let file = tempdir </> "heyefi.config"
         removeIfExists file
         writeFile file "a = (\n"
         output <- capture_ (reloadConfig file)
         output `shouldContain` "Error parsing configuration file at /tmp/heyefi.config with message: endOfInput")))
