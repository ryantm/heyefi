{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HEyefi.ConfigSpec where

import Test.Hspec

import Control.Exception (catch, SomeException, throwIO)
import System.IO.Silently (capture, capture_)
import System.FilePath ((</>))
import System.Directory (getTemporaryDirectory)

import HEyefi.Config

import qualified Data.HashMap.Strict as HM
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

getNonexistentTemporaryFile :: IO FilePath
getNonexistentTemporaryFile = do
  tempdir <- catch getTemporaryDirectory (\(_::SomeException) -> return ".")
  let file = tempdir </> "heyefi.config"
  removeIfExists file
  return file

makeAndReloadFile_ :: String -> IO String
makeAndReloadFile_ s = fmap fst (makeAndReloadFile s)

makeAndReloadFile :: String -> IO (String, Config)
makeAndReloadFile contents = do
  file <- getNonexistentTemporaryFile
  writeFile file contents
  capture (reloadConfig file)

validConfig :: String
validConfig = "upload_dir = \"/data/photos\"\ncards = [[\"0012342de4ce\",\"e7403a0123402ca062\"],[\"1234562d5678\",\"12342a062\"]]"

spec :: Spec
spec = do
  describe "reloadConfig" (do
    (it "should report an error for a non-existent configuration file"
     (do
         file <- getNonexistentTemporaryFile
         output <- capture_ (reloadConfig file)
         output `shouldContain` "Could not find configuration file at " ++ file))
    (it "should report an error for an unparsable configuration file"
     (do
         output <- makeAndReloadFile_ "a = (\n"
         output `shouldContain` "Error parsing configuration file at "
         output `shouldContain` "with message: endOfInput"))
    (it "should complain about missing cards configuration"
     (do
         output <- makeAndReloadFile_ "upload_dir = \"/data/annex/doxie/unsorted\""
         output `shouldContain` "missing a definition for `cards`."))
    (it "should parse cards for a validConfig"
     (do
         (_, config) <- makeAndReloadFile validConfig
         HM.lookup "0012342de4ce" (cardMap config) `shouldBe` (Just "e7403a0123402ca062"))))
