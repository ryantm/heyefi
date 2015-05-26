{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HEyefi.ConfigSpec where

import Test.Hspec

import Control.Exception (catch, SomeException, throwIO)
import System.IO.Silently (capture)
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
         (output, config) <- capture (reloadConfig file)
         (cardMap config) `shouldBe` HM.empty
         output `shouldContain` "Could not find configuration file at " ++ file))
    (it "should report an error for an unparsable configuration file"
     (do
         (output, config) <- makeAndReloadFile "a = (\n"
         (cardMap config) `shouldBe` HM.empty
         output `shouldContain` "Error parsing configuration file at "
         output `shouldContain` "with message: endOfInput"))
    (it "should complain about missing cards configuration"
     (do
         (output, config) <- makeAndReloadFile "upload_dir = \"/data/annex/doxie/unsorted\""
         (cardMap config) `shouldBe` HM.empty
         output `shouldContain` "missing a definition for `cards`."))
    (it "should complain about cards not having the correct format"
     (do
         (_, config) <- makeAndReloadFile "upload_dir = \"/data/annex/doxie/unsorted\"\ncards=[[\"1\",\"2\",\"3\"]]"
         (cardMap config) `shouldBe` HM.empty
         output2 <- makeAndReloadFile_ "upload_dir = \"/data/annex/doxie/unsorted\"\ncards=\"1\""
         output2 `shouldContain` "Format of cards does not match"))
    (it "should complain about missing upload_dir configuration"
     (do
         (output, config) <-  makeAndReloadFile "cards = [[\"0012342de4ce\",\"e7403a0123402ca062\"],[\"1234562d5678\",\"12342a062\"]]"
         (uploadDirectory config) `shouldBe` ""
         output `shouldContain`"missing a definition for `upload_dir`."))
    (it "should parse cards for a validConfig"
     (do
         (output, config) <- makeAndReloadFile validConfig
         putStrLn output
         (uploadDirectory config) `shouldBe` "/data/photos"
         HM.lookup "0012342de4ce" (cardMap config) `shouldBe` (Just "e7403a0123402ca062")
         HM.lookup "1234562d5678" (cardMap config) `shouldBe` (Just "12342a062"))))
