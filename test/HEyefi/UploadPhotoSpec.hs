{-# LANGUAGE OverloadedStrings #-}

module HEyefi.UploadPhotoSpec where

import qualified Data.ByteString.Lazy as BS
import           System.Directory

import           Test.Hspec
import           HEyefi.UploadPhoto
import           HEyefi.Config
import           HEyefi.Types

uploadDir = "test_upload_dir"
uploadConfig = emptyConfig { uploadDirectory = uploadDir }

spec :: Spec
spec =
  describe "writeTarFile"
    (it "should write the file to the upload_dir" (
        do
          bs <- BS.readFile "test_upload_file.tar"
          removeDirectory uploadDir
          createDirectory uploadDir
          runWithConfig uploadConfig (writeTarFile bs)
          contents <- getDirectoryContents uploadDir
          length contents `shouldBe` 3))
