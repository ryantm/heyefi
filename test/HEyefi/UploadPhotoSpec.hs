{-# LANGUAGE OverloadedStrings #-}

module HEyefi.UploadPhotoSpec where

import qualified Data.ByteString.Lazy as BS
import           System.Directory
import           System.Posix.User
import           Test.Hspec
import           System.Posix.Files (
    setOwnerAndGroup
  , fileOwner
  , fileGroup
  , getFileStatus
  )


import           HEyefi.Config
import           HEyefi.Types
import           HEyefi.UploadPhoto


uploadDir :: FilePath
uploadDir = "test_upload_dir"

uploadConfig :: Config
uploadConfig = emptyConfig { uploadDirectory = uploadDir }

spec :: Spec
spec =
  describe "writeTarFile"
    (it "should write the file to the upload_dir" (
        do
          bs <- BS.readFile "test_upload_file.tar"
          removeDirectoryRecursive uploadDir
          createDirectory uploadDir
          ge <- getGroupEntryForName "wheel"
          ue <- getUserEntryForName "ryantm"
          setOwnerAndGroup uploadDir (userID ue) (groupID ge)
          _ <- runWithConfig uploadConfig (writeTarFile bs)
          contents <- getDirectoryContents uploadDir
          length contents `shouldBe` 3
          let f = "test_upload_dir/test_upload_file.txt"
          fs <- getFileStatus f
          fileGroup fs `shouldBe` groupID ge
          fileOwner fs `shouldBe` userID ue))
