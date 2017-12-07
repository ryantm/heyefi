module HEyefi.UploadPhotoSpec where

import           Control.Monad
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
import           HEyefi.SpecPrelude


uploadDir :: FilePath
uploadDir = "test_upload_dir"

uploadConfig :: Config
uploadConfig = emptyConfig { uploadDirectory = uploadDir }

removeUploadDir :: IO ()
removeUploadDir = do
  pathExists <- doesPathExist uploadDir
  when pathExists
    (removeDirectoryRecursive uploadDir)

spec :: Spec
spec =
  describe "writeTarFile"
    (it "should write the file to the upload_dir" (
        do
          bs <- BS.readFile "test_upload_file.tar"
          removeUploadDir
          createDirectory uploadDir
          gid <- getRealGroupID
          uid <- getRealUserID
          setOwnerAndGroup uploadDir uid gid
          _ <- runWithConfig uploadConfig (writeTarFile bs)
          contents <- getDirectoryContents uploadDir
          length contents `shouldBe` 3
          let f = "test_upload_dir/test_upload_file.txt"
          fs <- getFileStatus f
          fileGroup fs `shouldBe` gid
          fileOwner fs `shouldBe` uid
          removeUploadDir))
