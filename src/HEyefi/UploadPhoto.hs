{-# LANGUAGE OverloadedStrings #-}

module HEyefi.UploadPhoto where

import           HEyefi.Constant (multipartBodyBoundary)
import           HEyefi.Log (logInfo)
import           HEyefi.Soap (mkResponse)
import           HEyefi.Types (uploadDirectory, HEyefiM, HEyefiApplication)

import           Codec.Archive.Tar (extract)
import           Control.Arrow ((>>>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import qualified Data.ByteString.Lazy as BL
import           Network.Multipart ( parseMultipartBody, MultiPart (..), BodyPart (..) )
import           System.Directory (copyFile, getDirectoryContents)
import           System.FilePath.Posix ((</>))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import           System.Posix.Files (setOwnerAndGroup, fileOwner, fileGroup, getFileStatus, FileStatus)
import Text.XML.HXT.Core ( runX
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , txt
                         , root
                         , writeDocumentToString)

copyMatchingOwnership :: FileStatus -> FilePath -> FilePath -> IO ()
copyMatchingOwnership fs from to = do
  setOwnerAndGroup from (fileOwner fs) (fileGroup fs)
  copyFile from to

changeOwnershipAndCopy :: FilePath -> FilePath -> IO ()
changeOwnershipAndCopy uploadDir extractionDir = do
  s <- getFileStatus uploadDir
  names <- getDirectoryContents extractionDir
  mapM_ (processName s) (properNames names)
  where
    properNames = filter (`notElem` [".", ".."])
    processName s n =
      copyMatchingOwnership s (extractionDir </> n) (uploadDir </> n)

uploadPhotoResponse :: HEyefiM String
uploadPhotoResponse = do
  let document =
        root [ ]
        [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
        , mkelem "SOAP-ENV:Envelope"
          [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
          [ mkelem "SOAP-ENV:Body" []
            [ mkelem "UploadPhotoResponse"
              [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
              [ mkelem "success" [] [ txt "true" ]
              ]
            ]
          ]
        ]
  result <- liftIO (runX (document >>> writeDocumentToString []))
  return (head result)

-- TODO: handle case where uploaded file has a bad format
-- TODO: handle case where temp file is not created
writeTarFile :: BL.ByteString -> HEyefiM ()
writeTarFile file = do
  config <- ask
  let uploadDir = uploadDirectory config
  liftIO (withSystemTempFile "heyefi.tar" (handleFile uploadDir))
  where
    handleFile uploadDir filePath handle = do
      withSystemTempDirectory "heyefi_extracted" (handleDir uploadDir filePath handle)
    handleDir uploadDir tempFile tempFileHandle extractionDir = do
      BL.hPut tempFileHandle file
      hClose tempFileHandle
      extract extractionDir tempFile
      changeOwnershipAndCopy uploadDir extractionDir

handleUpload :: BL.ByteString -> HEyefiApplication
handleUpload body _ f = do
  let MultiPart bodyParts = parseMultipartBody multipartBodyBoundary body
  logInfo (show (length bodyParts))
  lBP bodyParts
  let (BodyPart _ soapEnvelope) = bodyParts !! 0
  let (BodyPart _ file) = bodyParts !! 1
  let (BodyPart _ digest) = bodyParts !! 2

  writeTarFile file

  logInfo (show soapEnvelope)
  logInfo (show digest)
  responseBody <- uploadPhotoResponse
  logInfo (show responseBody)
  r <- mkResponse responseBody
  liftIO (f r)

  where
    lBP [] = return ()
    lBP ((BodyPart headers _):xs) = do
      logInfo (show headers)
      lBP xs
      return ()
