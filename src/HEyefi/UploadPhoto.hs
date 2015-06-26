{-# LANGUAGE OverloadedStrings #-}

module HEyefi.UploadPhoto where

import           HEyefi.Constant (multipartBodyBoundary)
import           HEyefi.Log (logDebug, logInfo)
import           HEyefi.Soap (mkResponse)
import           HEyefi.Types (uploadDirectory, HEyefiM, HEyefiApplication)

import           Codec.Archive.Tar (extract)
import           Control.Arrow ((>>>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Lazy (get)
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

copyMatchingOwnership :: FileStatus -> FilePath -> FilePath -> IO (FilePath)
copyMatchingOwnership fs from to = do
  setOwnerAndGroup from (fileOwner fs) (fileGroup fs)
  copyFile from to
  return to

changeOwnershipAndCopy :: FilePath -> FilePath -> IO (FilePath)
changeOwnershipAndCopy uploadDir extractionDir = do
  s <- getFileStatus uploadDir
  names <- getDirectoryContents extractionDir
  paths <- mapM (processName s) (properNames names)
  return (head paths)
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
writeTarFile :: BL.ByteString -> HEyefiM (FilePath)
writeTarFile file = do
  config <- get
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
  logInfo "Got Upload request"
  let MultiPart bodyParts = parseMultipartBody multipartBodyBoundary body
  logDebug (show (length bodyParts))
  lBP bodyParts
  let (BodyPart _ soapEnvelope) = bodyParts !! 0
  let (BodyPart _ file) = bodyParts !! 1
  let (BodyPart _ digest) = bodyParts !! 2

  outputPath <- writeTarFile file
  logInfo ("Uploaded to " ++ outputPath)

  logDebug (show soapEnvelope)
  logDebug (show digest)
  responseBody <- uploadPhotoResponse
  logDebug (show responseBody)
  r <- mkResponse responseBody
  liftIO (f r)

  where
    lBP [] = return ()
    lBP ((BodyPart headers _):xs) = do
      logDebug (show headers)
      lBP xs
      return ()
