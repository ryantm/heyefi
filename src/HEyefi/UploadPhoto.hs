{-# LANGUAGE OverloadedStrings #-}

module HEyefi.UploadPhoto where

import           HEyefi.Constant (multipartBodyBoundary)
import           HEyefi.Log (logInfo)
import           HEyefi.Soap (mkResponse)
import           HEyefi.Config (SharedConfig, uploadDirectory)

import           Codec.Archive.Tar (extract)
import           Control.Arrow ((>>>))
import           Control.Concurrent.STM (atomically, readTVar)
import qualified Data.ByteString.Lazy as BL
import           Network.Multipart ( parseMultipartBody, MultiPart (..), BodyPart (..) )
import           Network.Wai ( Application )
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import Text.XML.HXT.Core ( runX
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , txt
                         , root
                         , writeDocumentToString)
import System.Posix.Files (setOwnerAndGroup, fileOwner, fileGroup, getFileStatus)

matchDirectoryOwnership :: FilePath -> FilePath -> IO ()
matchDirectoryOwnership directoryToMatch directoryToChange = do
  s <- getFileStatus directoryToMatch

  setOwnerAndGroup file (fileOwner s) (fileGroup s)

uploadPhotoResponse :: IO String
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
  result <- runX (document >>> writeDocumentToString [])
  return (head result)

-- TODO: handle case where uploaded file has a bad format
-- TODO: handle case where temp file is not created
writeTarFile :: SharedConfig -> BL.ByteString -> IO ()
writeTarFile c file = do
  config <- atomically (readTVar c)
  let uploadDir = uploadDirectory config
  withSystemTempFile "heyefi.tar" (handleFile uploadDir)
  where
    handleFile uploadDir filePath handle = do
      withSystemTempDirectory "heyefi_extracted" (handleDir tempFile uploadDir)
    handleDir uploadDir tempFile extractionDir = do
      BL.hPut handle file
      hClose handle
      extract extractionDir tempFile
      matchDirectoryOwnership uploadDir extractionDir

handleUpload :: SharedConfig -> BL.ByteString -> Application
handleUpload config body _ f = do
  let MultiPart bodyParts = parseMultipartBody multipartBodyBoundary body
  logInfo (show (length bodyParts))
  lBP bodyParts
  let (BodyPart _ soapEnvelope) = bodyParts !! 0
  let (BodyPart _ file) = bodyParts !! 1
  let (BodyPart _ digest) = bodyParts !! 2

  writeTarFile config file

  logInfo (show soapEnvelope)
  logInfo (show digest)
  responseBody <- uploadPhotoResponse
  logInfo (show responseBody)
  r <- mkResponse responseBody
  f r

  where
    lBP [] = return ()
    lBP ((BodyPart headers _):xs) = do
      logInfo (show headers)
      lBP xs
      return ()
