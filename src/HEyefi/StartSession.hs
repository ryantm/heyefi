{-# LANGUAGE OverloadedStrings #-}

module HEyefi.StartSession where

import HEyefi.Hex (unhex)
import HEyefi.Config (Config, getUploadKeyForMacaddress)
import HEyefi.Log (logInfo)

import Text.XML.HXT.Core ( runX
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , txt
                         , root
                         , writeDocumentToString)
import Control.Arrow ((>>>))
import Data.Hash.MD5 (md5s, Str (..))
import Data.Maybe (fromJust)


--TODO: make nounce not hard coded
startSessionResponse :: Config ->
                        String ->
                        String ->
                        String ->
                        String ->
                        IO String
startSessionResponse config macaddress cnonce transfermode transfermodetimestamp = do
  let upload_key_0 = getUploadKeyForMacaddress config macaddress
  case upload_key_0 of
   Nothing -> do
     logInfo ("No upload key found in configuration for macaddress: " ++ macaddress)
     return ""
   Just upload_key_0' -> do
     let credentialString = macaddress ++ cnonce ++ upload_key_0'
     let binaryCredentialString = unhex credentialString
     let credential = md5s (Str (fromJust binaryCredentialString))
     let document =
           root []
           [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
           , mkelem "SOAP-ENV:Envelope"
             [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
             [ mkelem "SOAP-ENV:Body" []
               [ mkelem "StartSessionResponse"
                 [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
                 [ mkelem "credential" [] [ txt credential ]
                 , mkelem "snonce" [] [ txt "bff7fe782919114202d3b601682ba8aa" ]
                 , mkelem "transfermode" [] [ txt transfermode ]
                 , mkelem "transfermodetimestamp" [] [ txt transfermodetimestamp ]
                 , mkelem "upsyncallowed" [] [ txt "true" ]
                 ]
               ]
             ]
           ]
     result <- runX (document >>> writeDocumentToString [])
     return (head result)
