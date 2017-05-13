module HEyefi.StartSession where

import HEyefi.Config (getUploadKeyForMacaddress, putSNonce)
import HEyefi.Hex (unhex)
import HEyefi.Log (logInfo)
import HEyefi.Types (HEyefiM(..))

import Control.Arrow ((>>>))
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (intToDigit)
import Data.Hash.MD5 (md5s, Str (..))
import Data.Maybe (fromJust)
import System.Random (randomRIO)
import Text.XML.HXT.Core ( runLA
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , txt
                         , root
                         , writeDocumentToString)

newServerNonce :: IO String
newServerNonce =
  replicateM 32 (do
                    i <- randomRIO (0, 15)
                    return (intToDigit i))

startSessionResponse :: String ->
                        String ->
                        String ->
                        String ->
                        HEyefiM String
startSessionResponse macaddress cnonce transfermode transfermodetimestamp = do
  upload_key_0 <- getUploadKeyForMacaddress macaddress
  case upload_key_0 of
   Nothing -> do
     logInfo ("No upload key found in configuration for macaddress: " ++ macaddress)
     return ""
   Just upload_key_0' -> do
     snonce <- liftIO newServerNonce
     putSNonce snonce
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
                 , mkelem "snonce" [] [ txt snonce ]
                 , mkelem "transfermode" [] [ txt transfermode ]
                 , mkelem "transfermodetimestamp" [] [ txt transfermodetimestamp ]
                 , mkelem "upsyncallowed" [] [ txt "true" ]
                 ]
               ]
             ]
           ]

     return (head (runLA (document >>> writeDocumentToString []) undefined))
