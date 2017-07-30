module HEyefi.StartSession where

import           HEyefi.Config (getUploadKeyForMacaddress, putSNonce)
import           HEyefi.Hex (unhex)
import           HEyefi.Log (logInfo)
import           HEyefi.Prelude
import           HEyefi.Types (HEyefiM(..))

import           Control.Arrow ((>>>))
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (intToDigit)
import           Data.Maybe (fromJust)

import           System.Random (randomRIO)
import Text.XML.HXT.Core ( runLA
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , txt
                         , root
                         , writeDocumentToString)

newServerNonce :: IO Text
newServerNonce = do
  string <- replicateM 32 (do
                            i <- randomRIO (0, 15)
                            return (intToDigit i))
  return (pack string)

startSessionResponse :: Text ->
                        Text ->
                        Text ->
                        Text ->
                        HEyefiM Text
startSessionResponse macaddress cnonce transfermode transfermodetimestamp = do
  upload_key_0 <- getUploadKeyForMacaddress macaddress
  case upload_key_0 of
   Nothing -> do
     logInfo (noUploadKeyInConfiguration macaddress)
     return ""
   Just upload_key_0' -> do
     snonce <- liftIO newServerNonce
     putSNonce snonce
     let credentialText = macaddress <> cnonce <> upload_key_0'
     let binaryCredentialString = unhex credentialText
     let credential = tmd5 (fromJust binaryCredentialString)
     let document =
           root []
           [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
           , mkelem "SOAP-ENV:Envelope"
             [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
             [ mkelem "SOAP-ENV:Body" []
               [ mkelem "StartSessionResponse"
                 [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
                 [ mkelem "credential" [] [ txtT credential ]
                 , mkelem "snonce" [] [ txtT snonce ]
                 , mkelem "transfermode" [] [ txtT transfermode ]
                 , mkelem "transfermodetimestamp" [] [ txtT transfermodetimestamp ]
                 , mkelem "upsyncallowed" [] [ txt "true" ]
                 ]
               ]
             ]
           ]

     return (pack (head (runLA (document >>> writeDocumentToString []) undefined)))
     where
       txtT = txt . unpack
