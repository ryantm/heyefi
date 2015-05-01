{-# LANGUAGE OverloadedStrings #-}

module HEyefi.StartSession where

import HEyefi.Constant (upload_key_0)
import HEyefi.Hex (unhex)

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

startSessionResponse :: String -> String -> String -> String -> IO String
startSessionResponse macaddress cnonce transfermode transfermodetimestamp = do
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
  where
    credential = md5s (Str (fromJust binaryCredentialString))
    binaryCredentialString = unhex credentialString
    credentialString :: String
    credentialString = macaddress ++ cnonce ++ upload_key_0
