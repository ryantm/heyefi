{-# LANGUAGE OverloadedStrings #-}

module HEyefi.StartSession where

import HEyefi.Constant (upload_key_0)

import Text.XML.HXT.Core ( runX
                         , mkelem
                         , sattr
                         , txt
                         , root
                         , writeDocumentToString )
import Control.Arrow ((>>>))
import Data.Hash.MD5 (md5s, Str (..))
import Data.Hex (unhex)



startSessionResponse :: String -> String -> String -> String -> IO String
startSessionResponse macaddress cnonce transfermode transfermodetimestamp = do
  bcs <- binaryCredentialString
  let document =
        mkelem "SOAP-ENV:Envelope"
        [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
        [ mkelem "SOAP-ENV:Body" []
          [ mkelem "StartSessionResponse"
            [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
            [ mkelem "credential" [] [ txt (credential bcs) ]
            , mkelem "snonce" [] [ txt "340282366920938463463374607431768211456" ]
            , mkelem "transfermode" [] [ txt transfermode ]
            , mkelem "transfermodetimestamp" [] [ txt transfermodetimestamp ]
            , mkelem "upsyncallowed" [] [ txt "true" ]
            ]
          ]
        ]
  result <- (runX (root [] [document] >>> writeDocumentToString []))
  return (head result)
  where
    credential bcs = md5s (Str (bcs))
    binaryCredentialString = unhex credentialString
    credentialString :: String
    credentialString = macaddress ++ cnonce ++ upload_key_0
