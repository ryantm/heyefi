{-# LANGUAGE OverloadedStrings #-}

module HEyefi.UploadPhoto where

import Text.XML.HXT.Core ( runX
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , txt
                         , root
                         , writeDocumentToString)
import Control.Arrow ((>>>))

uploadPhotoResponse :: IO String
uploadPhotoResponse = do
  let document =
        root [ ]
        [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
        , mkelem "SOAP-ENV:Envelope"
          [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
          [ mkelem "SOAP-ENV:Body" []
            [ mkelem "UploadPhotoResponse"
              [ ]
              [ mkelem "success" [] [ txt "true" ]
              ]
            ]
          ]
        ]
  result <- runX (document >>> writeDocumentToString [])
  return (head result)
