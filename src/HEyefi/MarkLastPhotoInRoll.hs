{-# LANGUAGE OverloadedStrings #-}

module HEyefi.MarkLastPhotoInRoll where

import Text.XML.HXT.Core ( runX
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , root
                         , writeDocumentToString)
import Control.Arrow ((>>>))

markLastPhotoInRollResponse :: IO String
markLastPhotoInRollResponse = do
  let document =
        root []
        [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
        , mkelem "SOAP-ENV:Envelope"
          [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
          [ mkelem "SOAP-ENV:Body" []
            [ mkelem "MarkLastPhotoInRollResponse"
              [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
              []
            ]
          ]
        ]
  result <- runX (document >>> writeDocumentToString [])
  return (head result)
