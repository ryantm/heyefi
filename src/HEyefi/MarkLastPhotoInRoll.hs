{-# LANGUAGE OverloadedStrings #-}

module HEyefi.MarkLastPhotoInRoll where

import HEyefi.Types (HEyefiM)

import Control.Arrow ((>>>))
import Control.Monad.IO.Class (liftIO)
import Text.XML.HXT.Core ( runX
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , root
                         , writeDocumentToString)

markLastPhotoInRollResponse :: HEyefiM String
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
  result <- liftIO (runX (document >>> writeDocumentToString []))
  return (head result)
