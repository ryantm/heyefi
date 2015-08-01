{-# LANGUAGE OverloadedStrings #-}

module HEyefi.MarkLastPhotoInRoll where

import HEyefi.Types (HEyefiM)

-- import Text.XML.HXT.Core ( runLA
--                          , mkelem
--                          , spi
--                          , t_xml
--                          , sattr
--                          , root
--                          , writeDocumentToString)
import Text.XML.HXT.Core

contents :: ArrowXml a => [a n XmlTree] -> [a n XmlTree]
contents body =
  [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
  , mkelem "SOAP-ENV:Envelope"
    [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
    [ mkelem "SOAP-ENV:Body" [] body ]]

soapResponse :: [LA n XmlTree] -> String
soapResponse body =
  head (runLA (document >>> writeDocumentToString []) undefined)
  where
    document = root [] (contents body)


markLastPhotoInRollResponse :: HEyefiM String
markLastPhotoInRollResponse =
  return (soapResponse
    [ mkelem "MarkLastPhotoInRollResponse"
      [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
      []
    ])
