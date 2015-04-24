{-# LANGUAGE OverloadedStrings #-}

module HEyefi.GetPhotoStatus where

import Text.XML.HXT.Core ( runX
                         , mkelem
                         , spi
                         , t_xml
                         , sattr
                         , txt
                         , root
                         , writeDocumentToString)
import Control.Arrow ((>>>))

getPhotoStatusResponse :: IO String
getPhotoStatusResponse = do
  let document =
        root []
        [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
        , mkelem "SOAP-ENV:Envelope"
          [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
          [ mkelem "SOAP-ENV:Body" []
            [ mkelem "GetPhotoStatusResponse"
              [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
              [ mkelem "fileid" [] [ txt "1" ]
              , mkelem "offset" [] [ txt "0" ]
              ]
            ]
          ]
        ]
  result <- runX (document >>> writeDocumentToString [])
  return (head result)
