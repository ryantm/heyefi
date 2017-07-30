module HEyefi.SoapResponse where

import           HEyefi.Prelude

import           Control.Arrow ((>>>))
import           Data.Text (Text)
import qualified Data.Text as T
import Text.XML.HXT.Core (
    runLA
  , root
  , writeDocumentToString
  , XmlTree
  , LA
  , mkelem
  , spi
  , sattr
  , t_xml
  , ArrowXml
  , mkelem
  , sattr
  , LA
  , XmlTree
  , txt )

soapMessage :: ArrowXml a => [a n XmlTree] -> [a n XmlTree]
soapMessage body =
  [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
  , mkelem "SOAP-ENV:Envelope"
    [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
    [ mkelem "SOAP-ENV:Body" [] body ]]

soapResponse :: [LA n XmlTree] -> Text
soapResponse body =
  T.pack (head (runLA (document >>> writeDocumentToString []) undefined))
  where
    document = root [] (soapMessage body)

uploadPhotoResponse :: [LA n XmlTree]
uploadPhotoResponse =
  [ mkelem "UploadPhotoResponse"
    [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
    [ mkelem "success" [] [ txt "true" ] ]
  ]

markLastPhotoInRollResponse :: Text
markLastPhotoInRollResponse = soapResponse markLastPhotoInRollBody

markLastPhotoInRollBody :: [LA n XmlTree]
markLastPhotoInRollBody =
  [ mkelem "MarkLastPhotoInRollResponse"
      [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
      []
  ]

getPhotoStatusResponse :: Text
getPhotoStatusResponse = soapResponse getPhotoStatusBody

getPhotoStatusBody :: [LA n XmlTree]
getPhotoStatusBody =
  [ mkelem "GetPhotoStatusResponse"
    [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
    [ mkelem "fileid" [] [ txt "1" ]
    , mkelem "offset" [] [ txt "0" ]
    ]
  ]
