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

contents body =
  [ spi t_xml "version=\"1.0\" encoding=\"UTF-8\""
  , mkelem "SOAP-ENV:Envelope"
    [ sattr "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" ]
    [ mkelem "SOAP-ENV:Body" [] body ]]

-- soapResponse :: Control.Monad.IO.Class.MonadIO m =>
--                 [Control.Arrow.IOStateListArrow.IOSLA
--                  (Text.XML.HXT.Arrow.XmlState.TypeDefs.XIOState ())
--                  Text.XML.HXT.DOM.TypeDefs.XmlTree
--                  Text.XML.HXT.DOM.TypeDefs.XmlTree]
--                 -> m String
soapResponse body = do
  let document = root [] (contents body)
  fmap head (liftIO (runX (document >>> writeDocumentToString [])))

markLastPhotoInRollResponse :: HEyefiM String
markLastPhotoInRollResponse =
  soapResponse
    [ mkelem "MarkLastPhotoInRollResponse"
      [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
      []
    ]
