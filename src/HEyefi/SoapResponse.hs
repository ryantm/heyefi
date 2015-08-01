{-# LANGUAGE OverloadedStrings #-}

module HEyefi.SoapResponse where

import Text.XML.HXT.Core (
    mkelem
  , sattr
  , LA
  , XmlTree
  , txt )

markLastPhotoInRollResponse :: [LA n XmlTree]
markLastPhotoInRollResponse =
  [ mkelem "MarkLastPhotoInRollResponse"
      [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
      []
  ]

getPhotoStatusResponse :: [LA n XmlTree]
getPhotoStatusResponse =
  [ mkelem "GetPhotoStatusResponse"
    [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
    [ mkelem "fileid" [] [ txt "1" ]
    , mkelem "offset" [] [ txt "0" ]
    ]
  ]
