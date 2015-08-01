{-# LANGUAGE OverloadedStrings #-}

module HEyefi.MarkLastPhotoInRoll where

import HEyefi.Types (HEyefiM)

import Text.XML.HXT.Core (
    mkelem
  , sattr
  , LA
  , XmlTree)

markLastPhotoInRollResponse :: [LA n XmlTree]
markLastPhotoInRollResponse =
  [ mkelem "MarkLastPhotoInRollResponse"
      [ sattr "xmlns" "http://localhost/api/soap/eyefilm" ]
      []
  ]
