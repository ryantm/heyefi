{-# LANGUAGE OverloadedStrings #-}
module HEyefi.SoapResponseSpec where

import Test.Hspec

import HEyefi.SoapResponse

spec :: Spec
spec =
  describe "getPhotoStatusResponse"
    (it "should have GetPhotoStatusResponse element"
     (do
         getPhotoStatusResponse `shouldContain` "GetPhotoStatusResponse"))
