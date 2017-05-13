module HEyefi.SoapResponseSpec where

import Test.Hspec

import HEyefi.SoapResponse

spec :: Spec
spec =
  describe "getPhotoStatusResponse"
    (it "should have GetPhotoStatusResponse element"
     (getPhotoStatusResponse `shouldContain` "GetPhotoStatusResponse"))
