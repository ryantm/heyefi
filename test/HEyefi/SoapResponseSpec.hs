module HEyefi.SoapResponseSpec where

import HEyefi.SoapResponse
import HEyefi.SpecPrelude

spec :: Spec
spec =
  describe "getPhotoStatusResponse"
    (it "should have GetPhotoStatusResponse element"
     (getPhotoStatusResponse `tshouldContain` "GetPhotoStatusResponse"))
