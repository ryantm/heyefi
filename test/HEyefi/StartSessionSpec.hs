module HEyefi.StartSessionSpec where

import Test.Hspec

import HEyefi.StartSession
import HEyefi.Config (newConfig)

spec :: Spec
spec = do
  describe "startSessionResponse"
    (it "should respond the same as eyefiserver2"
     (do c <- newConfig
         d <- startSessionResponse
              c
              "0018562de4ce"
              "3623cd00fe5aef4c7ccdc66ddbe2f151"
              "34"
              "1356903384"
         d `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Body><StartSessionResponse xmlns=\"http://localhost/api/soap/eyefilm\"><credential>7e8f22c00390b6f1ef7bafe7613cbdd2</credential><snonce>bff7fe782919114202d3b601682ba8aa</snonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp><upsyncallowed>true</upsyncallowed></StartSessionResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>"))


-- [03/08/15 09:24PM][startSession] - Delegating the XML parsing of startSession postData to EyeFiContentHandler()
-- [03/08/15 09:24PM][startSession] - Extracted elements: {'transfermode': u'34', 'macaddress': u'0018562de4ce', 'cnonce': u'3623cd00fe5aef4c7ccdc66ddbe2f151', 'transfermodetimestamp': u'1356903384'}
-- [03/08/15 09:24PM][startSession] - Got MAC address of 0018562de4ce
-- [03/08/15 09:24PM][startSession] - Setting Eye-Fi upload key to 36d61e4e7403a0586702c9159892a062
-- [03/08/15 09:24PM][startSession] - Concatenated credential string (pre MD5): 0018562de4ce3623cd00fe5aef4c7ccdc66ddbe2f15136d61e4e7403a0586702c9159892a062
-- [03/08/15 09:24PM][do_POST] - StartSession response: <?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"><SOAP-ENV:Body><StartSessionResponse xmlns="http://localhost/api/soap/eyefilm"><credential>7e8f22c00390b6f1ef7bafe7613cbdd2</credential><snonce>bff7fe782919114202d3b601682ba8aa</snonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp><upsyncallowed>true</upsyncallowed></StartSessionResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>
