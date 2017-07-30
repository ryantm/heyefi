module HEyefi.AppSpec where

import Control.Concurrent.STM
import Data.ByteString.Lazy (toStrict)
import Data.CaseInsensitive as CI
import Data.Text (Text, isInfixOf)
import Data.Text.Encoding (decodeUtf8)
import HEyefi.App
import HEyefi.Config
import HEyefi.SpecPrelude
import Network.HTTP.Types.Method
import Network.Wai (Application)
import Network.Wai.Test (SResponse (simpleBody))
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal



spec :: Spec
spec = do
  describe "StartSession" (
    it "should return expected body" (
       do
         a <- app'
         action <- runWaiSession sampleStartSessionRequest a
         responseBodyContains action sampleStartSessionResponse1 `shouldBe` True
         responseBodyContains action sampleStartSessionResponse2
           `shouldBe` True))
  with app' (
    do
      describe "MarkLastPhotoInRoll" (
        it "should respond with status 200" (
           sampleMarkLastPhotoRequest
           `shouldRespondWith`
           sampleMarkLastPhotoInRollResponse {matchStatus = 200}))
      describe "StartSession" (
        it "should respond with status 200" (
           sampleStartSessionRequest
           `shouldRespondWith` 200))
      describe "GetPhotoStatus" (
        it "should respond with status 200" (
           sampleStartSessionRequest
           `shouldRespondWith` 200)))

responseBodyContains :: SResponse -> Text -> Bool
responseBodyContains r t =
  t
  `isInfixOf`
  (decodeUtf8 . toStrict) (simpleBody r)

app' :: IO Application
app' = do
  sharedConfig <- atomically (newTVar (insertCard "0018562de4ce" "36d61e4e7403a0586702c9159892a062" emptyConfig))
  return (app sharedConfig)

sampleMarkLastPhotoRequest :: WaiSession SResponse
sampleMarkLastPhotoRequest =
  request
  methodPost "/"
  [(CI.mk "SoapAction",  "\"urn:MarkLastPhotoInRoll\"")]
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:MarkLastPhotoInRoll><macaddress>001856417729</macaddress><mergedelta>0</mergedelta></ns1:MarkLastPhotoInRoll></SOAP-ENV:Body></SOAP-ENV:Envelope>"

sampleMarkLastPhotoInRollResponse :: ResponseMatcher
sampleMarkLastPhotoInRollResponse = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Body><MarkLastPhotoInRollResponse xmlns=\"http://localhost/api/soap/eyefilm\"/></SOAP-ENV:Body></SOAP-ENV:Envelope>"

sampleStartSessionRequest :: WaiSession SResponse
sampleStartSessionRequest =
  request
  methodPost "/"
  [(CI.mk "SoapAction",  "\"urn:StartSession\"")]
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:StartSession><macaddress>0018562de4ce</macaddress><cnonce>6eb0444343c1953e47fb28181bb4e47f</cnonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp></ns1:StartSession></SOAP-ENV:Body></SOAP-ENV:Envelope>"

sampleStartSessionResponse1 :: Text
sampleStartSessionResponse1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Body><StartSessionResponse xmlns=\"http://localhost/api/soap/eyefilm\"><credential>f9d03ddcce53582ff10075577e522373</credential><snonce>"

sampleStartSessionResponse2 :: Text
sampleStartSessionResponse2 = "</snonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp><upsyncallowed>true</upsyncallowed></StartSessionResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>"

sampleGetPhotoRequest :: WaiSession SResponse
sampleGetPhotoRequest =
  request
  methodPost "/"
  [(CI.mk "SoapAction",  "\"urn:GetPhotoRequest\"")]
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:GetPhotoStatus><credential>7daa9ecf3a9f135f5bb30541ed84fcfb</credential><macaddress>0018562de4ce</macaddress><filename>IMG_2195.JPG.tar</filename><filesize>125952</filesize><filesignature>736ffb7fa20f1708fd300c58c0aabb61</filesignature><flags>4</flags></ns1:GetPhotoStatus></SOAP-ENV:Body></SOAP-ENV:Envelope>"
