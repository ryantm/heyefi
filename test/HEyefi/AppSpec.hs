{-# LANGUAGE OverloadedStrings #-}

module HEyefi.AppSpec where

import Control.Concurrent.STM
import Data.CaseInsensitive as CI
import Network.HTTP.Types.Method
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai

import HEyefi.App
import HEyefi.Config


spec :: Spec
spec = do
  with app' (
    do
      describe "MarkLastPhotoInRoll" (
        it "should respond with status 200" (
           do
             sampleMarkLastPhotoRequest
               `shouldRespondWith`
               sampleMarkLastPhotoInRollResponse {matchStatus = 200}))
      describe "StartSession" (
        it "should respond with status 200" (
           do
             sampleStartSessionRequest
               `shouldRespondWith` 200)))

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

sampleStartSessionResponse :: ResponseMatcher
sampleStartSessionResponse = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Body><StartSessionResponse xmlns=\"http://localhost/api/soap/eyefilm\"><credential>f9d03ddcce53582ff10075577e522373</credential><snonce>a0f6fc3983454d6da100c8ab5f3efa12</snonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp><upsyncallowed>true</upsyncallowed></StartSessionResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>"
