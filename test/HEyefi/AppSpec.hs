{-# LANGUAGE OverloadedStrings #-}

module HEyefi.AppSpec where

import Data.CaseInsensitive as CI
import Network.HTTP.Types.Method
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai

import HEyefi.App
import HEyefi.Config


app' :: IO Application
app' = do
  config <- liftIO newConfig
  return (app config)

sampleMarkLastPhotoRequest :: WaiSession SResponse
sampleMarkLastPhotoRequest =
  request
  methodPost "/"
  [(CI.mk "SoapAction",  "\"urn:MarkLastPhotoInRoll\"")]
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:MarkLastPhotoInRoll><macaddress>001856417729</macaddress><mergedelta>0</mergedelta></ns1:MarkLastPhotoInRoll></SOAP-ENV:Body></SOAP-ENV:Envelope>"

sampleMarkLastPhotoInRoll :: ResponseMatcher
sampleMarkLastPhotoInRoll = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Body><MarkLastPhotoInRollResponse xmlns=\"http://localhost/api/soap/eyefilm\"/></SOAP-ENV:Body></SOAP-ENV:Envelope>"

spec :: Spec
spec = do
  with app' (
    do
      describe "MarkLastPhotoInRoll" (
        it "should respond with status 200" (
           do
             sampleMarkLastPhotoRequest
               `shouldRespondWith`
               sampleMarkLastPhotoInRoll {matchStatus = 200})))
