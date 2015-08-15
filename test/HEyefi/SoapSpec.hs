{-# LANGUAGE OverloadedStrings #-}

module HEyefi.SoapSpec where

import Test.Hspec

import HEyefi.Soap
import HEyefi.Types


sampleStartSessionRequest :: String
sampleStartSessionRequest = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:StartSession><macaddress>0018562de4ce</macaddress><cnonce>6eb0444343c1953e47fb28181bb4e47f</cnonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp></ns1:StartSession></SOAP-ENV:Body></SOAP-ENV:Envelope>"

sampleGetPhotoRequest :: String
sampleGetPhotoRequest = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:GetPhotoStatus><credential>7daa9ecf3a9f135f5bb30541ed84fcfb</credential><macaddress>0018562de4ce</macaddress><filename>IMG_2195.JPG.tar</filename><filesize>125952</filesize><filesignature>736ffb7fa20f1708fd300c58c0aabb61</filesignature><flags>4</flags></ns1:GetPhotoStatus></SOAP-ENV:Body></SOAP-ENV:Envelope>"

intToMaybe :: Int -> Maybe Int
intToMaybe 5 = Just 5
intToMaybe 4 = Just 4
intToMaybe _ = Nothing

spec :: Spec
spec = do
  describe "firstJust" (
    do
      it "should handle an empty list"
        (firstJust intToMaybe [] `shouldBe` Nothing)
      it "should handle one not matched element"
        (firstJust intToMaybe [6] `shouldBe` Nothing)
      it "should handle one matched element"
        (firstJust intToMaybe [5] `shouldBe` Just 5)
      it "should get the first matched element"
        (firstJust intToMaybe [1,5,4] `shouldBe` Just 5))
  describe "headerToSoapAction" (
    do
      it "should return StartSession for one"
        (headerToSoapAction (soapActionHeaderName, "\"urn:StartSession\"")
            `shouldBe` (Just StartSession))
      it "should return GetPhotoStatus for one"
        (headerToSoapAction (soapActionHeaderName, "\"urn:GetPhotoStatus\"")
            `shouldBe` (Just GetPhotoStatus))
      it "should return MarkLastPhotoInRoll for one"
        (headerToSoapAction (soapActionHeaderName,
                             "\"urn:MarkLastPhotoInRoll\"")
            `shouldBe` (Just MarkLastPhotoInRoll)))
