{-# LANGUAGE OverloadedStrings #-}

module HEyefi.SoapSpec where

import Test.Hspec

import HEyefi.Soap
import HEyefi.Types


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
        (firstJust intToMaybe [4] `shouldBe` Just 4)
      it "should get the first matched element"
        (firstJust intToMaybe [1,5,4] `shouldBe` Just 5))
  describe "headerToSoapAction" (
    do
      it "should return StartSession for one"
        (headerToSoapAction (soapActionHeaderName, "\"urn:StartSession\"")
            `shouldBe` Just StartSession)
      it "should return GetPhotoStatus for one"
        (headerToSoapAction (soapActionHeaderName, "\"urn:GetPhotoStatus\"")
            `shouldBe` Just GetPhotoStatus)
      it "should return MarkLastPhotoInRoll for one"
        (headerToSoapAction (soapActionHeaderName,
                             "\"urn:MarkLastPhotoInRoll\"")
            `shouldBe` Just MarkLastPhotoInRoll))
