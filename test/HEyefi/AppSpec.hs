{-# LANGUAGE OverloadedStrings #-}

module HEyefi.AppSpec where

import Data.CaseInsensitive as CI
import Network.HTTP.Types.Method
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import HEyefi.App
import HEyefi.Config


app' :: IO Application
app' = do
  config <- liftIO newConfig
  return (app config)

spec :: Spec
spec = do
  with app' (
    do
      describe "MarkLastPhotoInRoll" (
        it "should respond with status 200"
        (request
         methodPost "/"
         [(CI.mk "SoapAction",  "\"urn:MarkLastPhotoInRoll\"")]
         ""
         `shouldRespondWith` 200)))
