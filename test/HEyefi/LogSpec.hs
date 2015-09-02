module HEyefi.LogSpec where

import HEyefi.Log
import HEyefi.Config
import HEyefi.Types

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO.Silently
import Test.Hspec


currentTime :: IO String
currentTime = do
  t <- getCurrentTime
  return (formatTime defaultTimeLocale "%FT%T" t)

spec :: Spec
spec = do
  describe "logDebug" (do
    it "should output nothing when debugging is not enabled" (
        do
          result <- capture_ (runWithEmptyConfig (logDebug "hi"))
          result `shouldBe` "")
    it "should output somethign when debugging enabled" (
        do
          let config = emptyConfig { logLevel = Debug }
          result <- capture_ (runWithConfig config (logDebug "hi"))
          result `shouldContain` "Debug"
          result `shouldContain` "hi"))
  describe "logInfo"
    (it "should output something" (
        do
          result <- capture_ (runWithEmptyConfig (logInfo "hi"))
          time <- currentTime
          result `shouldContain` time
          result `shouldContain` "Info"
          result `shouldContain` "hi"))
