module HEyefi.LogSpec where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import HEyefi.Config
import HEyefi.Log
import HEyefi.Prelude
import HEyefi.SpecPrelude
import HEyefi.Types

currentTime :: IO Text
currentTime = do
  t <- getCurrentTime
  return (pack (formatTime defaultTimeLocale "%FT%T" t))

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
          result `tshouldContain` "Debug"
          result `tshouldContain` "hi"))
  describe "logInfo"
    (it "should output something" (
        do
          result <- capture_ (runWithEmptyConfig (logInfo "hi"))
          time <- currentTime
          result `tshouldContain` time
          result `tshouldContain` "Info"
          result `tshouldContain` "hi"))
