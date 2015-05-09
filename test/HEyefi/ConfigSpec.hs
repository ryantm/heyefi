{-# LANGUAGE ScopedTypeVariables #-}

module HEyefi.ConfigSpec where

import Test.Hspec

import Control.Exception (catch, SomeException)
import System.IO.Silently (capture_)
import System.FilePath ((</>))
import System.Directory (getTemporaryDirectory)
import Data.Text (isInfixOf, pack)

import HEyefi.Config


spec :: Spec
spec = do
  describe "reloadConfig"
    (it "should report an error for a non-existent configuration file"
     (do
         tempdir <- catch getTemporaryDirectory (\(_::SomeException) -> return ".")
         let file = tempdir </> "heyefi.config"
         output <- capture_ (reloadConfig file)
         (pack ("Could not find configuration file at " ++ file)) `isInfixOf` (pack output) `shouldBe` True ))
