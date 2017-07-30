module HEyefi.SpecPrelude (
  module X,
  tshouldContain,
  capture_) where

import HEyefi.Prelude as X
import Test.Hspec as X
import qualified Test.Hspec as HS
import qualified System.IO.Silently as S

tshouldContain :: Text -> Text -> HS.Expectation
tshouldContain actual expected =
  unpack actual `shouldContain` unpack expected


capture_ :: IO a -> IO Text
capture_ a = fmap pack (S.capture_ a)
