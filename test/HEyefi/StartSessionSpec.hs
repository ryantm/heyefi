{-# LANGUAGE OverloadedStrings #-}
module HEyefi.StartSessionSpec where

import Test.Hspec

import HEyefi.StartSession
import HEyefi.Config (emptyConfig, insertCard)
import HEyefi.Types (runHeyefi)

import Control.Monad.Reader (runReaderT)

spec :: Spec
spec = do
  describe "startSessionResponse"
    (it "should respond the same as eyefiserver2"
     (do
         let c = insertCard "0018562de4ce" "36d61e4e7403a0586702c9159892a062" emptyConfig
         d <- (runReaderT
          (runHeyefi (do
                         startSessionResponse
                           "0018562de4ce"
                           "3623cd00fe5aef4c7ccdc66ddbe2f151"
                           "34"
                           "1356903384"))
          c)

         d `shouldContain` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Body><StartSessionResponse xmlns=\"http://localhost/api/soap/eyefilm\"><credential>7e8f22c00390b6f1ef7bafe7613cbdd2</credential><snonce>"
         d `shouldContain` "</snonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp><upsyncallowed>true</upsyncallowed></StartSessionResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>"))
