module HEyefi.SoapParserSpec where

import Test.Hspec

import HEyefi.Soap

sampleStartSessionRequest :: String
sampleStartSessionRequest = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:StartSession><macaddress>0018562de4ce</macaddress><cnonce>6eb0444343c1953e47fb28181bb4e47f</cnonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp></ns1:StartSession></SOAP-ENV:Body></SOAP-ENV:Envelope>"

sampleGetPhotoRequest :: String
sampleGetPhotoRequest = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:GetPhotoStatus><credential>7daa9ecf3a9f135f5bb30541ed84fcfb</credential><macaddress>0018562de4ce</macaddress><filename>IMG_2195.JPG.tar</filename><filesize>125952</filesize><filesignature>736ffb7fa20f1708fd300c58c0aabb61</filesignature><flags>4</flags></ns1:GetPhotoStatus></SOAP-ENV:Body></SOAP-ENV:Envelope>"

sampleMarkLastPhotoRequest :: String
sampleMarkLastPhotoRequest = "<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="EyeFi/SOAP/EyeFilm"><SOAP-ENV:Body><ns1:MarkLastPhotoInRoll><macaddress>001856417729</macaddress><mergedelta>0</mergedelta></ns1:MarkLastPhotoInRoll></SOAP-ENV:Body></SOAP-ENV:Envelope>"

spec :: Spec
spec = return ()
