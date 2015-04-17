module HEyefi.SoapParserSpec where

sampleStartSessionRequest = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"EyeFi/SOAP/EyeFilm\"><SOAP-ENV:Body><ns1:StartSession><macaddress>0018562de4ce</macaddress><cnonce>6eb0444343c1953e47fb28181bb4e47f</cnonce><transfermode>34</transfermode><transfermodetimestamp>1356903384</transfermodetimestamp></ns1:StartSession></SOAP-ENV:Body></SOAP-ENV:Envelope>"
