module HEyefi.Strings where

cardsFormatDoesNotMatch :: String
cardsFormatDoesNotMatch =
  "Format of cards does not match [[MacAddress, Key],[MacAddress, Key],...]."

missingCardsDefinition :: String
missingCardsDefinition = "Configuration is missing a definition for `cards`."

uploadDirFormatDoesNotMatch :: String
uploadDirFormatDoesNotMatch =
  "Format of upload_dir does not match \"/path/to/upload/dir\""

missingUploadDirDefinition :: String
missingUploadDirDefinition =
  "Configuration is missing a definition for `upload_dir`."


noUploadKeyInConfiguration :: String -> String
noUploadKeyInConfiguration =
  (++) "No upload key found in configuration for macaddress: "

invalidCredential :: String -> String -> String
invalidCredential expected actual =
  "Invalid credential in GetPhotoStatus request. Expected: "
   ++ expected ++ " Actual: " ++ actual

listeningOnPort :: String -> String
listeningOnPort = (++) "Listening on port "

gotStartSessionRequest :: String
gotStartSessionRequest = "Got StartSession request"

gotGetPhotoStatusRequest :: String
gotGetPhotoStatusRequest = "Got GetPhotoStatus request"

gotMarkLastPhotoInRollRequest :: String
gotMarkLastPhotoInRollRequest = "Got MarkLastPhotoInRoll request"

loadedConfiguration :: String
loadedConfiguration = "Loaded configuration"

tryingToLoadConfiguration :: String -> String
tryingToLoadConfiguration = (++) "Trying to load configuration at "

errorParsingConfigurationFile :: String -> String -> String
errorParsingConfigurationFile p msg =
  "Error parsing configuration file at " ++
  p ++
  " with message: " ++
  msg

couldNotFindConfigurationFile :: String -> String
couldNotFindConfigurationFile = (++) "Could not find configuration file at "

didNotMatchDispatch :: String
didNotMatchDispatch = "did not match dispatch"

notADefinedSoapAction :: String -> String
notADefinedSoapAction sa = sa ++ " is not a defined SoapAction yet"

programDescription :: String
programDescription = "A server daemon for Eye-Fi SD cards."

programHeaderDescription :: String
programHeaderDescription = "heyefi - a server for Eye-Fi SD cards"

gotUploadRequest :: String
gotUploadRequest = "Got Upload request"

uploadedTo :: String -> String
uploadedTo = (++) "Uploaded to "
