module HEyefi.Texts where

import Data.Text (Text)
import Data.Monoid ((<>))

cardsFormatDoesNotMatch :: Text
cardsFormatDoesNotMatch =
  "Format of cards does not match [[MacAddress, Key],[MacAddress, Key],...]."

missingCardsDefinition :: Text
missingCardsDefinition = "Configuration is missing a definition for `cards`."

uploadDirFormatDoesNotMatch :: Text
uploadDirFormatDoesNotMatch =
  "Format of upload_dir does not match \"/path/to/upload/dir\""

missingUploadDirDefinition :: Text
missingUploadDirDefinition =
  "Configuration is missing a definition for `upload_dir`."


noUploadKeyInConfiguration :: Text -> Text
noUploadKeyInConfiguration =
  (<>) "No upload key found in configuration for macaddress: "

invalidCredential :: Text -> Text -> Text
invalidCredential expected actual =
  "Invalid credential in GetPhotoStatus request. Expected: "
   <> expected <> " Actual: " <> actual

listeningOnPort :: Text -> Text
listeningOnPort = (<>) "Listening on port "

gotStartSessionRequest :: Text
gotStartSessionRequest = "Got StartSession request"

gotGetPhotoStatusRequest :: Text
gotGetPhotoStatusRequest = "Got GetPhotoStatus request"

gotMarkLastPhotoInRollRequest :: Text
gotMarkLastPhotoInRollRequest = "Got MarkLastPhotoInRoll request"

loadedConfiguration :: Text
loadedConfiguration = "Loaded configuration"

tryingToLoadConfiguration :: Text -> Text
tryingToLoadConfiguration = (<>) "Trying to load configuration at "

errorParsingConfigurationFile :: Text -> Text -> Text
errorParsingConfigurationFile p msg =
  "Error parsing configuration file at " <>
  p <>
  " with message: " <>
  msg

couldNotFindConfigurationFile :: Text -> Text
couldNotFindConfigurationFile = (<>) "Could not find configuration file at "

didNotMatchDispatch :: Text
didNotMatchDispatch = "did not match dispatch"

notADefinedSoapAction :: Text -> Text
notADefinedSoapAction sa = sa <> " is not a defined SoapAction yet"

programDescription :: Text
programDescription = "A server daemon for Eye-Fi SD cards."

programHeaderDescription :: Text
programHeaderDescription = "heyefi - a server for Eye-Fi SD cards"

gotUploadRequest :: Text
gotUploadRequest = "Got Upload request"

uploadedTo :: Text -> Text
uploadedTo = (<>) "Uploaded to "
