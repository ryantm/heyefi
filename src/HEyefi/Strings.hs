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
  ("Invalid credential in GetPhotoStatus request. Expected: "
   ++ expected ++ " Actual: " ++ actual)
