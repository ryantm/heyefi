module HEyefi.Constant where

port :: Int
port = 59278

configPath :: String
configPath = "/etc/heyefi/heyefi.config"

-- upload_uid :: Int
-- upload_uid = 1000
-- upload_gid :: Int
-- upload_gid = 1000
-- upload_file_mode :: Int
-- upload_file_mode = 420
-- upload_dir_mode :: Int
-- upload_dir_mode = 509

multipartBodyBoundary :: String
multipartBodyBoundary =
  "---------------------------02468ace13579bdfcafebabef00d"


-- Messages
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
