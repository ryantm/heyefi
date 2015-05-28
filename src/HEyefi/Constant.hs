module HEyefi.Constant where

port :: Int
port = 59278

configPath :: String
configPath = "/home/ryantm/p/heyefi/heyefi.config"

mac_0 :: String
mac_0 = "0018562de4ce"

upload_key_0 :: String
upload_key_0 = "36d61e4e7403a0586702c9159892a062"
upload_dir :: String
upload_dir = "/data/annex/doxie/unsorted"

upload_uid :: Int
upload_uid = 1000
upload_gid :: Int
upload_gid = 1000
upload_file_mode :: Int
upload_file_mode = 420
upload_dir_mode :: Int
upload_dir_mode = 509

geotag_enable :: Int
geotag_enable = 1
geotag_lag :: Int
geotag_lag = 3600
geotag_accuracy :: Int
geotag_accuracy = 140000

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
