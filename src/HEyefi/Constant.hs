module HEyefi.Constant where

import HEyefi.Prelude

port :: Int
port = 59278

configPath :: Text
configPath = "/etc/heyefi/heyefi.config"

multipartBodyBoundary :: Text
multipartBodyBoundary =
  "---------------------------02468ace13579bdfcafebabef00d"
