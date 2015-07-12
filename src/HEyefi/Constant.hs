module HEyefi.Constant where

port :: Int
port = 59278

configPath :: String
configPath = "/etc/heyefi/heyefi.config"

multipartBodyBoundary :: String
multipartBodyBoundary =
  "---------------------------02468ace13579bdfcafebabef00d"
