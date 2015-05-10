module HEyefi.Hex where

unhex :: String -> Maybe String
unhex [] = Just []
unhex (a:b:xs) = do
  first <- c a
  second <- c b
  rest <- unhex xs
  return (toEnum ((first * 16) + second) : rest)
unhex [_] = Nothing


c :: Char -> Maybe Int
c '0' = Just 0
c '1' = Just 1
c '2' = Just 2
c '3' = Just 3
c '4' = Just 4
c '5' = Just 5
c '6' = Just 6
c '7' = Just 7
c '8' = Just 8
c '9' = Just 9
c 'a' = Just 10
c 'b' = Just 11
c 'c' = Just 12
c 'd' = Just 13
c 'e' = Just 14
c 'f' = Just 15
c 'A' = Just 10
c 'B' = Just 11
c 'C' = Just 12
c 'D' = Just 13
c 'E' = Just 14
c 'F' = Just 15
c _   = Nothing
