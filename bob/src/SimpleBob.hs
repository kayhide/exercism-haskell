module SimpleBob (responseFor) where

import Data.Bits (bit, setBit, testBit, zeroBits)
import Data.Char (isDigit, isLower, isUpper)

responseFor :: String -> String
responseFor xs = case testBit (chars xs) <$> [0..3] of
  [False, True , _    , True ] -> "Calm down, I know what I'm doing!"
  [False, True , _    , _    ] -> "Whoa, chill out!"
  [_    , _    , _    , True ] -> "Sure."
  [False, False, False, False] -> "Fine. Be that way!"
  _                            -> "Whatever."

chars :: String -> Int
chars [] = zeroBits
chars (' ' : xs) = chars xs
chars (x : ' ' : xs) = chars (x : xs)
chars "?" = bit 3
chars (x : xs)
  | isLower x = setBit (chars xs) 0
  | isUpper x = setBit (chars xs) 1
  | isDigit x = setBit (chars xs) 2
  | otherwise = chars xs
