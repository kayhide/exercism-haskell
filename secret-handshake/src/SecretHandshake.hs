module SecretHandshake (handshake) where

import Data.Bits (testBit)
import Data.Bool (bool)
import Data.Function ((&))
import Data.List (foldl')

handshake :: Int -> [String]
handshake n = foldl' (&) [] $ zipWith (bool id) fs bits
  where
    bits :: [Bool]
    bits = testBit n <$> [0 .. 4]

    fs :: [[String] -> [String]]
    fs =
      [ (<> ["wink"])
      , (<> ["double blink"])
      , (<> ["close your eyes"])
      , (<> ["jump"])
      , reverse
      ]
