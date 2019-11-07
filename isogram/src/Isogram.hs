module Isogram (isIsogram) where

import Control.Arrow ((&&&))
import Data.Bits (bit, zeroBits, (.|.))
import Data.Char (isAlpha, isAscii, ord, toLower)

isIsogram :: String -> Bool
isIsogram =
  isStrictlyIncreasing
  . scanl (.|.) zeroBits
  . toBits
  . filter ((&&) <$> isAlpha <*> isAscii)

isStrictlyIncreasing :: [Int] -> Bool
isStrictlyIncreasing = and . uncurry (zipWith (<)) . (id &&& tail)

toBits :: String -> [Int]
toBits = map (bit . subtract (ord 'a') . ord . toLower)
