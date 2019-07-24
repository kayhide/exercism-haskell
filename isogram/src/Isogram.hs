module Isogram (isIsogram) where

import           Control.Arrow ((&&&))
import           Data.Bits     (bit, zeroBits, (.|.))
import           Data.Char     (isAlpha, ord, toLower)

isIsogram :: String -> Bool
isIsogram = isOrdered . scanl (.|.) zeroBits . toBits . filter isAlpha

isOrdered :: [Int] -> Bool
isOrdered = and . uncurry (zipWith (<)) . (id &&& tail)

toBits :: String -> [Int]
toBits = map (bit . subtract (ord 'a') . ord . toLower)
