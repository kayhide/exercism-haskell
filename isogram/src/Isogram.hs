module Isogram (isIsogram) where

import Control.Monad (foldM, guard)
import Data.Bits (bit, zeroBits, (.|.))
import Data.Char (isAlpha, isAscii, ord, toLower)
import Data.Maybe (isJust)

isIsogram :: String -> Bool
isIsogram =
  isJust
  . foldM f zeroBits
  . toBits
  . filter ((&&) <$> isAlpha <*> isAscii)
  where
    f :: Int -> Int -> Maybe Int
    f acc x = acc .|. x <$ guard (acc < acc .|. x)

toBits :: String -> [Int]
toBits = map (bit . subtract (ord 'a') . ord . toLower)
