module Isogram (isIsogram) where

import Control.Monad (foldM, guard)
import Data.Bits (zeroBits, testBit, setBit)
import Data.Char (isAlpha, isAscii, ord, toLower)
import Data.Maybe (isJust)

isIsogram :: String -> Bool
isIsogram =
  isJust
  . foldM f zeroBits
  . map (subtract (ord 'a') . ord . toLower)
  . filter ((&&) <$> isAlpha <*> isAscii)
  where
    f :: Int -> Int -> Maybe Int
    f acc x = setBit acc x <$ guard (not $ testBit acc x)
