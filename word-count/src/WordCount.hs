{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (tails)
import qualified Data.Map as Map


wordCount :: String -> [(String, Int)]
wordCount = Map.toList . Map.fromListWith (+) . fmap (, 1) . words'

words' :: String -> [String]
words' = words . fmap f . takeWhile (not . null) . tails . (' ' :)
  where
    f :: String -> Char
    f (_ : c@(isAlphaNum -> True) : _)                           = toLower c
    f ((isAlphaNum -> True) : c@'\'' : (isAlphaNum -> True) : _) = c
    f _                                                          = ' '
