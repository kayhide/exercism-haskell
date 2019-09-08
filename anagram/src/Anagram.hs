module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter pred'
  where
    pred' :: String -> Bool
    pred' xs' = xs'' /= lower' && sort xs'' == sorted'
      where xs'' = toLower <$> xs'

    lower' :: String
    lower' = toLower <$> xs

    sorted' :: String
    sorted' = sort lower'
