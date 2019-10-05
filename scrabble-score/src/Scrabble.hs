module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)


scoreLetter :: Char -> Integer
scoreLetter char
  | c' `elem` "aeioulnrst" = 1
  | c' `elem` "dg" = 2
  | c' `elem` "bcmp" = 3
  | c' `elem` "fhvwy" = 4
  | c' `elem` "k" = 5
  | c' `elem` "jx" = 8
  | c' `elem` "qz" = 10
  | otherwise = 0
  where
    c' :: Char
    c' = toLower char


scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter
