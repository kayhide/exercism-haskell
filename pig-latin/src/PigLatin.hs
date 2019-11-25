{-# LANGUAGE TupleSections #-}
module PigLatin (translate) where

import Data.List (isSuffixOf)

translate :: String -> String
translate = unwords . fmap pig . words


pig :: String -> String
pig = f . syllabify . ("", "",)
  where
    f :: Syllable -> String
    f (c, v, xs) = concat [v, xs, c, "ay"]


type Vowal = String
type Consonant = String
type Syllable = (Consonant, Vowal, String)

syllabify :: Syllable -> Syllable
syllabify (c, v, "") = (c, v, "")
syllabify (c@(_ : _), "", 'y' : xs) = (c, "y", xs)
syllabify (c@"y", "", xs@('t' : _)) = ("", c, xs)
syllabify (c@"x", "", xs@('r' : _)) = ("", c, xs)
syllabify (c, "", x : xs)
  | "qu" `isSuffixOf` (c <> [x]) = (c <> [x], "", xs)
  | x `elem` "aeiou" = (c, [x], xs)
  | otherwise = syllabify (c <> [x], "", xs)
