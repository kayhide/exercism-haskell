{-# LANGUAGE LambdaCase #-}
module Say (inEnglish) where

import Control.Arrow (first)
import Control.Monad (guard)
import Data.List (unfoldr, foldl')
import Data.Tuple (swap)


inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 = Nothing
  | n == 0 = pure "zero"
  | n < oneTrillion = pure $ inEnglish' n
  | otherwise = Nothing
  where
    oneTrillion :: Integer
    oneTrillion = 1000 * 1000 * 1000 * 1000

inEnglish' :: Integer -> String
inEnglish' n =
  unwordsNonNull
  $ foldl' (\acc (x, y) -> x : y : acc) []
  $ fmap (first chunkToString)
  $ filter ((0 <) . fst)
  $ zip (toChunks n) ["", "thousand", "million", "billion", "trillion"]

unwordsNonNull :: [String] -> String
unwordsNonNull = unwords . filter (not . null)

toChunks :: Integer -> [Integer]
toChunks = unfoldr f
  where
    f :: Integer -> Maybe (Integer, Integer)
    f n = swap (n `divMod` 1000) <$ guard (0 < n)

chunkToString :: Integer -> String
chunkToString n
  | n < 20 = case n of
      0  -> ""
      1  -> "one"
      2  -> "two"
      3  -> "three"
      4  -> "four"
      5  -> "five"
      6  -> "six"
      7  -> "seven"
      8  -> "eight"
      9  -> "nine"
      10 -> "ten"
      11 -> "eleven"
      12 -> "twelve"
      13 -> "thirteen"
      14 -> "fourteen"
      15 -> "fifteen"
      16 -> "sixteen"
      17 -> "seventeen"
      18 -> "eighteen"
      19 -> "nineteen"
      _  -> error "Invalid"
  | n < 100 = case n `divMod` 10 of
      (2, 0) -> "twenty"
      (3, 0) -> "thirty"
      (4, 0) -> "forty"
      (5, 0) -> "fifty"
      (6, 0) -> "sixty"
      (7, 0) -> "seventy"
      (8, 0) -> "eighty"
      (9, 0) -> "ninety"
      (d, i) -> chunkToString (d * 10) <> "-" <> chunkToString i
  | n < 1000 = case n `divMod` 100 of
      (d, i) -> unwordsNonNull [chunkToString d, "hundred", chunkToString i]
  | otherwise = error "Invalid"
