module Atbash (decode, encode) where

import Control.Monad (guard)
import Data.Char (chr, isAlpha, isDigit, ord, toLower)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)

decode :: String -> String
decode = catMaybes . fmap convert

encode :: String -> String
encode = unwords . chunksOf 5 . catMaybes . fmap convert

convert :: Char -> Maybe Char
convert c
  | isAlpha c = pure . chr . (ord 'z' -) . subtract (ord 'a') . ord $ toLower c
  | isDigit c = pure c
  | otherwise = Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr f
  where
    f :: [a] -> Maybe ([a], [a])
    f xs = splitAt n xs <$ guard (not $ null xs)
