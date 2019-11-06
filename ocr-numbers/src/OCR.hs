module OCR (convert) where

import Control.Monad (guard)
import Data.List (splitAt, intercalate, transpose, unfoldr)

convert :: String -> String
convert = intercalate "," . fmap (fmap toDigit . toChunks) . chunksOf 4 . lines

toChunks :: [String] -> [[String]]
toChunks = transpose . fmap (chunksOf 3)


toDigit :: [String] -> Char
toDigit xs = case xs of
    [" _ ", "| |", "|_|", "   "] -> '0'
    ["   ", "  |", "  |", "   "] -> '1'
    [" _ ", " _|", "|_ ", "   "] -> '2'
    [" _ ", " _|", " _|", "   "] -> '3'
    ["   ", "|_|", "  |", "   "] -> '4'
    [" _ ", "|_ ", " _|", "   "] -> '5'
    [" _ ", "|_ ", "|_|", "   "] -> '6'
    [" _ ", "  |", "  |", "   "] -> '7'
    [" _ ", "|_|", "|_|", "   "] -> '8'
    [" _ ", "|_|", " _|", "   "] -> '9'
    _                            -> '?'


chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr f
  where
    f :: [a] -> Maybe ([a], [a])
    f xs = splitAt n xs <$ guard (not $ null xs)
