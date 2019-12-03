{-# LANGUAGE ViewPatterns #-}
module RailFenceCipher (encode, decode) where

import Control.Monad (guard)
import Data.Bool (bool)
import Data.List (transpose)
import Data.Maybe (catMaybes)

encode :: Int -> String -> String
encode n = concat . zipWith row [0 .. pred n] . repeat
  where
    row :: Int -> String -> String
    row r = catMaybes . zipWith f (rows n)
      where
        f :: Int -> Char -> Maybe Char
        f i c = c <$ guard (i == r)

decode :: Int -> String -> String
decode n = concat . fmap catMaybes . transpose . zipWith unrow [0 .. pred n] . chunks
  where
    chunks :: String -> [String]
    chunks str = f counts str
      where
        rows' :: [Int]
        rows' = take (length str) $ rows n

        counts :: [Int]
        counts = (\i -> count i (rows')) <$> [0 .. pred n]

        f _ []                            = []
        f (x : xs) (splitAt x -> (s, s')) = s : f xs s'

    unrow :: Int -> String -> [Maybe Char]
    unrow r = f (rows n)
      where
        f _ [] = []
        f (r' : rs) s@(c : cs) = bool (Nothing : f rs s) (pure c : f rs cs) $ r == r'


-- | Generate an infinite list of:
--   `[0, 1, ... n - 2, n - 1, n - 2, .... 1, 0, 1 ...]`
rows :: Int -> [Int]
rows n = f <$> [0 ..]
  where
    f :: Int -> Int
    f x = bool (\y -> (n - 1) * 2 - y) id =<< (< n) $ x `mod` ((n - 1) * 2)


count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
