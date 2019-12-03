module RailFenceCipher (encode, decode) where

import Data.List (sortOn)


encode :: Int -> String -> String
encode n = sortWith (saw n)

decode :: Int -> String -> String
decode n str = sortWith (sortWith (saw n) [0 .. length str - 1]) str

sortWith :: [Int] -> [a] -> [a]
sortWith is xs = snd <$> sortOn fst (zip is xs)

-- | Generate an infinite list of:
--   `[0, 1, ... n - 2, n - 1, n - 2, .... 1, 0, 1 ...]`
saw :: Int -> [Int]
saw n = cycle $ [0 .. n - 2] <> [n - 1, n - 2 .. 1]
