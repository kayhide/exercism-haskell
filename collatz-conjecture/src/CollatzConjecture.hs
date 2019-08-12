module CollatzConjecture (collatz) where

import Data.List (elemIndex)

collatz :: Integer -> Maybe Integer
collatz x
  | x > 0 = fmap fromIntegral . elemIndex 1 . iterate step $ x
  | otherwise = Nothing

step :: Integer -> Integer
step x
  | even x = x `div` 2
  | otherwise = 3 * x + 1
