module BinarySearch (find) where

import Data.Array


find :: Ord a => Array Int a -> a -> Maybe Int
find xs x = search xs (bounds xs) x


search :: Ord a => Array Int a -> (Int, Int) -> a -> Maybe Int
search xs (lo, hi) x
  | ix < lo || hi < ix = Nothing
  | otherwise = case compare x (xs ! ix) of
      LT -> search xs (lo, ix - 1) x
      EQ -> pure ix
      GT -> search xs (ix + 1, hi) x
  where
    ix :: Int
    ix = (hi + lo) `div` 2
