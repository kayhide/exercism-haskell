module Series (slices) where

import Data.List (tails)
import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs =
  takeWhile ((== n) . length) $ fmap digitToInt . take n <$> tails xs
