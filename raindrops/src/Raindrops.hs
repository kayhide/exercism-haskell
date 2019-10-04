module Raindrops (convert) where

import Data.Bool (bool)

convert :: Int -> String
convert n =
  case (== 0) . (n `mod`) <$>  [3, 5, 7] of
    [False, False, False] -> show n
    xs -> concat $ zipWith (bool "") ["Pling", "Plang", "Plong"] xs
