module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . Set.fromList $ concatMap f factors
  where
    f :: Integer -> [Integer]
    f 0 = []
    f x = takeWhile (< limit) $ iterate (+ x) x
