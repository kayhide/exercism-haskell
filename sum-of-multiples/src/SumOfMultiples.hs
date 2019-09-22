module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub $ concatMap f factors
  where
    f :: Integer -> [Integer]
    f 0 = []
    f x = takeWhile (< limit) $ iterate (+ x) x
