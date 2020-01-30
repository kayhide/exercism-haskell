module Change (findFewestCoins) where

import Data.Maybe (listToMaybe)


findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = listToMaybe $ candidates target coins

candidates :: Integer -> [Integer] -> [[Integer]]
candidates _ [] = []
candidates v (x : xs)
  | v < 0 = []
  | v == 0 = [[]]
  | otherwise =
      candidates v xs
      <> ((x :) <$> candidates (v - x) (x : xs))
