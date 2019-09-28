module Triplet (tripletsWithSum) where

import Control.Monad (guard)

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = do
  c <- [sum `div` 3 .. sum - 3]
  let cc = c * c
  let ab = sum - c
  a <- [1 .. ab `div` 2]
  let b = ab - a
  guard $ a * a + b * b == cc
  pure (a, b, c)
