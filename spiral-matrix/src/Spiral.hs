module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n
  | n < 1 = []
  | n == 1 = [[1]]
  | otherwise = wrap . roll . add (2 * n - 1) . spiral $ pred n


add :: Int -> [[Int]] -> [[Int]]
add x = (fmap . fmap) (+ x)

roll :: [[Int]] -> [[Int]]
roll = fmap reverse . reverse

wrap :: [[Int]] -> [[Int]]
wrap xss = [1 .. n] : zipWith (<>) xss (pure <$> [n + 1..])
  where
    n :: Int
    n = length xss + 1
