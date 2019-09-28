module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = take x $ iterate f [1]
  where
    f :: [Integer] -> [Integer]
    f xs = zipWith (+) (0 : xs) (xs <> [0])
