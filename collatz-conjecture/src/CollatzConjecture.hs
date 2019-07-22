module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x
  | x > 0 = Just . fromIntegral . length . takeWhile (/= 1) $ iterate run x
  | otherwise = Nothing

run :: Integer -> Integer
run x
  | even x = x `div` 2
  | otherwise = 3 * x + 1
