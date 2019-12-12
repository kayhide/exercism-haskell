module Change (findFewestCoins) where


findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins =
  case candidates target coins of
    []    -> Nothing
    x : _ -> pure x

candidates :: Integer -> [Integer] -> [[Integer]]
candidates _ [] = []
candidates v (x : xs)
  | v < 0 = []
  | v == 0 = [[]]
  | otherwise =
      candidates v xs
      <> ((x :) <$> candidates (v - x) (x : xs))
