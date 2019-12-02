module Triplet (tripletsWithSum) where

import Control.Monad (guard)

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = do
  a <- [1 .. sum `div` 3]
  let (b, m) = (sum * sum - 2 * sum * a) `divMod` (2 * (sum - a))
  guard $ m == 0 && (a < b)
  pure (a, b, sum - (a + b))


-- |
-- a + b + c = s
--         c = s - (a + b)
-- ==>
-- a^2 + b^2 = c^2
--           = (s - (a + b))^2
--           = s^2 - 2*s*(a + b) + (a + b)^2
--           = s^2 - 2*s*(a + b) + a^2 + 2*a*b + a^2
--         0 = s^2 - 2*s*(a + b) + 2*a*b
--           = s^2 - 2*s*a + b*(-2*s + 2*a)
--           = s^2 - 2*s*a - b*(2*s - 2*a)
--           = (s^2 - 2*s*a) / (2*s - 2*a) - b
--         b = (s^2 - 2*s*a) / (2*s - 2*a)
