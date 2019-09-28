module Grains (square, total) where

import Data.Coerce (coerce)
import Data.Monoid (Sum (..))
import Safe (atMay)


square :: Integer -> Maybe Integer
square n
 | n <= 64 = atMay nums $ fromIntegral (n - 1)
 | otherwise = Nothing

total :: Integer
total = getSum . mconcat . coerce $ take 64 nums


nums :: [Integer]
nums = iterate (* 2) 1
