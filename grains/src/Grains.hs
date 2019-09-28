module Grains (square, total) where

import Safe (atMay)


square :: Integer -> Maybe Integer
square n
 | n <= 64 = atMay nums $ fromIntegral (n - 1)
 | otherwise = Nothing

total :: Integer
total = sum $ take 64 nums


nums :: [Integer]
nums = iterate (* 2) 1
