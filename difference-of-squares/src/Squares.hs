module Squares (difference, squareOfSum, sumOfSquares) where

import Control.Arrow ((&&&))

difference :: Integral a => a -> a
difference n = uncurry (-) $ (squareOfSum' &&& sumOfSquares') [1 .. n]

squareOfSum :: Integral a => a -> a
squareOfSum n = squareOfSum' [1 .. n]

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sumOfSquares' [1 .. n]


squareOfSum' :: Integral a => [a] -> a
squareOfSum' = sq . sum

sumOfSquares' :: Integral a => [a] -> a
sumOfSquares' = sum . fmap sq

sq :: Integral a => a -> a
sq x = x * x
