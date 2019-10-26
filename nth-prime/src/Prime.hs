module Prime (nth) where

import Control.Applicative ((<**>))
import Data.List (uncons)


nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise = fst <$> uncons (drop (n - 1) primes)


primes :: [Integer]
primes = 2 : 3 : filter isPrime ([6, 12 ..] <**> [pred, succ])

isPrime :: Integer -> Bool
isPrime n = all ((/= 0) . (n `mod`)) $ takeWhile (< n `div` 2) primes
