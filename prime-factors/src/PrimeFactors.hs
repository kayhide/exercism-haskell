module PrimeFactors (primeFactors) where

import Control.Monad (guard)
import Data.List (unfoldr)


primeFactors :: Integer -> [Integer]
primeFactors x = unfoldr f (2, x)
  where
    f :: (Integer, Integer) -> Maybe (Integer, (Integer, Integer))
    f (n, x') = do
      guard $ n <= x'
      case x' `divMod` n of
        (x'', 0) -> pure (n, (n, x''))
        _        -> f ((n + 1), x')
