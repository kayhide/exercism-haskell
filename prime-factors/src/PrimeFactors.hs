module PrimeFactors (primeFactors) where

import Control.Monad (guard)
import Data.List (unfoldr)


primeFactors :: Integer -> [Integer]
primeFactors x = unfoldr f (possiblePrimes, x)
  where
    f :: ([Integer], Integer) -> Maybe (Integer, ([Integer], Integer))
    f ([], _) = error "Bad divisors"
    f (n : ns, x') = do
      guard $ n <= x'
      case x' `divMod` n of
        (x'', 0) -> pure (n, (n : ns, x''))
        _        -> f (ns, x')

possiblePrimes :: [Integer]
possiblePrimes = 2 : 3 : ([pred, succ] <*> [6, 12 ..])
