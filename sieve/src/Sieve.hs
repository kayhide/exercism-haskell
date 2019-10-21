module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

import qualified Data.Set as Set
import Data.Set (Set, lookupGT, (\\))


primesUpTo :: Integer -> [Integer]
primesUpTo n = Set.toList $ f 2 $ Set.fromList [2 .. n]
  where
    f :: Integer -> Set Integer -> Set Integer
    f x xs = maybe id f =<< lookupGT x $ sieve x xs

    sieve :: Integer -> Set Integer -> Set Integer
    sieve x = (\\ Set.fromList [x * 2, x * 3 .. n])
