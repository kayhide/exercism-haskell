{-# LANGUAGE LambdaCase #-}
module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

import Data.List (find, (\\))


primesUpTo :: Integer -> [Integer]
primesUpTo n = f 2 [2 .. n]
  where
    f :: Integer -> [Integer] -> [Integer]
    f x xs = maybe id f =<< find (x <) $ sieve x xs

    sieve :: Integer -> [Integer] -> [Integer]
    sieve x = (\\ [x * 2, x * 3 .. n])
