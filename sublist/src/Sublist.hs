module Sublist (sublist) where

import Data.List (isPrefixOf, tails)


sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys =
  case (xs `isSublistOf` ys, ys `isSublistOf` xs) of
    (True, True)  -> Just EQ
    (True, False) -> Just LT
    (False, True) -> Just GT
    _             -> Nothing

-- | I found this implementation is actually the same to `Data.List.isInfix`.
--   http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.OldList.html#isInfixOf
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf xs = any (xs `isPrefixOf`) . tails
