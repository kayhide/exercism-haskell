module Sublist (sublist) where

import Data.List (isPrefixOf, tails)


sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys =
  case (xs `isSublistOf` ys, ys `isSublistOf` xs) of
    (True, True)  -> Just EQ
    (True, False) -> Just LT
    (False, True) -> Just GT
    _             -> Nothing

isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf xs = any (xs `isPrefixOf`) . tails
