module Hamming (distance) where

import Data.Bool

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance [] (_ : _) = Nothing
distance (_ : _) [] = Nothing
distance (x : xs) (y : ys) = (bool 1 0 (x == y) +) <$>  distance xs ys
