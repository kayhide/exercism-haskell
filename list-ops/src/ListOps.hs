{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

import Data.Bool (bool)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z = \case
  [] -> z
  (!x) : xs -> foldl' f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = \case
  [] -> z
  x : xs -> f x (foldr f z xs)

length :: [a] -> Int
length = foldr (+) 0 . map (const 1)

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = \case
  [] -> []
  x : xs -> f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p = \case
  [] -> []
  x : xs -> bool id (x :) (p x) $ filter p xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat = foldr (++) []
