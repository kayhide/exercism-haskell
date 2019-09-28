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
map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> bool xs (x : xs) (p x)) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
