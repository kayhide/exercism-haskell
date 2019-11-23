{-# LANGUAGE ViewPatterns #-}
module Palindromes (largestPalindrome, smallestPalindrome) where

import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome x y =  Map.lookupMax $ build x y

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome x y = Map.lookupMin $ build x y

build :: Integer -> Integer -> Map Integer [(Integer, Integer)]
build minFactor maxFactor = Map.fromListWith (<>) $ do
  x <- [minFactor .. maxFactor]
  y <- [x .. maxFactor]
  guard $ isPalindrome $ x * y
  pure $ (x * y, [(x, y)])


isPalindrome :: Integer -> Bool
isPalindrome (show -> digits) = digits == reverse digits
