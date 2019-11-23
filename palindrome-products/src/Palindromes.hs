{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
module Palindromes (largestPalindrome, smallestPalindrome) where

import Control.Monad (guard)


newtype Large = Large (Integer, [(Integer, Integer)])

instance Semigroup Large where
  x@(Large (x', xs)) <> y@(Large (y', ys)) = case compare x' y' of
    LT -> y
    EQ -> Large (x', xs <> ys)
    GT -> x

newtype Largest = Largest { getLargest :: Maybe (Integer, [(Integer, Integer)]) }
  deriving Semigroup via (Maybe Large)
  deriving Monoid via (Maybe Large)


newtype Small = Small (Integer, [(Integer, Integer)])

instance Semigroup Small where
  x@(Small (x', xs)) <> y@(Small (y', ys)) = case compare x' y' of
    LT -> x
    EQ -> Small (x', xs <> ys)
    GT -> y

newtype Smallest = Smallest { getSmallest :: Maybe (Integer, [(Integer, Integer)]) }
  deriving Semigroup via (Maybe Small)
  deriving Monoid via (Maybe Small)


largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome x y =  getLargest $ mconcat $ Largest . pure <$> build x y

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome x y = getSmallest $ mconcat $ Smallest . pure <$> build x y


build :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
build minFactor maxFactor = do
  x <- [minFactor .. maxFactor]
  y <- [x .. maxFactor]
  guard $ isPalindrome $ x * y
  pure $ (x * y, [(x, y)])


isPalindrome :: Integer -> Bool
isPalindrome (show -> digits) = digits == reverse digits
