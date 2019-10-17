{-# LANGUAGE ScopedTypeVariables #-}
module Base (Error(..), rebase) where

import Control.Monad (foldM, guard, (>=>))
import Data.List (unfoldr)
import Data.Tuple (swap)

import Debug.Trace

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase = decode inputBase >=> encode outputBase


decode :: forall a. Integral a => a -> [a] -> Either (Error a) a
decode base
  | base < 2 = const $ Left InvalidInputBase
  | otherwise = foldM f 0
  where
    f :: a -> a -> Either (Error a) a
    f acc x
      | x < 0 || base <= x = Left $ InvalidDigit x
      | otherwise = pure $ acc * base + x


encode :: forall a. Integral a => a -> a -> Either (Error a) [a]
encode base
  | base < 2 = const $ Left InvalidOutputBase
  | otherwise = pure . reverse . unfoldr f
  where
    f :: a -> Maybe (a, a)
    f x = swap (x `divMod` base) <$ guard (x > 0)
