module Triangle (TriangleType(..), triangleType) where

import Control.Monad (guard)
import Data.List (group, sort)
import Data.Maybe (fromMaybe)


data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c = fromMaybe Illegal $ categorize =<< verify (sort [a, b, c])


verify :: (Num a, Ord a) => [a] -> Maybe [a]
verify xs = case xs of
  [a, b, c] -> xs <$ guard (a + b > c)
  _         -> Nothing

categorize :: (Num a, Ord a) => [a] -> Maybe TriangleType
categorize xs = case length $ group xs of
  1 -> pure Equilateral
  2 -> pure Isosceles
  3 -> pure Scalene
  _ -> Nothing
