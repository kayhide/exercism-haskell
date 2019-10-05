module Triangle (TriangleType(..), triangleType) where

import Control.Arrow ((&&&))
import Data.List (group, sort)


data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c =
  case (isLegal &&& length . group) $ sort [a, b, c] of
    (True, 1) -> Equilateral
    (True, 2) -> Isosceles
    (True, 3) -> Scalene
    _         -> Illegal


isLegal :: (Num a, Ord a) => [a] -> Bool
isLegal xs = case xs of
  [a, b, c] -> a + b > c
  _         -> False
