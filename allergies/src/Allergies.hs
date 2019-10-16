module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (testBit)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

allergies :: Int -> [Allergen]
allergies score = filter (flip isAllergicTo score) [minBound .. maxBound]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = testBit score $ fromEnum allergen
