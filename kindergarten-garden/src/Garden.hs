{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Control.Arrow (first)
import Data.List (transpose, unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)


data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = Map String [Plant]

garden :: [String] -> String -> Garden
garden students plants = Map.fromList $ zip students plants'
  where
    plants' :: [[Plant]]
    plants' = fmap concat $ transpose $ chunksOf 2 . fmap toPlant <$> lines plants

    toPlant :: Char -> Plant
    toPlant = \case
      'C' -> Clover
      'G' -> Grass
      'R' -> Radishes
      'V' -> Violets
      _ -> error "Unknown plant"

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student = fromMaybe [] . Map.lookup student


chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr splitAt'
  where
    splitAt' :: [a] -> Maybe ([a], [a])
    splitAt' xs = case splitAt n xs of
      res@(length -> 2, _) -> Just res
      _                    -> Nothing
