{-# LANGUAGE LambdaCase #-}
module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Bool (bool)
import Data.List (transpose, unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map


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
lookupPlants = Map.findWithDefault []


chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr splitAt'
  where
    splitAt' :: [a] -> Maybe ([a], [a])
    splitAt' xs = case splitAt n xs of
      res@(xs', _) -> bool Nothing (Just res) $ length xs' == n
