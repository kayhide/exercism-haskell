{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = first (const xs) $ accumulate <$> traverse fromChar xs

accumulate :: [Nucleotide] -> Map Nucleotide Int
accumulate = foldr (\k -> Map.insertWith (+) k 1) empty
  where
    empty :: Map Nucleotide Int
    empty = Map.fromList $ (,0) <$> [A, C, G, T]

fromChar :: Char -> Either Char Nucleotide
fromChar = \case
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  c -> Left c

