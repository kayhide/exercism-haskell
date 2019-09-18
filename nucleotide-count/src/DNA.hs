{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = maybe (Left xs) (Right . accumulate) $ traverse fromChar xs

accumulate :: [Nucleotide] -> Map Nucleotide Int
accumulate = foldr (flip (Map.insertWith (+)) 1) empty
  where
    empty :: Map Nucleotide Int
    empty = Map.fromList $ (,0) <$> [A, C, G, T]

fromChar :: Char -> Maybe Nucleotide
fromChar = \case
  'A' -> Just A
  'C' -> Just C
  'G' -> Just G
  'T' -> Just T
  _ -> Nothing

