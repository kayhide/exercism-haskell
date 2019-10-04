{-# LANGUAGE TupleSections #-}
module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map

transform :: Map Int String -> Map Char Int
transform legacyData = Map.fromList . concatMap (\(n, cs) -> (, n) . toLower <$> cs) $ Map.toList legacyData
