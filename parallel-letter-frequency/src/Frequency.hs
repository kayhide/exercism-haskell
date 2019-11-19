module Frequency (frequency) where

import Control.Parallel.Strategies (parBuffer, rseq, withStrategy)
import Data.Char (isAlpha, toLower)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = Map.unionsWith (+) $ withStrategy (parBuffer nWorkers rseq) $ build <$> texts


build :: Text -> Map Char Int
build = foldl' f Map.empty . filter isAlpha . fmap toLower . Text.unpack
  where
    f :: Map Char Int -> Char -> Map Char Int
    f acc c = Map.insertWith (+) c 1 acc

