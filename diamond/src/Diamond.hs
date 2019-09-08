{-# LANGUAGE LambdaCase #-}
module Diamond (diamond) where

import Data.Char (chr, ord)

diamond :: Char -> Maybe [String]
diamond c = do
  xs <- piramid c
  pure $ reverse (drop 1 xs) <> xs

-- | Builds:
--   C...C
--   .B.B.
--   ..A..
piramid :: Char -> Maybe [String]
piramid c = do
  xs <- line c
  pure $ (\cs -> cs <> drop 1 (reverse cs)) <$> xs

-- | Builds:
--   C..
--   .B.
--   ..A
line :: Char -> Maybe [String]
line = \case
  'A' -> Just ["A"]
  c -> do
    xs <- line (chr $ ord c - 1)
    let x' = c : replicate (length xs) ' '
    let xs' = (' ' :) <$> xs
    pure $ x' : xs'
