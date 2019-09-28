{-# LANGUAGE TupleSections #-}
module Matrix (saddlePoints) where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Control.Arrow ((***))


type Matrix e = Array (Int, Int) e
type Vector e = Array Int e

saddlePoints :: Matrix Int -> [(Int, Int)]
saddlePoints = filter <$> isSaddlePoint <*> Array.indices

isSaddlePoint :: Matrix Int -> (Int, Int) -> Bool
isSaddlePoint matrix (y, x) =
  all (== matrix ! (y, x)) [maximum row', minimum col']
  where
    row' :: Vector Int
    row' = row y matrix

    col' :: Vector Int
    col' = col x matrix


row :: Int -> Matrix e -> Vector e
row y matrix = Array.ixmap (cols matrix) (y,) matrix

col :: Int -> Matrix e -> Vector e
col x matrix = Array.ixmap (rows matrix) (,x) matrix


rows :: Matrix e -> (Int, Int)
rows = (fst *** fst) . Array.bounds

cols :: Matrix e -> (Int, Int)
cols = (snd *** snd) . Array.bounds
