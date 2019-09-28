{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Control.Arrow ((&&&))
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Matrix a = Matrix { cols :: Int, rows :: Int, flatten :: Vector a }
  deriving (Eq, Show)

column :: Int -> Matrix a -> Vector a
column x Matrix {..} =
  Vector.backpermute flatten $ Vector.fromList ((+ x) . (* cols) <$> [0 .. rows - 1])

fromList :: [[a]] -> Matrix a
fromList = \case
  [] -> Matrix 0 0 Vector.empty
  xss@(xs : _) -> Matrix (length xs) (length xss) $ Vector.fromList (concat xss)

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ fmap read . words <$> lines xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (rows', cols') matrix = matrix { cols = cols', rows = rows' }

row :: Int -> Matrix a -> Vector a
row x Matrix {..} = Vector.slice (x * cols) cols flatten

shape :: Matrix a -> (Int, Int)
shape Matrix {..} = (rows, cols)

transpose :: Matrix a -> Matrix a
transpose Matrix {..} =
  Matrix rows cols
  $ Vector.backpermute flatten
  $ Vector.fromList
  $ uncurry (+) . ((* cols) . (`mod` rows) &&& (`div` rows)) <$> [0 .. length flatten - 1]
