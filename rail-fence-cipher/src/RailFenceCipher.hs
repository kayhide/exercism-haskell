module RailFenceCipher (encode, decode) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

encode :: Int -> String -> String
encode = go Vector.backpermute

decode :: Int -> String -> String
decode = go forpermute

go :: (Vector Char -> Vector Int -> Vector Char) -> Int -> String -> String
go mapper n str = Vector.toList $ mapper (Vector.fromList str) (Vector.fromList $ indices)
  where
    indices :: [Int]
    indices = fst <$> sortBy (compare `on` snd) coords

    coords :: [(Int, Int)]
    coords = take (length str) $ zip [0 ..] $ saw n


-- | Generate an infinite list of:
--   `[0, 1, ... n - 2, n - 1, n - 2, .... 1, 0, 1 ...]`
saw :: Int -> [Int]
saw n = cycle $ [0 .. n - 2] <> [n - 1, n - 2 .. 1]


forpermute :: Vector a -> Vector Int -> Vector a
forpermute xs is = Vector.create $ do
  ys <- MVector.new $ length xs
  sequence_ $ Vector.zipWith (MVector.write ys) is xs
  pure ys
