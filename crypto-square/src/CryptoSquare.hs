module CryptoSquare (encode) where

import Control.Monad (guard)
import Data.Bool (bool)
import Data.Char (isAlpha, isDigit, toLower)
import Data.List (splitAt, transpose, unfoldr)


encode :: String -> String
encode xs = unwords . transpose . fmap pad . chunksOf cols $ fmap toLower xs'
  where
    xs' :: String
    xs' = filter (\c -> isAlpha c || isDigit c) xs

    rows :: Int
    rows = floor . sqrt' $ length xs'

    cols :: Int
    cols = rows + bool 0 1 (rows * rows < length xs')

    pad :: String -> String
    pad s = s <> replicate (cols - length s) ' '


sqrt' :: Int -> Float
sqrt' = sqrt . fromIntegral


chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr f
  where
    f :: [a] -> Maybe ([a], [a])
    f xs = splitAt n xs <$ guard (not $ null xs)
