module Luhn (isValid) where

import Control.Monad (foldM, guard)
import Data.Bool (bool)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Maybe (isJust)


isValid :: String -> Bool
isValid n = isJust $ do
  (_, xs) <- foldM f (cycle [id, double'], []) $ reverse n
  case xs of
    _ : _ : _ -> guard $ sum xs `mod` 10 == 0
    _         -> Nothing

  where
    f :: ([Int -> Int], [Int]) -> Char -> Maybe ([Int -> Int], [Int])
    f (g : gs, acc) c
      | isDigit c = pure (gs, g (digitToInt c) : acc)
      | isSpace c = pure (g : gs, acc)
      | otherwise = Nothing
    f _ _ = Nothing

    double' :: Int -> Int
    double' x = bool id (subtract 9) =<< (9 <) $ x * 2
