module IsbnVerifier (isbn) where

import Control.Monad (guard, zipWithM)
import Data.Maybe (isJust)
import Text.Read (readMaybe)


isbn :: String -> Bool
isbn xs = isJust $ do
    let xs' = filter (/= '-') xs
    guard $ length xs' == 10

    ns <- zipWithM ($) (reader <$> [0 ..]) xs'
    guard $ sum ns `mod` 11 == 0


-- | Reader function
-- Builds a function to read `i`-th `Char` and to return a value which is
-- multiplied by a specific coefficient.
-- Its function retuns `Nothing` if input `Char` is invalid at that position.

reader :: Int -> Char -> Maybe Int
reader i c
  | 0 <= i && i < 9 = (* (10 - i)) <$> readMaybe [c]
  | i == 9 && c == 'X' = Just 10
  | i == 9 = readMaybe [c]
  | otherwise = Nothing
