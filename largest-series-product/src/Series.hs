module Series (Error(..), largestProduct) where

import Control.Monad (foldM)
import Data.List (elemIndex, tails)


data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size xs
  | size < 0 || length xs < size = Left InvalidSpan
  | size == 0 = pure 1
  | otherwise = foldM f 0 . takeWhile ((== size) . length) $ take size <$> tails xs
  where
    f :: Integer -> String -> Either Error Integer
    f acc cs = max acc . product <$> readDigits cs


readDigits :: String -> Either Error [Integer]
readDigits = foldM f []
  where
    f :: [Integer] -> Char -> Either Error [Integer]
    f acc c = case elemIndex c ['0' .. '9'] of
      Nothing -> Left $ InvalidDigit c
      Just i  -> pure $ fromIntegral i : acc
