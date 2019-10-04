module Phone (number) where

import Control.Monad (guard)
import Data.Char (isDigit)


number :: String -> Maybe String
number xs = do
  let temp = "NXXNXXXXXX"
  let xs' = dropWhile (== '1') $ filter isDigit xs
  guard $ length temp == length xs'
  traverse f $ zip temp xs'

  where
    f :: (Char, Char) -> Maybe Char
    f (x, c) = case x of
      'N' -> c <$ guard (c `notElem` "01")
      'X' -> pure c
      _   -> Nothing
