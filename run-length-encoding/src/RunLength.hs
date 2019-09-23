{-# LANGUAGE LambdaCase #-}
module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group, span)

decode :: String -> String
decode text = case span isDigit text of
  ([], x : xs) -> x : decode xs
  (n, x : xs)  -> replicate (read n) x <> decode xs
  _            -> []


encode :: String -> String
encode = concatMap f . group
  where
    f :: String -> String
    f = \case
      []  -> []
      [x] -> pure x
      xs  -> show (length xs) <> take 1 xs
