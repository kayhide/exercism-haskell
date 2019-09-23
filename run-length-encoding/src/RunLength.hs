{-# LANGUAGE LambdaCase #-}
module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group, span)

decode :: String -> String
decode = f . span isDigit
  where
    f :: (String, String) -> String
    f = \case
      ([], x : xs) -> x : decode xs
      (ds, x : xs) -> replicate (read ds) x <> decode xs
      _            -> []


encode :: String -> String
encode = concatMap f . group
  where
    f :: String -> String
    f = \case
      []         -> []
      xs@[_]     -> xs
      xs@(x : _) -> show (length xs) <> [x]
