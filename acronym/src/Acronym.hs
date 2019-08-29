{-# LANGUAGE LambdaCase #-}
module Acronym (abbreviate) where

import Data.Text (Text, pack, unpack, toUpper)
import Data.Char (isUpper, isAlpha, isLower)
import Control.Monad (guard)

abbreviate :: Text -> Text
abbreviate = toUpper . pack . f . unpack
  where
    f :: String -> String
    f xs =
      zip (Nothing : fmap Just xs) xs
      >>= \case
        (Nothing, x) -> pure x
        (Just y, x) -> do
          guard $ isAlpha x
          guard $ not (isAlpha y || (y == '\'')) || (isLower y && isUpper x)
          pure x
