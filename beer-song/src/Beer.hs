{-# LANGUAGE LambdaCase #-}
module Beer (song) where

import Data.Bool (bool)
import Data.Char (toLower, toUpper)
import Data.List (intercalate)

song :: String
song = intercalate "\n" $ verse <$> [99, 98 .. 0]

verse :: Int -> String
verse i = unlines . fmap paragraph $
          [ [beer i `on_` wall, beer i]
          , [action, beer next `on_` wall]
          ]
  where
    action :: String
    action = bool
      "go to the store and buy some more"
      ("take " <> one <> " down and pass it around")
      $ i > 0

    one :: String
    one = bool "it" "one" $ i > 1

    next :: Int
    next = bool 99 (i - 1) $ i > 0


beer :: Int -> String
beer i = bottles i `of_` "beer"

wall :: String
wall = "the wall"

bottles :: Int -> String
bottles = \case
  0 -> "No more bottles"
  1 -> "1 bottle"
  x -> show x <> " bottles"


-- * General functions

paragraph :: [String] -> String
paragraph = capitalize . (<> ".") . intercalate ", "

capitalize :: String -> String
capitalize []       = []
capitalize (x : xs) = toUpper x : (toLower <$> xs)

of_ :: String -> String -> String
of_ x y = x <> " of " <> y

on_ :: String -> String -> String
on_ x y = x <> " on " <> y
