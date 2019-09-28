{-# LANGUAGE LambdaCase #-}
module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = intercalate "\n" $ unlines . this . thing <$> [0 .. 11]

this :: [String] -> [String]
this = \case
  [] -> []
  (x : xs) -> unwords ["This is the", x] : xs

that :: String -> [String] -> [String]
that did = \case
  [] -> []
  (x : xs) -> unwords ["that", did, "the", x] : xs

thing :: Int -> [String]
thing i = case i of
  0  -> pure "house that Jack built."
  1  -> "malt" : that "lay in" thing'
  2  -> "rat" : that "ate" thing'
  3  -> "cat" : that "killed" thing'
  4  -> "dog" : that "worried" thing'
  5  -> "cow with the crumpled horn" : that "tossed" thing'
  6  -> "maiden all forlorn" : that "milked" thing'
  7  -> "man all tattered and torn" : that "kissed" thing'
  8  -> "priest all shaven and shorn" : that "married" thing'
  9  -> "rooster that crowed in the morn" : that "woke" thing'
  10 -> "farmer sowing his corn" : that "kept" thing'
  11 -> "horse and the hound and the horn" : that "belonged to" thing'
  _  -> []

  where
    thing' :: [String]
    thing' = thing $ i - 1
