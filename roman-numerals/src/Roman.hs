{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Roman (numerals) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.List (unfoldr)


numerals :: Integer -> Maybe String
numerals n = fmap mconcat . traverse toRoman . reverse $ digits n


toRoman :: (Int, Int) -> Maybe String
toRoman (d, n) = build n . take 3 $ drop (d * 2) "IVXLCDM"

build :: Int -> [Char] -> Maybe String
build n cs = case n of
  0 -> pure []
  1 -> sequence [i]
  2 -> sequence [i, i]
  3 -> sequence [i, i, i]
  4 -> sequence [i, v]
  5 -> sequence [v]
  6 -> sequence [v, i]
  7 -> sequence [v, i, i]
  8 -> sequence [v, i, i, i]
  9 -> sequence [i, x]
  _ -> Nothing
  where
    i = cs `atMay` 0 :: Maybe Char
    v = cs `atMay` 1 :: Maybe Char
    x = cs `atMay` 2 :: Maybe Char


-- * Helper functions

-- | Returns a list of digit numbers along with their indices.
digits :: forall i. Integral i => i -> [(Int, Int)]
digits = zip [0 ..] . unfoldr f
  where
    f :: i -> Maybe (Int, i)
    f x = (fromIntegral . snd &&& fst) (x `divMod` 10) <$ guard (x > 0)

-- | Maybe version of `(!!)`.
atMay :: [a] -> Int -> Maybe a
atMay [] _      = Nothing
atMay (x : _) 0 = pure x
atMay xs n      = drop n xs `atMay` 0
