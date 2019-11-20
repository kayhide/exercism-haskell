module Transpose (transpose) where

import Control.Arrow (first, (&&&))
import Control.Monad (guard)
import Data.List (uncons, unfoldr)
import Data.Maybe (fromMaybe, isNothing)


transpose :: [String] -> [String]
transpose = unfoldr f
  where
    f :: [String] -> Maybe (String, [String])
    f xss = do
      guard $ not $ all null xss
      pure $ h $ g <$> xss

    g :: String -> (Maybe Char, String)
    g = maybe (Nothing, "") (first pure) . uncons

    h :: [(Maybe Char, String)] -> (String, [String])
    h = (chomp . fmap fst &&& fmap snd)

chomp :: [Maybe Char] -> String
chomp = fmap (fromMaybe ' ') . reverse . dropWhile isNothing . reverse
