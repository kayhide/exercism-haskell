module Minesweeper (annotate) where

import Data.Bool (bool)
import Data.Char (intToDigit)
import Optics (iover, itraversed, ix, (%), (<%>), (^?))


annotate :: [String] -> [String]
annotate board = iover (itraversed <%> itraversed) f board
  where
    f :: (Int, Int) -> Char -> Char
    f (y, x) ' ' =
      (bool intToDigit (const ' ') =<< (== 0))
      $ length
      $ filter (== pure '*')
      $ (\j i -> board ^? ix j % ix i) <$> [y - 1 .. y + 1] <*> [x - 1 .. x + 1]
    f _ c = c
