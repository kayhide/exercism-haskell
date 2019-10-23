module Brackets (arePaired) where

import Control.Monad (foldM, guard)


arePaired :: String -> Bool
arePaired = (== pure []) . foldM f []

f :: [Char] -> Char -> Maybe [Char]
f xs c
  | c `elem` "[{(" = pure $ c : xs
  | c `elem` "]})" = case xs of
      []      -> Nothing
      x : xs' -> xs' <$ guard ([x, c] `elem` ["[]", "{}", "()"])
  | otherwise = pure xs
