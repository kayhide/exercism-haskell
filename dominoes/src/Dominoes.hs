module Dominoes (chain) where

import Control.Monad (guard)
import Data.List (delete)
import Data.Tuple (swap)
import Data.Maybe (listToMaybe, maybeToList)


type Domino = (Int, Int)

chain :: [Domino] -> Maybe [Domino]
chain = go []


go :: [Domino] -> [Domino] -> Maybe [Domino]
go [] [] = pure []
go [] (y : ys) = go [y] ys
go xs [] = xs <$ guard (fst (head xs) == snd (last xs))
go xs@((x, _) : _) ys = listToMaybe $ do
  (z, ys') <- candidates x ys
  maybeToList $ go (z : xs) ys'

candidates :: Int -> [Domino] -> [(Domino, [Domino])]
candidates x xs = collect xs <> collect (swap <$> xs)
  where
    collect :: [Domino] -> [(Domino, [Domino])]
    collect xs' = do
      y <- filter ((==) x . snd) xs'
      pure (y, delete y xs')
