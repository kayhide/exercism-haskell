{-# LANGUAGE LambdaCase #-}
module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Control.Arrow (first, second)
import Control.Monad (guard)
import Control.Monad.State (State, execState, gets)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.List (foldl')
import Data.Maybe (isNothing)
import Data.Set (Set)
import Optics
import Optics.State.Operators


data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type Board = [String]

territories :: Board -> [(Set Coord, Maybe Color)]
territories board = foldl' f [] coords
  where
    f :: [(Set Coord, Maybe Color)] -> Coord -> [(Set Coord, Maybe Color)]
    f acc coord =
      bool
        (maybe acc (: acc) $ territoryFor board coord)
        acc
        $ or $ acc ^.. traversed % _1 % contains coord

    coords :: [Coord]
    coords = case board of
      []    -> []
      l : _ -> (,) <$> [1 .. length l] <*> [1 .. length board]

territoryFor :: Board -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = do
  guard $ isInside board coord
  guard $ isNothing $ board <!> coord
  pure $ execState (fill board coord) (mempty, mempty) & _2 %~ single

  where
    single :: Foldable f => f a -> Maybe a
    single xs = case xs ^.. folded of
      [x] -> Just x
      _   -> Nothing

fill :: Board -> Coord -> State (Set Coord, Set Color) ()
fill board coord =
  gets (^. _1 % contains coord)
  >>= bool go (pure ())
  where
    go :: State (Set Coord, Set Color) ()
    go = case board <!> coord of
      Nothing -> do
        _1 % contains coord .= True
        traverse_ (fill board) $ adjacents board coord
      Just c ->
        _2 % contains c .= True

adjacents :: Board -> Coord -> [Coord]
adjacents board (x, y) =
  filter (isInside board) $ [first, second] <*> [pred, succ] <*> [(x, y)]

isInside :: Board -> Coord -> Bool
isInside board (x, y) = case board of
  []    -> False
  l : _ -> 1 <= x && x <= length l && 1 <= y && y <= length board

(<!>) :: Board -> Coord -> Maybe Color
(<!>) board (x, y) =
  board ^? ix (y - 1) % ix (x - 1)
  >>= \case
    'B' -> pure Black
    'W' -> pure White
    _   -> Nothing
