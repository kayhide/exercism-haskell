{-# LANGUAGE LambdaCase #-}
module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Control.Monad.State (State, execState, gets)
import Data.Bifunctor (first, second)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.List (foldl')
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
    f org coord =
      bool
        (maybe org (: org) $ territoryFor board coord)
        org
        $ or $ org ^.. traversed % _1 % contains coord

    coords :: [Coord]
    coords = case board of
      []    -> []
      l : _ -> (,) <$> [1 .. length l] <*> [1 .. length board]

territoryFor :: Board -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
  case (isInside board coord, board <!> coord) of
    (True, Nothing) ->
      case execState (fill board coord) (mempty, mempty) of
        (coords, colors) -> case colors ^.. folded of
          [c] -> Just (coords, Just c)
          _   -> Just (coords, Nothing)
    _ -> Nothing

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
  filter (isInside board) $ [first, second] <*> [subtract 1, (+ 1)] <*> [(x, y)]

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
