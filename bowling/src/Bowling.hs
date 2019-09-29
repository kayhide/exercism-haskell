{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Bowling (score, BowlingError(..)) where

import Control.Monad (foldM, when)
import Data.Bool (bool)


data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = finalize =<< foldM roll (Game [] []) (zip rolls [0..])


data Frame = Open [Int] | Spare [Int] | Strike
  deriving (Eq, Show)

toRolls :: Frame -> [Int]
toRolls = \case
  Open ns  -> ns
  Spare ns -> ns
  Strike   -> pure 10

fromRolls :: [Int] -> Frame
fromRolls xs = case xs of
  [_, _] -> bool Spare Open (sum xs < 10) xs
  [10]   -> Strike
  _      -> Open xs

data Game =
  Game { current :: [Int], frames :: [Frame] }
  deriving (Eq, Show)


roll :: Game -> (Int, Int) -> Either BowlingError Game
roll Game {..} (i, index) = do
  let pins = ((30 - sum current - 1) `mod` 10) + 1
  when (i < 0 || pins < i || length frames > 9) $
    Left $ InvalidRoll index i

  let current' = i : current
  let cont = pure $ Game current' frames
  let next = pure $ Game [] $ fromRolls (reverse current') : frames
  case length frames of
    9 -> case current' of
      [_, _, _] -> next
      [_, 10]   -> cont
      [_, _]    -> bool next cont $ sum current' == 10
      _         -> cont
    _ -> case current' of
      [_, _] -> next
      [10]   -> next
      _      -> cont

finalize :: Game -> Either BowlingError Int
finalize Game {..} = do
  when (length frames < 10) $ Left IncompleteGame
  pure $ calc $ reverse frames
  where
    calc :: [Frame] -> Int
    calc = \case
      [] -> 0
      x : xs -> calc xs + case x of
        Open ns -> sum ns
        Spare _ -> 10 + (sum . take 1 $ concatMap toRolls xs)
        Strike  -> 10 + (sum . take 2 $ concatMap toRolls xs)
