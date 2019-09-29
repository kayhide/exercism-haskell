{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Bowling (score, BowlingError(..)) where

import Control.Monad (foldM, when)
import Data.Bool (bool)


data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = finalize =<< foldM roll (Game [] []) rolls

type Frame = [Int]

data Game =
  Game { current :: Frame, frames :: [Frame] }
  deriving (Eq, Show)


roll :: Game -> Int -> Either BowlingError Game
roll Game {..} i = do
  let pins = ((30 - sum current - 1) `mod` 10) + 1
  when (i < 0 || pins < i || length frames > 9) $
    Left $ InvalidRoll (length . concat $ current : frames) i

  let current' = i : current
  let cont = pure $ Game current' frames
  let next = pure $ Game [] (current' : frames)
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
  pure $ calc $ reverse <$> reverse frames
  where
    calc :: [Frame] -> Int
    calc = \case
      [] -> 0
      xs : xss -> calc xss + case (xs, sum xs) of
        ([_], 10)    -> 10 + (sum . take 2 $ concat xss)
        ([_, _], 10) -> 10 + (sum . take 1 $ concat xss)
        (_, s)       -> s
