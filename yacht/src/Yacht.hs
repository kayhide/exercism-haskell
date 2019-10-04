module Yacht (yacht, Category(..)) where

import Data.Bool (bool)
import Data.List (group, nub, sort, sortOn)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice = case category of
  Ones -> sum $  filter (== 1) dice
  Twos -> sum $ filter (== 2) dice
  Threes -> sum $ filter (== 3) dice
  Fours -> sum $ filter (== 4) dice
  Fives ->sum $ filter (== 5) dice
  Sixes ->sum $ filter (== 6) dice
  FullHouse -> case sortOn length $ group $ sort dice of
    [[_, _], [_, _, _]] -> sum dice
    _                   -> 0
  FourOfAKind -> case sortOn length $ group $ sort dice of
    [[_], [x, _, _, _]] -> x * 4
    [[x, _, _, _, _]]   -> x * 4
    _                   -> 0
  LittleStraight -> bool 0 30 $ sort dice == [1 .. 5]
  BigStraight -> bool 0 30 $ sort dice == [2 .. 6]
  Choice -> sum dice
  Yacht -> bool 0 50 $ length (nub dice) == 1
