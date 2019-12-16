{-# LANGUAGE RecordWildCards #-}
module Alphametics (solve) where

import Control.Arrow (first, (***))
import Control.Monad (foldM, guard, join)
import Data.Bool (bool)
import Data.Char (isAlpha)
import Data.List (transpose, (\\))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)


solve :: String -> Maybe [(Char, Int)]
solve = fmap Map.toList . listToMaybe . solve' . parse


data Equation = Equation { lhs :: [String], rhs :: String }
  deriving (Eq, Show)

type Dict = Map Char Int

data State = State
  { equation :: Equation
  , carry    :: Int
  , heads    :: [Char]
  , dict     :: Dict
  }
  deriving (Eq, Show)

solve' :: Equation -> [Dict]
solve' Equation {..} = go $ State eq' 0 heads Map.empty
  where
    heads :: [Char]
    heads = catMaybes $ listToMaybe <$> (rhs : lhs)

    eq' :: Equation
    eq' = Equation (transpose $ reverse <$> lhs) (reverse rhs)


go :: State -> [Dict]
go (State (Equation [] []) 0 _ dict) = pure dict
go state                             = join $ go <$> step state

step :: State -> [State]
step State {..} = do
  let Equation {..} = equation
  let (ls, lhs') = first join $ splitAt 1 lhs
  dict' <- foldM nexts dict ls
  case (sum <$> traverse (dict' !?) ls, rhs) of
    (Just sum', r : rhs') -> do
      let (carry', m) = (sum' + carry) `divMod` 10
      dict'' <- case dict' !? r of
        Nothing -> do
          guard $ m `elem` nums r dict'
          pure $ Map.insert r m dict'
        Just x -> do
          guard $ x == m
          pure dict'
      pure $ State (Equation lhs' rhs') carry' heads dict''
    _ -> []

  where
    nexts :: Dict -> Char -> [Dict]
    nexts d c =
      bool ((\i -> Map.insert c i d) <$> nums c d) (pure d) $ Map.member c d

    nums :: Char -> Dict -> [Int]
    nums c d = [bool 0 1 (c `elem` heads) .. 9] \\ Map.elems d


parse :: String -> Equation
parse =
  uncurry Equation
  . (filter (all isAlpha) *** head . filter (all isAlpha))
  . span  (/= "==")
  . words
