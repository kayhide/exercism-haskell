{-# LANGUAGE TupleSections #-}
module Alphametics (solve) where

import Control.Arrow ((***))
import Control.Monad (foldM, guard, join)
import Data.Char (isAlpha)
import Data.Containers.ListUtils (nubInt)
import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)


solve :: String -> Maybe [(Char, Int)]
solve puzzle = fmap Map.toList $ listToMaybe $ solve' $ parse puzzle


type Equation = ([String], String)
type Dict = Map Char Int
type State = Map Char [Int]

solve' :: Equation -> [Dict]
solve' eq = do
  dict <- sequence $ initialState eq
  guard $ (==) <$> length <*> length . nubInt $ Map.elems dict
  guard $ isCorrect dict eq
  pure dict

initialState :: Equation -> State
initialState (xs, y) = Map.fromListWith intersect . join $ f <$> (y : xs)
  where
    f :: String -> [(Char, [Int])]
    f = uncurry (<>) . (fmap (, [1 .. 9]) *** fmap (, [0 .. 9])) . splitAt 1

parse :: String -> Equation
parse = (filter (all isAlpha) *** head . filter (all isAlpha)) . span  (/= "==") . words

isCorrect :: Dict -> Equation -> Bool
isCorrect dict (xs, y) = fromMaybe False $ do
  ls <- traverse (apply dict) xs
  r <- apply dict y
  pure $ sum ls == r

apply :: Dict -> String -> Maybe Int
apply dict xs = foldM f 0 xs
  where
    f :: Int -> Char -> Maybe Int
    f acc c = (+ acc * 10) <$> Map.lookup c dict
