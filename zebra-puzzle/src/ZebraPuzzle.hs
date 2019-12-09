{-# LANGUAGE RecordWildCards #-}
module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad (guard)
import Control.Monad.Fix (fix)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (delete, find, groupBy, intersect, sortOn, (\\))
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Optics (Prism', filtered, itoListOf, itraversed, ix, over, prism, to,
               traversed, (%), (%~), (&), (^..), (^?), _Just, _head)


data Resident
  = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Item
  = E | S | U | N | J
  | Red | Green | Ivory | Yellow | Blue
  | Dog | Snails | Fox | Horse | Zebra
  | Coffee | Tea | Milk | OrangeJuice | Water
  | OldGold | Chesterfields | Kools | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

groups :: [[Item]]
groups =
  [ [E .. J]
  , [Red .. Blue]
  , [Dog .. Zebra]
  , [Coffee .. Water]
  , [OldGold .. Parliaments]
  ]

group :: Item -> [Item]
group x = fromMaybe (error "Bad Item") $ find (elem x) groups

toGroups :: Slot -> [[Item]]
toGroups = groupBy ((==) `on` group)

type Slot = [Item]
type Fact = [Slot]

data Solution =
  Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  } deriving (Eq, Show)

solve :: Solution
solve = fromMaybe (error "No solution") $ do
  fact <- listToMaybe $ go $ replicate 5 $ concat groups
  waterDrinker <- find (elem Water) fact ^? _Just % _head % to fromEnum % to toEnum
  zebraOwner <- find (elem Zebra) fact ^? _Just % _head % to fromEnum % to toEnum
  pure Solution {..}


go :: Fact -> [Fact]
go fact
  | isSolved fact' = pure fact'
  | otherwise = do
      guard $ isPossible fact'
      (i, xs) <-
        sortOn (length . snd)
        $ itoListOf (itraversed % to toGroups % traversed % filtered ((1 <) . length)) fact'
      x <- xs
      go $ fact' & ix i %~ (\\ delete x (group x))
  where
    fact' :: Fact
    fact' = fix (\rec x ->
                   bool (const x) rec =<< (x /=) $ step x
                ) fact

isSolved :: Fact -> Bool
isSolved fact = length (concat groups) == length (concat fact)

isPossible :: Fact -> Bool
isPossible fact =
  all (not . null) (intersect <$> groups <*> fact)
  && null (concat groups \\ concat fact)

step :: Fact -> Fact
step fact = foldr ($) fact clues


clues :: [Fact -> Fact]
clues =
  [ meet E Red
  , meet S Dog
  , meet Coffee Green
  , meet U Tea
  , align Ivory Green
  , meet OldGold Snails
  , meet Kools Yellow
  , sweep . over (ix 2) (place Milk)
  , sweep . over (ix 0) (place N)
  , adject Chesterfields Fox
  , adject Kools Horse
  , meet LuckyStrike OrangeJuice
  , meet J Parliaments
  , adject N Blue
  ]


place :: Item -> Slot -> Slot
place x = (\\ delete x (group x))

meet :: Item -> Item -> Fact -> Fact
meet x y = unify [0] x y

adject :: Item -> Item -> Fact -> Fact
adject x y = unify [-1, 1] x y

align :: Item -> Item -> Fact -> Fact
align x y = unify [1] x y

unify :: [Int] -> Item -> Item -> Fact -> Fact
unify ds x y =
  sweep
  . neg (negate <$> ds) y x
  . neg ds x y
  . pos (negate <$> ds) y x
  . pos ds x y

pos :: [Int] -> Item -> Item -> Fact -> Fact
pos ds x y fact = foldr f fact $ fst <$> itoListOf (itraversed % exactly x) fact
  where
    f :: Int -> Fact -> Fact
    f i fact' = fromMaybe fact' $ do
      let ixs = catMaybes $ (\j -> j <$ fact' ^? ix j % including y) <$> (i +) <$> ds
      case ixs of
        [i'] -> pure $ fact' & ix i' %~ place y
        _    -> Nothing

neg :: [Int] -> Item -> Item -> Fact -> Fact
neg ds x y fact = foldr f fact [0 .. length fact - 1]
  where
    f :: Int -> Fact -> Fact
    f i fact' = fromMaybe fact' $ do
      let ixs = catMaybes $ (\j -> j <$ fact' ^? ix j % including y) <$> (i +) <$> ds
      guard $ null ixs
      pure $ fact' & ix i %~ delete x

sweep :: Fact -> Fact
sweep fact = foldr f fact groups
  where
    f :: [Item] -> Fact -> Fact
    f xs fact' =
      fact'
      & traversed % multiple xs
      %~ (\\ (intersect xs $ fact ^.. traversed % single xs % traversed))


-- * Prisms for Slot and Item

exactly :: Item -> Prism' Slot Slot
exactly x = filtering $ (== pure x) . intersect (group x)

including :: Item -> Prism' Slot Slot
including x = filtering $ elem x

single :: Eq a => [a] -> Prism' [a] [a]
single xs = filtering $ (1 ==) . length . intersect xs

multiple :: Eq a => [a] -> Prism' [a] [a]
multiple xs = filtering $ (1 <) . length . intersect xs

filtering :: (a -> Bool) -> Prism' a a
filtering p = prism id (bool Left Right =<< p)


-- * Debug Functions
--
-- import Debug.Trace
--
-- format :: Fact -> String
-- format fact = unlines $ unwords . fmap show . groupBy ((==) `on` group) <$> fact
--
-- traceFactId :: Fact -> Fact
-- traceFactId = uncurry trace . (format &&& id)
--
-- step' :: Fact -> Fact
-- step' = traceFactId . step
