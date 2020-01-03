{-# LANGUAGE RecordWildCards #-}
module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad (guard)
import Control.Monad.Fix (fix)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (find, groupBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
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
  deriving (Eq, Show, Ord, Enum, Bounded)

groups :: [Set Item]
groups =
  Set.fromList <$>
  [ [E .. J]
  , [Red .. Blue]
  , [Dog .. Zebra]
  , [Coffee .. Water]
  , [OldGold .. Parliaments]
  ]

group :: Item -> Set Item
group x = fromMaybe (error "Bad Item") $ find (Set.member x) groups

toGroups :: Slot -> [Set Item]
toGroups = fmap Set.fromAscList . groupBy ((==) `on` group) . Set.elems

type Slot = Set Item
type Fact = [Slot]

data Solution =
  Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  } deriving (Eq, Show)

solve :: Solution
solve = fromMaybe (error "No solution") $ do
  fact <- listToMaybe $ go $ replicate 5 $ Set.unions groups
  waterDrinker <- find (elem Water) fact ^? _Just % to Set.elems % _head % to (toEnum . fromEnum)
  zebraOwner <- find (elem Zebra) fact ^? _Just % to Set.elems % _head % to (toEnum . fromEnum)
  pure Solution {..}


go :: Fact -> [Fact]
go fact
  | isSolved fact' = pure fact'
  | otherwise = do
      guard $ isPossible fact'
      (i, xs) <-
        sortOn (length . snd)
        $ itoListOf (itraversed % to toGroups % traversed % filtered ((1 <) . length)) fact'
      x <- Set.elems xs
      go $ fact' & ix i %~ place x
  where
    fact' :: Fact
    fact' = fix (\rec x ->
                   bool (const x) rec =<< (x /=) $ step x
                ) fact

isSolved :: Fact -> Bool
isSolved fact = sum (length <$> groups) == sum (length <$> fact)

isPossible :: Fact -> Bool
isPossible fact =
  all (not . null) (Set.intersection <$> groups <*> fact)
  && null (Set.unions groups \\ Set.unions fact)

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
place x = (\\ Set.delete x (group x))

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
      pure $ fact' & ix i %~ Set.delete x

sweep :: Fact -> Fact
sweep fact = foldr f fact groups
  where
    f :: Slot -> Fact -> Fact
    f xs fact' =
      fact'
      & traversed % multiple xs
      %~ (\\ (Set.intersection xs $ Set.unions $ fact ^.. traversed % single xs))


-- * Prisms for Slot and Item

exactly :: Item -> Prism' Slot Slot
exactly x = filtering $ (== Set.singleton x) . Set.intersection (group x)

including :: Item -> Prism' Slot Slot
including x = filtering $ elem x

single :: Set Item -> Prism' Slot Slot
single xs = filtering $ (1 ==) . length . Set.intersection xs

multiple :: Set Item -> Prism' Slot Slot
multiple xs = filtering $ (1 <) . length . Set.intersection xs

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
