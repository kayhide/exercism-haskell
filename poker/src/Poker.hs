{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Poker (bestHands) where

import Control.Applicative ((<*), (<|>))
import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (elemIndex, group, groupBy, partition, sortOn)
import Data.Ord (Down (..))
import Safe (headMay)


bestHands :: [String] -> Maybe [String]
bestHands hands = do
  hands' <- traverse analyze hands
  wins <- headMay $ groupBy ((==) `on` snd) $ sortOn (Down . snd) hands'
  pure $ fst <$> wins


data Hand
  = HighCard [Int]
  | OnePair [Int]
  | TwoPair [Int]
  | ThreeOfAKind [Int]
  | Straight [Int]
  | Flush [Int]
  | FullHouse [Int]
  | FourOfAKind [Int]
  | StraightFlush [Int]
  deriving (Eq, Ord, Show)

type Card = (Int, Char)

toCards :: String -> Maybe [Card]
toCards = traverse toCard . words

toCard :: String -> Maybe Card
toCard = \case
    [x, c] -> (,c) <$> elemIndex x "__23456789_JQKA" <* guard (x /= '_' && isSuit c)
    ['1', '0', c] -> (10, c) <$ guard (isSuit c)
    _ -> Nothing
  where
    isSuit :: Char -> Bool
    isSuit = (`elem` "SHDC")

toHand :: [Card] -> Hand
toHand cards = case sortOn (Down . length) $ group nums of
  xss@([_, _, _, _] : _)       -> FourOfAKind $ concat xss
  xss@([_, _, _] : [_, _] : _) -> FullHouse $ concat xss
  xss@([_, _, _] : _)          -> ThreeOfAKind $ concat xss
  xss@([_, _] : [_, _] : _)    -> TwoPair $ concat xss
  xss@([_, _] : _)             -> OnePair $ concat xss

  _ -> case (isStraight, isFlush) of
    (Just nums', True)  -> StraightFlush nums'
    (Just nums', False) -> Straight nums'
    (Nothing, True)     -> Flush nums
    _                   -> HighCard nums

  where
    nums :: [Int]
    nums = sortOn Down $ fst <$> cards

    isStraight :: Maybe [Int]
    isStraight = f nums <|> (f =<< aceToOne nums)
      where
        f :: [Int] -> Maybe [Int]
        f ns = bool Nothing (Just ns) . all (== 1) . zipWith (-) ns $ tail ns

        aceToOne :: [Int] -> Maybe [Int]
        aceToOne ns = case partition (== 14) ns of
          ([], _)  -> Nothing
          (as, xs) -> Just $ xs <> replicate 1 (length as)

    isFlush :: Bool
    isFlush = (== 1) . length . group $ snd <$> cards

analyze :: String -> Maybe (String, Hand)
analyze = sequence . (id &&& fmap toHand . toCards)
