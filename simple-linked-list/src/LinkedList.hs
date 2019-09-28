{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE LambdaCase #-}
module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

import Data.List (foldl')

data LinkedList a = Nil | Cons { datum :: a, next :: LinkedList a }
  deriving (Eq, Show, Foldable)


fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

isNil :: LinkedList a -> Bool
isNil = \case
  Nil -> True
  Cons _ _ -> False

new :: a -> LinkedList a -> LinkedList a
new = Cons

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl' (flip Cons) Nil

toList :: LinkedList a -> [a]
toList = foldr (:) []
