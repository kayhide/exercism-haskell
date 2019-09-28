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

data LinkedList a = Nil | Cons a (LinkedList a)
  deriving (Eq, Show, Foldable)

datum :: LinkedList a -> a
datum = \case
  Nil -> error "No element"
  Cons x _ -> x

fromList :: [a] -> LinkedList a
fromList = \case
  [] -> Nil
  x : xs -> Cons x $ fromList xs

isNil :: LinkedList a -> Bool
isNil = \case
  Nil -> True
  Cons _ _ -> False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next = \case
  Nil -> Nil
  Cons _ xs -> xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip Cons) Nil

toList :: LinkedList a -> [a]
toList = foldr (:) []
