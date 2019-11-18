{-# LANGUAGE LambdaCase #-}
module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

data CustomSet a = Leaf | Tree a (CustomSet a) (CustomSet a)

instance Eq a => Eq (CustomSet a) where
  xs == ys = toList xs == toList ys

instance Show a => Show (CustomSet a) where
  show = \case
    Leaf -> "_"
    Tree v xs ys -> show (v, xs, ys)

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete x = \case
  Leaf -> Leaf
  Tree v xs ys -> case x `compare` v of
    LT -> Tree v (delete x xs) ys
    EQ -> union xs ys
    GT -> Tree v xs (delete x ys)

difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = case (setA, setB) of
  (Leaf, _) -> Leaf
  (_, Leaf) -> setA
  (Tree x xsl xsr, Tree y ysl ysr) -> case x `compare` y of
    LT -> union (difference (Tree x xsl Leaf) ysl) (difference xsr setB)
    EQ -> union (difference xsl ysl) (difference xsr ysr)
    GT -> union (difference xsl setB) (difference (Tree x Leaf xsr) ysr)

empty :: CustomSet a
empty = Leaf

fromList :: (Ord a) => [a] -> CustomSet a
fromList = foldr insert Leaf

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x = \case
  Leaf -> Tree x Leaf Leaf
  tree@(Tree v xs ys) -> case x `compare` v of
    LT -> Tree v (insert x xs) ys
    EQ -> tree
    GT -> Tree v xs (insert x ys)

intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = case (setA, setB) of
  (Leaf, _) -> Leaf
  (_, Leaf) -> Leaf
  (Tree x xsl xsr, Tree y ysl ysr) -> case x `compare` y of
    LT -> union (intersection setA ysl) (intersection xsr (Tree y Leaf ysr))
    EQ -> Tree x (intersection xsl ysl) (intersection xsr ysr)
    GT -> union (intersection xsl (Tree y ysl Leaf)) (intersection setA ysr)

isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = case (setA, setB) of
  (Leaf, _) -> True
  (_, Leaf) -> True
  (Tree x xsl xsr, Tree y ysl ysr) -> case x `compare` y of
    LT -> isDisjointFrom setA ysl && isDisjointFrom xsr ysr
    EQ -> False
    GT -> isDisjointFrom xsl ysl && isDisjointFrom setA ysr

isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = case (setA, setB) of
  (Leaf, _) -> True
  (_, Leaf) -> False
  (Tree x xsl xsr, Tree y ysl ysr) -> case x `compare` y of
    LT -> isSubsetOf setA ysl && isSubsetOf xsr ysr
    EQ -> isSubsetOf xsl ysl && isSubsetOf xsr ysr
    GT -> isSubsetOf xsl ysl && isSubsetOf setA ysr


member :: (Ord a) => a -> CustomSet a -> Bool
member x = \case
  Leaf -> False
  Tree v xs ys -> case x `compare` v of
    LT -> (member x xs)
    EQ -> True
    GT -> (member x ys)

null :: (Ord a) => CustomSet a -> Bool
null = \case
  Leaf -> True
  Tree _ _ _ -> False

size :: CustomSet a -> Int
size = \case
  Leaf -> 0
  Tree _ xs ys -> 1 + size xs + size ys

toList :: CustomSet a -> [a]
toList = \case
  Leaf -> []
  Tree v xs ys -> toList xs <> [v] <> toList ys

union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = foldr insert setA $ toList setB
