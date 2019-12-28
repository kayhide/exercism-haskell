{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

import Data.Bool (bool)
import Data.List (init)
import Data.Maybe (fromMaybe)
import Optics (AffineTraversal', An_AffineTraversal, Lens', castOptic, equality,
               lens, view, (%), (&), (.~), (^?), _Just)


data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Zipper a =
  Zipper (BinTree a) Focus
  deriving (Eq, Show)

type Focus = [Bool]

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree = view _Tree

value :: Zipper a -> a
value (Zipper tree f) =
  fromMaybe
  (error "Bad Focus")
  $ tree ^? focusing f % _Value

left :: Zipper a -> Maybe (Zipper a)
left (Zipper tree f) =
  Zipper tree (f <> [False]) <$ tree ^? focusing f % _LeftTree % _Just

right :: Zipper a -> Maybe (Zipper a)
right (Zipper tree f) =
  Zipper tree (f <> [True]) <$ tree ^? focusing f % _RightTree % _Just

up :: Zipper a -> Maybe (Zipper a)
up = \case
  Zipper _ [] -> Nothing
  Zipper tree f -> pure $ Zipper tree $ init f

setValue :: a -> Zipper a -> Zipper a
setValue x zipper@(Zipper _ f) = zipper & _Tree % focusing f % _Value .~ x

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree zipper@(Zipper _ f) = zipper & _Tree % focusing f % _LeftTree .~ tree

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree zipper@(Zipper _ f) = zipper & _Tree % focusing f % _RightTree .~ tree


_Value :: Lens' (BinTree a) a
_Value = lens btValue (\s a -> s { btValue = a })

_LeftTree :: Lens' (BinTree a) (Maybe (BinTree a))
_LeftTree = lens btLeft (\s a -> s { btLeft = a })

_RightTree :: Lens' (BinTree a) (Maybe (BinTree a))
_RightTree = lens btRight (\s a -> s { btRight = a })

_Tree :: Lens' (Zipper a) (BinTree a)
_Tree = lens (\(Zipper tree _) -> tree) (\(Zipper _ f) a -> Zipper a f)

focusing :: [Bool] -> AffineTraversal' (BinTree a) (BinTree a)
focusing bs =
  foldr (%) (castOptic @An_AffineTraversal equality)
  $ bool (_LeftTree % _Just) (_RightTree % _Just) <$> bs
