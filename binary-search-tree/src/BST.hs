module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)

data BST a = BST
  { bstValue :: Maybe a
  , bstLeft  :: Maybe (BST a)
  , bstRight :: Maybe (BST a)
  }
  deriving (Eq, Show)


empty :: BST a
empty = BST Nothing Nothing Nothing

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x (BST v' l' r') = case compare x <$> v' of
  Nothing -> singleton x
  Just GT -> BST v' l' (pure $ maybe (singleton x) (insert x) r')
  Just _  -> BST v' (pure $ maybe (singleton x) (insert x) l') r'

singleton :: a -> BST a
singleton x = BST (pure x) Nothing Nothing

toList :: BST a -> [a]
toList (BST v' l' r') =
  fromMaybe [] $ (toList <$> l') <> (pure <$> v') <> (toList <$> r')

