{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module POV (fromPOV, tracePathBetween) where

import Control.Monad ((<=<))
import Data.Foldable (asum)
import Data.Tree (Tree (..))
import Optics (AffineTraversal', An_AffineTraversal, Lens', castOptic, equality,
               ix, lens, view, (%), (%~), (&), (^?), _1, _Just)


fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = go x tree ^? _Just % _1

go :: Eq a => a -> Tree a -> Maybe (Tree a, AffineTraversal' (Tree a) (Tree a))
go x tree@(Node v forest)
  | x == v = pure (tree, castOptic @An_AffineTraversal equality)
  | otherwise = do
      (tree', path) <- asum $ go x <$> forest
      k <- tree' ^? path % _value
      pure
        ( tree'
          & path % _children %~ ((tree & _children %~ filter ((/= k) . view _value)) :)
        , path % _children % ix 0
        )

tracePathBetween :: forall a. Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to' = path <=< fromPOV from
  where
    path :: Tree a -> Maybe [a]
    path (Node v forest)
      | to' == v = pure [to']
      | otherwise = (v :) <$> (asum $ path <$> forest)


_value :: Lens' (Tree a) a
_value = lens rootLabel $ \s a -> s { rootLabel = a }

_children :: Lens' (Tree a) [Tree a]
_children = lens subForest $ \s a -> s { subForest = a }
