{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef


data Node a =
  Node
  { value :: a
  , prev :: IORef (Maybe (Node a))
  , next :: IORef (Maybe (Node a))
  }
data Deque a =
  Deque
  { first :: IORef (Maybe (Node a))
  , last :: IORef (Maybe (Node a))
  }


mkDeque :: IO (Deque a)
mkDeque = Deque <$> newIORef Nothing <*> newIORef Nothing

pop :: forall a. Deque a -> IO (Maybe a)
pop (Deque first' last') = traverse f =<< readIORef last'
  where
    f :: Node a -> IO a
    f n = do
      prev' <- readIORef (prev n)
      writeIORef last' prev'
      writeIORef (maybe first' next prev') $ Nothing
      pure $ value n

push :: Deque a -> a -> IO ()
push (Deque first' last') x = do
  last'' <- readIORef last'
  node <- Node x <$> newIORef last'' <*> newIORef Nothing
  writeIORef last' $ Just node
  writeIORef (maybe first' next last'') $ Just node

unshift :: Deque a -> a -> IO ()
unshift (Deque first' last') x = do
  first'' <- readIORef first'
  node <- Node x <$> newIORef Nothing <*> newIORef first''
  writeIORef first' $ Just node
  writeIORef (maybe last' prev first'') $ Just node

shift :: forall a. Deque a -> IO (Maybe a)
shift (Deque first' last') = traverse f =<< readIORef first'
  where
    f :: Node a -> IO a
    f n = do
      next'' <- readIORef $ next n
      writeIORef first' next''
      writeIORef (maybe last' prev next'') Nothing
      pure $ value n
