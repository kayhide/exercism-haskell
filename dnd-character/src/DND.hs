{-# LANGUAGE RecordWildCards #-}
module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Control.Monad (replicateM)
import Test.QuickCheck (Gen, choose)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier x = (x - 10) `div` 2

ability :: Gen Int
ability = do
  xs <- replicateM 4 $ choose (1, 6)
  pure $ sum xs - minimum xs

character :: Gen Character
character = do
  strength     <- ability
  dexterity    <- ability
  constitution <- ability
  intelligence <- ability
  wisdom       <- ability
  charisma     <- ability
  let hitpoints = 10 + modifier constitution
  pure Character {..}
