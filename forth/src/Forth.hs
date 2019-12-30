{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , empty
  ) where

import Control.Monad (foldM, when, (>=>))
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState =
  ForthState
  { stack :: [Int]
  , defs  :: Map Text Fn
  }

empty :: ForthState
empty = ForthState [] fns

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state@ForthState {..} = case text of
  (Text.stripPrefix ":" >=> Text.stripSuffix ";" -> Just text') -> defineCustom text' state
  _ -> foldM evalToken state $ Text.words text

toList :: ForthState -> [Int]
toList ForthState {..} = reverse stack


data Fn
  = Fn1 (Int -> Either ForthError [Int])
  | Fn2 (Int -> Int -> Either ForthError [Int])
  | Custom [Text]

fns :: Map Text Fn
fns =
  Map.fromList
  [ ("+", Fn2 $ \x y -> pure . pure $ x + y)
  , ("-", Fn2 $ \x y -> pure . pure $ x - y)
  , ("*", Fn2 $ \x y -> pure . pure $ x * y)
  , ("/", Fn2 $ \x y -> case y of
        0 -> Left DivisionByZero
        _ -> pure . pure $ x `div` y
      )
  , ("dup", Fn1 $ \x -> pure [x, x])
  , ("drop", Fn1 $ \_ -> pure [])
  , ("swap", Fn2 $ \x y -> pure [y, x])
  , ("over", Fn2 $ \x y -> pure [x, y, x])
  ]

defineCustom :: Text -> ForthState -> Either ForthError ForthState
defineCustom text state@ForthState {..} = case Text.words text of
  (Text.toLower -> name) : texts -> do
    when (Text.all isDigit name) $ Left InvalidWord
    pure $ state { defs = Map.insert name (Custom $ expand <$> texts) defs }
  _ -> Left InvalidWord
  where
    expand :: Text -> Text
    expand t = case Map.lookup t defs of
      Just (Custom xs) -> Text.unwords $ expand <$> xs
      _                -> t

evalToken :: ForthState -> Text -> Either ForthError ForthState
evalToken state@ForthState {..} token
  | Text.all isDigit token = pure $ state { stack = read (Text.unpack token) : stack }
  | otherwise = case Map.lookup (Text.toLower token) defs of
      Just f -> case (f, stack) of
        (Fn1 f', x : xs') -> (\res -> state { stack = reverse res <> xs' }) <$> f' x
        (Fn2 f', y : x : xs') -> (\res -> state { stack = reverse res <> xs' }) <$> f' x y
        (Custom xs, _) -> foldM evalToken state xs
        _       -> Left StackUnderflow
      _ -> Left $ UnknownWord token
