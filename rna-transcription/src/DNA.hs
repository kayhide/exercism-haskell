{-# LANGUAGE LambdaCase #-}
module DNA (toRNA) where

import Control.Applicative

toRNA :: String -> Either Char String
toRNA = foldr (liftA2 (:)) (Right "") . fmap turn'

turn' :: Char -> Either Char Char
turn' = \case
  'C' -> Right 'G'
  'G' -> Right 'C'
  'T' -> Right 'A'
  'A' -> Right 'U'
  x  -> Left x
