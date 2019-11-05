module Queens (boardString, canAttack) where

import Data.Function (on)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ do
  y <- [0 .. 7]
  pure . unwords $ do
    x <- [0 .. 7]
    let pos = pure (y, x)
    case (pos == white, pos == black) of
      (True, _) -> pure "W"
      (_, True) -> pure "B"
      _         -> pure "_"


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack a b =
  or $ on (==) <$>
  [ fst                         -- Same row
  , snd                         -- Same column
  , uncurry (+)                 -- Same diagonal /
  , uncurry (-)                 -- Same diagonal \
  ] <*> pure a <*> pure b
