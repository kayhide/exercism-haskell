module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ do
  y <- [0 .. 7]
  pure . unwords $ do
    x <- [0 .. 7]
    let pos = (y, x)
    case (pure pos == white, pure pos == black) of
      (True, _) -> pure "W"
      (_, True) -> pure "B"
      _         -> pure "_"


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (y, x) (y', x') =
  or
  [ y' == y
  , x' == x
  , y' - y == x' - x
  , y' - y == - (x' - x)
  ]
