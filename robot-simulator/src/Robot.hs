{-# LANGUAGE LambdaCase #-}
module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot { bearing :: Bearing, coordinates :: (Integer, Integer) }


mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move orig@(Robot bearing' coordinates') = \case
  [] -> orig
  'R' : xs -> move (mkRobot (turnRight bearing') coordinates') xs
  'L' : xs -> move (mkRobot (turnLeft bearing') coordinates') xs
  'A' : xs -> move (mkRobot bearing' (advance coordinates' bearing')) xs
  _ -> error "Bad instruction"


turnRight :: Bearing -> Bearing
turnRight = toEnum . (`mod` 4) . (+ 1) . fromEnum

turnLeft :: Bearing -> Bearing
turnLeft = toEnum . (`mod` 4) . (+ 3). fromEnum

advance :: (Integer, Integer) -> Bearing -> (Integer, Integer)
advance (x, y) = \case
    North -> (x, y + 1)
    East  -> (x + 1, y)
    South -> (x, y - 1)
    West  -> (x - 1, y)
