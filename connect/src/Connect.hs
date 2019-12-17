module Connect (Mark(..), winner) where

import Control.Monad (guard, when)
import Control.Monad.State (State, execState, gets)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Optics (contains, filtered, itoListOf, itraversed, ix, to, traversed,
               (%), (<%>), (^.), (^..), (^?), _1)
import Optics.State.Operators ((.=))


data Mark = Cross | Nought deriving (Eq, Show)


winner :: [String] -> Maybe Mark
winner board = listToMaybe $ do
  connection@(_, mark) <- connections board
  guard $ isReached board connection
  pure mark

type Board = [String]
type Coord = (Int, Int)
type Connection = (Set Coord, Mark)

connections :: Board -> [Connection]
connections board = foldl' f [] coords
  where
    coords :: [Coord]
    coords = fst <$> itoListOf (itraversed <%> itraversed % filtered (`elem` "XO")) board

    f :: [Connection] -> Coord -> [Connection]
    f acc (y, x) = (acc <>) $ maybeToList $ do
      guard $ and $ acc ^.. traversed % _1 % contains (y, x) % to not
      c <- board ^? ix y % ix x
      case c of
        'X' -> pure (connectionFor board (y, x), Cross)
        'O' -> pure (connectionFor board (y, x), Nought)
        _   -> Nothing

connectionFor :: Board -> Coord -> Set Coord
connectionFor board coord@(y, x) =
  case board ^? ix y % ix x % filtered (`elem` "XO") of
    Nothing -> Set.empty
    Just c  -> execState (follow c coord) Set.empty

  where
    follow :: Char -> Coord -> State (Set Coord) ()
    follow c coord'@(y', x') = do
      done <- gets (^. contains coord')
      when (not done && pure c == board ^? ix y' % ix x') $ do
        contains coord' .= True
        traverse_ (follow c) $ adjacentsOf coord'

adjacentsOf :: Coord -> Set Coord
adjacentsOf (y, x) = Set.fromList
  [ (y - 1, x - 1)
  , (y - 1, x + 1)
  , (y, x - 2)
  , (y, x + 2)
  , (y + 1, x - 1)
  , (y + 1, x + 1)
  ]

data Edged = EdgedFirst | EdgedLast
  deriving (Eq, Show)

isReached :: Board -> Connection -> Bool
isReached board (coords, mark) =
  (&&) <$> elem EdgedFirst <*> elem EdgedLast $ do
  let edgedFirst = case mark of
        Cross  -> \(y, x) -> Nothing == board ^? ix y % ix (x - 2) % filtered (/= ' ')
        Nought -> \(y, _) -> y == 0
  let edgedLast = case mark of
        Cross  -> \(y, x) -> Nothing == board ^? ix y % ix (x + 2) % filtered (/= ' ')
        Nought -> \(y, _) -> y == length board - 1

  coord <- Set.toList coords
  bool [] [EdgedFirst] (edgedFirst coord) <> bool [] [EdgedLast] (edgedLast coord)
