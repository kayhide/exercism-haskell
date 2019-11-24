module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Arrow (first)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, modify, put)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find, unfoldr)
import Data.Set (Set, notMember)
import qualified Data.Set as Set
import Data.Stream.Infinite (Stream (..))
import qualified Data.Stream.Infinite as Infinite
import System.Random (RandomGen, newStdGen, randomR)


type Robot = IORef String
type RunState = Set String


initialState :: RunState
initialState = Set.empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
  names <- get
  gen <- liftIO newStdGen
  let (name :> _ ) = Infinite.filter (`notMember` names) $ randomNames gen
  put $ Set.insert name names
  liftIO $ newIORef name

resetName :: Robot -> StateT RunState IO ()
resetName robot = do
  robot' <- mkRobot
  modify . Set.delete =<< liftIO (readIORef robot)
  liftIO $ writeIORef robot =<< robotName robot'

robotName :: Robot -> IO String
robotName = readIORef



randomName :: RandomGen g => g -> (String, g)
randomName gen = foldr f ("", gen) ranges
    where
      ranges :: [(Char, Char)]
      ranges = replicate 2 ('A', 'Z') <> replicate 3  ('0', '9')

      f :: RandomGen g => (Char, Char) -> (String, g) -> (String, g)
      f r (xs, g) = first (: xs) $ randomR r g

randomNames :: RandomGen g => g -> Stream String
randomNames = Infinite.unfold randomName
