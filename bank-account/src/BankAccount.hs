{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)


type AccountId = Word

newtype BankAccount = BankAccount AccountId
  deriving (Eq, Show, Ord, Enum, Bounded)


type Balance = Integer
type Bank = Map BankAccount Balance


closeAccount :: BankAccount -> IO ()
closeAccount account = atomically $ do
  modifyTVar bank $ Map.delete account

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = atomically $ do
  Map.lookup account <$> readTVar bank

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
  modifyTVar bank $ Map.adjust (+ amount) account
  Map.lookup account <$> readTVar bank

openAccount :: IO BankAccount
openAccount = atomically $ do
  stateTVar bank $ \bank' -> do
    let acc = maybe minBound (succ . fst) $ Map.lookupMax bank'
    (acc, Map.insert acc 0 bank')


bank :: TVar Bank
bank = unsafePerformIO $ newTVarIO Map.empty
