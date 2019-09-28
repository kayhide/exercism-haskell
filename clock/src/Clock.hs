{-# LANGUAGE RecordWildCards #-}
module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

data Clock = Clock { hour :: Int, minute :: Int }
  deriving (Eq, Show)


fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock hour' minute'
  where
    hour' :: Int
    hour' = (hour + minute `div` 60) `mod` 24

    minute' :: Int
    minute' = minute `mod` 60

toString :: Clock -> String
toString Clock {..} = printf "%02d:%02d" hour minute

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour' minute' Clock {..} =
  fromHourMin (hour' + hour) (minute' + minute)
