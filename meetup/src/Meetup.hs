{-# LANGUAGE ViewPatterns #-}
module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, addDays, fromGregorian, toGregorian)
import Data.Time.Calendar.Easter (sundayAfter)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving (Eq, Show, Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
  First  -> head $ candidates
  Second -> head . drop 1 $ candidates
  Third  -> head . drop 2 $ candidates
  Fourth -> head . drop 3 $ candidates
  Last   -> last $ candidates
  Teenth -> head $ filter ((`elem` [13 .. 19]) . dayOfMonth) candidates
  where
    candidates :: [Day]
    candidates = dates year month weekday


dayOfMonth :: Day -> Int
dayOfMonth (toGregorian -> (_, _, d)) = d

dates :: Integer -> Int -> Weekday -> [Day]
dates year month weekday = takeWhile isOnMonth . dropWhile (not . isOnMonth) $ weekdays
  where
    isOnMonth :: Day -> Bool
    isOnMonth (toGregorian -> (y, m, _)) = y == year && m == month

    firstWeekday :: Day
    firstWeekday =
      addDays (fromIntegral $ fromEnum weekday - fromEnum Sunday)
      $ sundayAfter $ fromGregorian year month 1

    weekdays :: [Day]
    weekdays = iterate (addDays 7) firstWeekday
