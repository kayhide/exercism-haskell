module WordCount (wordCount) where

import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort, tails)


wordCount :: String -> [(String, Int)]
wordCount xs = fmap (head &&& length) . group .  sort $ words' xs

words' :: String -> [String]
words' str = words $ fmap f . takeWhile (not . null) $ tails (' ' : str)
  where
    f :: String -> Char
    f (x : '\'' : y : _) = bool ' ' '\'' $ isAlphaNum x && isAlphaNum y
    f (_ : c : _)        = bool ' ' (toLower c) $ isAlphaNum c
    f _                  = ' '
