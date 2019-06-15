module Bob (responseFor) where

import Data.Bits

responseFor :: String -> String
responseFor xs = case bits (chars xs) of
  [0, 1, _, _] -> "Whoa, chill out!"
  [_, _, _, 1] -> "Sure."
  [0, 0, 0, 0] -> "Fine. Be that way!"
  otherwise -> "Whatever."

bits :: Int -> [Int]
bits i = map (`mod` 2) $ take 4 $ iterate (`div` 2) i

chars :: String -> Int
chars [] = 0
chars (' ':xs) = chars xs
chars (x:' ':xs) = chars (x:xs)
chars "?" = shift 1 3
chars (x:xs)
  | x `elem` ['a'..'z'] = shift 1 0 .|. chars xs
  | x `elem` ['A'..'Z'] = shift 1 1 .|. chars xs
  | x `elem` ['1'..'9'] = shift 1 2 .|. chars xs
  | otherwise = chars xs
