{-# LANGUAGE LambdaCase #-}
module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (chr, ord)
import System.Random (newStdGen, randomRs)


caesarDecode :: String -> String -> String
caesarDecode = \case
  [k] -> fmap (unshiftBy k)
  key -> zipWith unshiftBy (key <> repeat 'a')

caesarEncode :: String -> String -> String
caesarEncode = \case
  [k] -> fmap (shiftBy k)
  key -> zipWith shiftBy (key <> repeat 'a')

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  gen <- newStdGen
  let key = take (length text) $ randomRs ('a', 'z') gen
  pure (key, caesarEncode key text)


shiftBy :: Char -> Char -> Char
shiftBy k = shift (ord k - ord 'a')

unshiftBy :: Char -> Char -> Char
unshiftBy k = shift (- ord k + ord 'a')

shift :: Int -> Char -> Char
shift n c = chr $ (ord c - ord 'a' + n + 26) `mod` 26 + ord 'a'
