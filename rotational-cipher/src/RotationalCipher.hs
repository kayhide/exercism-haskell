module RotationalCipher (rotate) where

import Data.Bool (bool)
import Data.Char (chr, isAlpha, isUpper, ord)

rotate :: Int -> String -> String
rotate n = fmap (bool id (rotateChar n) =<< isAlpha)


rotateChar :: Int -> Char -> Char
rotateChar n c = chr . (+ a) . (`mod` 26) . (+ n) . subtract a $ ord c
  where
    a :: Int
    a = ord $ bool 'a' 'A' $ isUpper c
