module Pangram (isPangram) where

import           Data.Char  (toLower)
import           Data.List  (find, scanl')
import           Data.Maybe (isJust)
import           Data.Set   (Set)
import qualified Data.Set   as Set


-- | Alphabets you are looking for.
-- The following is set to cover all alphabet chars.
-- If you want only vowals rather than all alphabets, set it as "aeiou".
targets :: [Char]
targets = ['a' .. 'z']


-- | Fold leftward to reduce target chars while consuming chars from given
-- sentence.
-- If targets get empty, it terminates immediately.
isPangram :: String -> Bool
isPangram = isJust . find null . scanl' go (Set.fromList targets)
  where
    go :: Set Char -> Char -> Set Char
    go chars c = Set.delete (toLower c) chars
