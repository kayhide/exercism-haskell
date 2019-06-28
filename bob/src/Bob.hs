{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Bob (responseFor) where

import           Data.Bool (bool)
import           Data.Char (isDigit, isLower, isSpace, isUpper)
import           Data.Text (Text)
import qualified Data.Text as T

-- * Strategy
-- We have possibilities for each of `Questioning`, `Exclaming` and `Silencing`.
-- Possibilities are tested on every `Char` and accumulated.
--
-- And we compose these 3 possibilities into `Condition` which is also
-- accumulated over every char.
--
-- The rules of accumulation is defined via `Semigroup` instance so that
-- the filnal `Condition` can be achieved by `foldMap`.
--
-- After consuming all incoming chars, the accumulated `Condition` can be
-- used to decide how to respond.


-- * Data Types

data Possibility = Unknown | Possible | Impossible
  deriving (Eq, Show)

type Questioning = Possibility
type Exclaming = Possibility
type Silencing = Possibility

data Condition = Condition Questioning Exclaming Silencing
  deriving (Eq, Show)


instance Semigroup Possibility where
  -- | `Unknown` is just igrnored.
  Unknown <> y = y
  x <> Unknown = x

  -- | `Impossible` beats `Possible`.
  Impossible <> _ = Impossible
  _ <> Impossible = Impossible

  -- | Otherwise, both operands must be `Possible`
  Possible <> Possible = Possible


instance Semigroup Condition where
  -- | Drops the possibility of questioning.
  -- This is the case where some meaningful char comes after a question char.
  Condition Possible xe _ <> Condition _ ye Impossible =
    Condition Unknown (xe <> ye) Impossible

  -- | Otherwise, every possibility followes individual instance.
  Condition xq xe xs <> Condition yq ye ys =
    Condition (xq <> yq) (xe <> ye) (xs <> ys)

instance Monoid Condition where
  -- | `mempty` is the initial state of `Condition`.
  -- Only `Silencing` is possible before evaluating anything.
  mempty = Condition Unknown Unknown Possible


-- | Test if a `Char` makes some conditions possible or impossible.
-- For an irreravant char, sets `Unkown`.
toCondition :: Char -> Condition
toCondition '?'               = Condition Possible Unknown Impossible
toCondition (isLower -> True) = Condition Unknown Impossible Impossible
toCondition (isUpper -> True) = Condition Unknown Possible Impossible
toCondition (isDigit -> True) = Condition Unknown Unknown Impossible
toCondition _                 = mempty


-- | Evaluates chars, accumulates possibilities and pick a suitable response.
responseFor :: Text -> Text
responseFor xs = case foldMap toCondition (T.unpack xs) of
  Condition Possible Possible _        -> "Calm down, I know what I'm doing!"
  Condition Possible _        _        -> "Sure."
  Condition _        Possible _        -> "Whoa, chill out!"
  Condition _        _        Possible -> "Fine. Be that way!"
  _                                    -> "Whatever."
