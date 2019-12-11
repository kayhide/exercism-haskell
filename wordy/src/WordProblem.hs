{-# LANGUAGE OverloadedStrings #-}
module WordProblem (answer) where

import Control.Arrow ((>>>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, space1, string)


answer :: String -> Maybe Integer
answer = parseMaybe questionParser . pack


type Parser = Parsec Void Text

questionParser :: Parser Integer
questionParser = do
  x <- string "What is" *> space1 *> termParser
  fnc <- (try fncParser <|> pure id) <* char '?'
  pure $ fnc x

fncParser :: Parser (Integer -> Integer)
fncParser = do
  op <- space1 *> opParser <* space1
  y <- termParser
  fnc <- (try fncParser <|> pure id)
  pure $ (`op` y) >>> fnc

opParser :: Parser (Integer -> Integer -> Integer)
opParser =
      try ((+) <$ string "plus")
  <|> try ((-) <$ string "minus")
  <|> try ((*) <$ string "multiplied by")
  <|>     (div <$ string "divided by")

litParser :: Parser Integer
litParser = do
  neg <- try (negate <$ string "-") <|> pure id
  neg . read <$> some digitChar

termParser :: Parser Integer
termParser = do
  x <- litParser
  pow <- (try powerParser <|> pure id)
  pure $ pow x

powerParser :: Parser (Integer -> Integer)
powerParser = do
  p <- space1 *> string "raised to the " *> litParser <* "th power"
  pure $ (^ p)
