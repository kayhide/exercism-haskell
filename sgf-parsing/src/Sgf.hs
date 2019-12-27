{-# LANGUAGE OverloadedStrings #-}
module Sgf (parseSgf) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Tree (Tree (..))
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, many, parseMaybe, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, spaceChar, string, upperChar)


type SgfTree = Tree SgfNode
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe SgfTree
parseSgf = parseMaybe $ paren treeParser


type Parser = Parsec Void Text

treeParser :: Parser SgfTree
treeParser = do
  root <- rootParser
  children <- try (pure <$> treeParser) <|> forestParser
  pure $ Node root children

forestParser :: Parser [SgfTree]
forestParser = many $ paren treeParser

rootParser :: Parser SgfNode
rootParser = do
  void $ string ";"
  pairs <- many $ (,) <$> key <*> some (bracket value)
  pure $ Map.fromList pairs

between :: Parser b -> Parser c -> Parser a -> Parser a
between pre post p = pre *> p <* post

paren :: Parser a -> Parser a
paren = between (char '(') (char ')')

bracket :: Parser a -> Parser a
bracket = between (char '[') (char ']')

key :: Parser Text
key = pack <$> some upperChar

value :: Parser Text
value = fmap mconcat $ some $
  try (spaceChar *> pure " ")
  <|> try (char '\\' *> spaceChar *> pure "")
  <|> try (char '\\' *> anySingle <&> Text.singleton)
  <|> (alphaNumChar <&> Text.singleton)
