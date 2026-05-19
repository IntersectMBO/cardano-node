module Cardano.ReCon.Common.Parser (parseIdentifier, parseIntValue, parseNatValue, parseBinRel) where

import           Cardano.ReCon.Common.Types (BinRel (..), IntValue, NatValue, Parser, VariableIdentifier)

import           Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char (char)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

isSubscriptDigit :: Char -> Bool
isSubscriptDigit c = c >= '₀' && c <= '₉'

parseIdentifier :: Parser VariableIdentifier
parseIdentifier =
  Text.pack <$> ((:) <$> firstChar <*> many nextChar) <?> "identifier"
  where
    firstChar = satisfy (\c -> isAlpha c || c == '_')
    nextChar  = satisfy (\c -> isAlphaNum c || c == '_' || isSubscriptDigit c)

parseIntValue :: Parser IntValue
parseIntValue = signed (pure ()) decimal

parseNatValue :: Parser NatValue
parseNatValue = decimal

parseBinRel :: Parser BinRel
parseBinRel =
      Lte <$ char '≤'
  <|> Gte <$ char '≥'
  <|> Eq  <$ char '='
  <|> Lt  <$ char '<'
  <|> Gt  <$ char '>'
