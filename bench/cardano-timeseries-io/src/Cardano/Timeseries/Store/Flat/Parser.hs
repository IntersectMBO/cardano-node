{-# LANGUAGE OverloadedStrings #-}

module Cardano.Timeseries.Store.Flat.Parser(double, point) where

import           Cardano.Timeseries.Domain.Instant
import           Cardano.Timeseries.Domain.Types (Labelled, MetricIdentifier, Timestamp)
import           Cardano.Timeseries.Store.Flat (Point (..))

import           Data.Char (isControl)
import           Data.Scientific (toRealFloat)
import           Data.Set (fromList)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import           Data.Word (Word64)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, space, space1, string)
import           Text.Megaparsec.Char.Lexer (decimal, scientific, signed)

type Parser = Parsec Void Text

double :: Parser Double
double = toRealFloat <$> signed (pure ()) scientific

-- | s ::= <doublequoted-string>
-- s [s = s, ..., s = s] <decimal-nat> <floating-point>
point :: Show a => Parser a -> Parser (Point a)
point value = makePoint
      <$> text
      <*  space
      <*> inBrackets labels
      <*  space
      <*> timestamp
      <*  space1
      <*> value

      where

  makePoint :: MetricIdentifier -> [Labelled Text] -> Timestamp -> a -> Point a
  makePoint n ls t v = Point n (Instant (fromList ls) t v)

  comma :: Parser ()
  comma = space <* string "," <* space

  equals :: Parser ()
  equals = space <* string "=" <* space

  inBrackets :: Parser a -> Parser a
  inBrackets f = string "[" *> space *> f <* space <* string "]"

  text :: Parser Text
  text = Text.pack <$> (char '\"' *> many one) <* char '\"' where
    one :: Parser Char
    one = satisfy (\x -> not (isControl x) && (x /= '"') && (x /= '\n') && (x /= '\r'))

  labels :: Parser [(Text, Text)]
  labels = sepBy
    ((,) <$> (space *> text) <* equals <*> text)
    comma

  timestamp :: Parser Word64
  timestamp = decimal
