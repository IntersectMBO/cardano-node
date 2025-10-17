{-# LANGUAGE OverloadedStrings #-}

module Cardano.Timeseries.Store.Parser(point, points) where

import           Cardano.Timeseries.Domain.Instant
import           Cardano.Timeseries.Domain.Types (Labelled, MetricIdentifier, Timestamp)
import           Cardano.Timeseries.Store.Flat (Point (..))

import           Data.Attoparsec.ByteString.Char8 (isDigit)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text (Parser, decimal, endOfLine, satisfy, space, string)
import           Data.Char (isAlpha)
import           Data.Set (fromList)
import           Data.Word (Word64)

identifier :: Parser String
identifier = (:) <$> firstChar <*> many' nextChar where
  firstChar :: Parser Char
  firstChar = satisfy (\x -> isAlpha x || x == '_')

  nextChar :: Parser Char
  nextChar = satisfy (\x -> isAlpha x || x == '_' || isDigit x)

-- | x [x = x, ..., x = x] n n
point :: Parser a -> Parser (Point a)
point value = makePoint
      <$> metric
      <*  skipMany space
      <*> inBrackets labels
      <*  skipMany space
      <*> timestamp
      <*  skipMany space
      <*> value

      where

  makePoint :: MetricIdentifier -> [Labelled String] -> Timestamp -> a -> Point a
  makePoint n ls t v = Point n (Instant (fromList ls) t v)

  comma :: Parser ()
  comma = skipMany space <* string "," <* skipMany space

  equals :: Parser ()
  equals = skipMany space <* string "=" <* skipMany space

  inDoublequotes :: forall a. Parser a -> Parser a
  inDoublequotes f = string "\"" *> f <* string "\""

  inBrackets :: forall a. Parser a -> Parser a
  inBrackets f = string "[" *> skipMany space *> f <* skipMany space <* string "]"

  metric :: Parser String
  metric = identifier

  labelValue :: Parser String
  labelValue = many' $ satisfy (\x -> isAlpha x || x == '_' || isDigit x)

  labels :: Parser [(String, String)]
  labels = sepBy'
    ((,) <$> (skipMany space *> identifier) <* equals <*> inDoublequotes labelValue)
    comma

  timestamp :: Parser Word64
  timestamp = decimal

-- | point
-- | ...
-- | point
points :: Parser a -> Parser [Point a]
points value = sepBy' (point value) endOfLine
