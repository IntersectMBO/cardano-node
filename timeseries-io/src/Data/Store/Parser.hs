{-# LANGUAGE OverloadedStrings #-}

module Data.Store.Parser(point, points) where

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text (Parser, decimal, endOfLine, space, string)
import           Data.Common.Parser (identifier)
import           Data.Store.Point (Point (..))
import           Data.Word (Word64)

-- | x, [x = x, ..., x = x], n, n
point :: Parser a -> Parser (Point a)
point value = Point
      <$> metric
      <*  comma
      <*> inBrackets labels
      <*  comma
      <*> timestamp
      <*  comma
      <*> value

      where

  comma :: Parser ()
  comma = skipMany space <* string "," <* skipMany space

  equals :: Parser ()
  equals = skipMany space <* string "=" <* skipMany space

  inBrackets :: forall a. Parser a -> Parser a
  inBrackets f = string "[" *> skipMany space *> f <* skipMany space <* string "]"

  metric :: Parser String
  metric = identifier

  labels :: Parser [(String, String)]
  labels = sepBy' ((,) <$> identifier <* equals <*> identifier) (skipMany space <* string "," <* skipMany space)

  timestamp :: Parser Word64
  timestamp = decimal

-- | point
-- | ...
-- | point
points :: Parser a -> Parser [Point a]
points value = sepBy' (point value) endOfLine
