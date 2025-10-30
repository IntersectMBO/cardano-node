{-# LANGUAGE OverloadedStrings #-}

module Data.Common.Parser(identifier) where

import           Data.Attoparsec.ByteString.Char8 (isDigit)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text (Parser, satisfy, string)
import           Data.Char (isAlpha)

identifier :: Parser String
identifier = string "$" *> ((:) <$> firstChar <*> many' nextChar) where
  firstChar :: Parser Char
  firstChar = satisfy (\x -> isAlpha x || x == '_')

  nextChar :: Parser Char
  nextChar = satisfy (\x -> isAlpha x || x == '_' || isDigit x)
