{-# LANGUAGE OverloadedStrings #-}

module Data.Query.Parser(expr) where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text (Parser, decimal, space, string)
import           Data.Common.Parser (identifier)
import           Data.Query.Lang

number :: Parser Expr
number = Number <$> decimal

variable :: Parser Expr
variable = Variable <$> identifier

function :: Parser Function
function = Add  <$ string "add"
       <|> Mul  <$ string "mul"
       <|> Lte  <$ string "lte"
       <|> And  <$ string "and"
       <|> Min  <$ string "min"
       <|> Mean <$ string "mean"

application :: Parser Expr
application = Application
          <$> function
          <*  skipMany space
          <*  string "("
          <*  skipMany space
          <*> sepBy expr (skipMany space <* string "," <* skipMany space)
          <*  skipMany space
          <*  string ")"

expr :: Parser Expr
expr = number <|> variable <|> application
