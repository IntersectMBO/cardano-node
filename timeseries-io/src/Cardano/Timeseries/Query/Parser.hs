{-# LANGUAGE OverloadedStrings #-}

module Cardano.Timeseries.Query.Parser(expr) where

import           Cardano.Timeseries.Query.Expr

import           Control.Applicative ((<|>))
import           Data.Attoparsec.ByteString.Char8 (isDigit)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text (Parser, decimal, satisfy, space, string)
import           Data.Char (isAlpha)

identifier :: Parser String
identifier = string "$" *> ((:) <$> firstChar <*> many' nextChar) where
  firstChar :: Parser Char
  firstChar = satisfy (\x -> isAlpha x || x == '_')

  nextChar :: Parser Char
  nextChar = satisfy (\x -> isAlpha x || x == '_' || isDigit x)

number :: Parser Expr
number = Number <$> decimal

variable :: Parser Expr
variable = Variable <$> identifier

str :: Parser Expr
str = Str <$> (string "\"" *> many' char <* string "\"") where
  char :: Parser Char
  char = satisfy (\x -> isAlpha x || x == '_' || x == ' ' || isDigit x)

function :: Parser Function
function = Add               <$ string "add"
       <|> Mul               <$ string "mul"
       <|> Lte               <$ string "lte"
       <|> And               <$ string "and"
       <|> Or                <$ string "or"
       <|> Mins              <$ string "mins"
       <|> Millis            <$ string "millis"
       <|> Seconds           <$ string "seconds"
       <|> Hours             <$ string "hours"
       <|> Avg               <$ string "avg"
       <|> AvgOverTime       <$ string "avg_over_time"
       <|> SumOverTime       <$ string "sum_over_time"
       <|> Max               <$ string "max"
       <|> Min               <$ string "min"
       <|> Fst               <$ string "fst"
       <|> Snd               <$ string "snd"
       <|> MkPair            <$ string "mk_pair"
       <|> EqBool            <$ string "eq_bool"
       <|> EqScalar          <$ string "eq_scalar"
       <|> LtScalar          <$ string "lt_scalar"
       <|> LteScalar         <$ string "lte_scalar"
       <|> GtScalar          <$ string "gt_scalar"
       <|> GteScalar         <$ string "gte_scalar"
       <|> AddScalar         <$ string "add_scalar"
       <|> MulScalar         <$ string "mul_scalar"
       <|> SubScalar         <$ string "sub_scalar"
       <|> DivScalar         <$ string "div_scalar"
       <|> Abs               <$ string "abs"
       <|> Quantile          <$ string "quantile"
       <|> QuantileOverTime  <$ string "quantile_over_time"
       <|> Rate              <$ string "rate"
       <|> Increase          <$ string "increase"
       <|> Now               <$ string "now"
       <|> BoolToScalar      <$ string "bool_to_scalar"
       <|> TimestampToScalar <$ string "timestamp_to_scalar"
       <|> DurationToScalar  <$ string "duration_to_scalar"
       <|> Range             <$ string "range"
       <|> RangeWithRate     <$ string "range_with_rate"
       <|> FilterByLabel     <$ string "filter_by_label"
       <|> Filter            <$ string "filter"
       <|> Join              <$ string "join"
       <|> Map               <$ string "map"

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
expr = number <|> variable <|> application <|> str
