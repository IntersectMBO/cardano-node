{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Cardano.Timeseries.Query.Parser(expr) where

import           Cardano.Timeseries.Domain.Identifier (Identifier (User))
import           Cardano.Timeseries.Query.Expr

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Attoparsec.ByteString.Char8 (isDigit)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text (Parser, double, satisfy, space, string)
import           Data.Char (isAlpha)
import           Data.Functor (void)
import           Data.List.NonEmpty (fromList)
import           GHC.Unicode (isControl)

keywords :: [String]
keywords = ["let", "in"]

variableIdentifier :: Parser String
variableIdentifier = (:) <$> firstChar <*> many' nextChar where
  firstChar :: Parser Char
  firstChar = satisfy (\x -> isAlpha x || x == '_')

  nextChar :: Parser Char
  nextChar = satisfy (\x -> isAlpha x || x == '_' || isDigit x)

number :: Parser Expr
number = Number <$> double

escapedVariable :: Parser String
escapedVariable = string "`" *> many' char <* string "`" where
  char :: Parser Char
  char = satisfy (\x -> not (isControl x) && (x /= '`') && (x /= '\n') && (x /= '\r'))

literalVariable :: Parser String
literalVariable = do
  x <- variableIdentifier
  guard (x `notElem` keywords)
  pure x

variable :: Parser Expr
variable = Variable . User <$> (literalVariable <|> escapedVariable)

str :: Parser Expr
str = Str <$> (string "\"" *> many' char <* string "\"") where
  char :: Parser Char
  char = satisfy (\x -> not (isControl x) && (x /= '"') && (x /= '\n') && (x /= '\r'))

function :: Parser Function
function =
           AddDuration              <$ string "add_duration"
       <|> Minutes                  <$ string "minutes"
       <|> Milliseconds             <$ string "milliseconds"
       <|> Seconds                  <$ string "seconds"
       <|> Hours                    <$ string "hours"
       <|> AvgOverTime              <$ string "avg_over_time"
       <|> Avg                      <$ string "avg"
       <|> SumOverTime              <$ string "sum_over_time"
       <|> Max                      <$ string "max"
       <|> Min                      <$ string "min"
       <|> Fst                      <$ string "fst"
       <|> Snd                      <$ string "snd"
       <|> EqBool                   <$ string "eq_bool"
       <|> EqScalar                 <$ string "eq_scalar"
       <|> NotEqScalar              <$ string "not_eq_scalar"
       <|> LtScalar                 <$ string "lt_scalar"
       <|> LteScalar                <$ string "lte_scalar"
       <|> GtScalar                 <$ string "gt_scalar"
       <|> GteScalar                <$ string "gte_scalar"
       <|> AddScalar                <$ string "add_scalar"
       <|> MulScalar                <$ string "mul_scalar"
       <|> SubScalar                <$ string "sub_scalar"
       <|> DivScalar                <$ string "div_scalar"
       <|> Abs                      <$ string "abs"
       <|> QuantileOverTime         <$ string "quantile_over_time"
       <|> Quantile                 <$ string "quantile"
       <|> Rate                     <$ string "rate"
       <|> Increase                 <$ string "increase"
       <|> Now                      <$ string "now"
       <|> BoolToScalar             <$ string "bool_to_scalar"
       <|> InstantVectorToScalar    <$ string "instant_vector_to_scalar"
       <|> TimestampToScalar        <$ string "timestamp_to_scalar"
       <|> DurationToScalar         <$ string "duration_to_scalar"
       <|> Range                    <$ string "range"
       <|> FilterByLabel            <$ string "filter_by_label"
       <|> Filter                   <$ string "filter"
       <|> Join                     <$ string "join"
       <|> Map                      <$ string "map"
       <|> Epoch                    <$ string "epoch"
       <|> FastForward              <$ string "fast_forward"
       <|> AddInstantVectorScalar   <$ string "add_instant_vector_scalar"
       <|> MulInstantVectorScalar   <$ string "mul_instant_vector_scalar"
       <|> And                      <$ string "and"
       <|> Or                       <$ string "or"
       <|> EqInstantVectorScalar    <$ string "eq_instant_vector_scalar"
       <|> NotEqInstantVectorScalar <$ string "not_eq_instant_vector_scalar"
       <|> LtInstantVectorScalar    <$ string "lt_instant_vector_scalar"
       <|> LteInstantVectorScalar   <$ string "lte_instant_vector_scalar"
       <|> GtInstantVectorScalar    <$ string "gt_instant_vector_scalar"
       <|> GteInstantVectorScalar   <$ string "gte_instant_vector_scalar"

builtin :: Parser Expr
builtin = Builtin <$> function

application :: Parser Expr
application = do
  f <- expr1
  args <- many1 (many1 space *> expr1)
  pure $ Application f (fromList args)

lambda :: Parser Expr
lambda = do
  void $ string "\\"
  skipMany space
  x <- variableIdentifier
  skipMany space
  void $ string "->"
  skipMany space
  body <- expr
  pure $ Lambda (User x) body

let_ :: Parser Expr
let_ = do
  void $ string "let"
  skipMany space
  x <- variableIdentifier
  skipMany space
  void $ string "="
  skipMany space
  rhs <- expr
  skipMany space
  void $ string "in"
  skipMany space
  body <- expr
  pure $ Let (User x) rhs body

continueTight :: Expr -> Parser Expr
continueTight a = a <$ string ")"

continuePair :: Expr -> Parser Expr
continuePair a = do
  void $ string ","
  skipMany space
  b <- expr
  skipMany space
  void $ string ")"
  pure (MkPair a b)

tightOrPair :: Parser Expr
tightOrPair = do
  void $ string "("
  skipMany space
  a <- expr
  skipMany space
  continueTight a <|> continuePair a

expr1 :: Parser Expr
expr1 = number <|> builtin <|> variable <|> str <|> tightOrPair

expr :: Parser Expr
expr = let_ <|> lambda <|> application <|> number <|> variable <|> str <|> tightOrPair
