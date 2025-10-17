module Data.Query.Lang(Function(..), Expr(..)) where

-- average(add($metric1, $metric2), min(5))

{- n ::= <integer>
 - x ::= <identifier>
 - c ::= add | mul | lte | and | min
 - e ::= n | $x | c ( e, ..., e )
 -}

data Function = Add | Mul | Lte | And | Min | Mean deriving Show

data Expr = Number Integer | Variable String | Application Function [Expr] deriving Show
