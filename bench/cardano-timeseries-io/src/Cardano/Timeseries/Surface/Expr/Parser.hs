{- HLINT ignore "Use <$>" -}
module Cardano.Timeseries.Surface.Expr.Parser(Parser, expr) where

import           Cardano.Timeseries.Domain.Identifier (Identifier (User))
import           Cardano.Timeseries.Domain.Types (Label)
import           Cardano.Timeseries.Query.Expr (LabelConstraint (..))
import           Cardano.Timeseries.Surface.Expr
import           Cardano.Timeseries.Surface.Expr.Head (Head)
import qualified Cardano.Timeseries.Surface.Expr.Head as Head

import           Prelude hiding (Foldable (..), head)

import           Control.Applicative hiding (many, some)
import           Control.Monad (guard)
import           Data.Char (isAlpha, isAlphaNum)
import           Data.Foldable (Foldable (..))
import           Data.Functor (void)
import           Data.Scientific (toRealFloat)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import           GHC.Unicode (isControl)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, space, space1, string)
import           Text.Megaparsec.Char.Lexer (decimal, scientific, signed)

type Parser = Parsec Void Text

keywords :: [Text]
keywords = ["let", "in", "true", "false", "epoch", "now", "fst", "snd",
            "min", "max", "avg", "filter", "join", "map", "abs", "increase",
            "rate", "avg_over_time", "sum_over_time", "quantile_over_time", "unless",
            "quantile_by", "earliest", "latest", "to_scalar", "metrics"]

unescapedVariableIdentifierNextChar :: Parser Char
unescapedVariableIdentifierNextChar = satisfy (\x -> isAlphaNum x || x == '_')

unescapedVariableIdentifier :: Parser Text
unescapedVariableIdentifier =
  Text.pack <$> ((:) <$> firstChar <*> many unescapedVariableIdentifierNextChar) <?> "identifier" where
  firstChar :: Parser Char
  firstChar = satisfy (\x -> isAlpha x || x == '_')

number :: Parser Expr
number =
  Number <$> getSourcePos <*> (toRealFloat <$> signed (pure ()) scientific <?> "number")

escapedVariableIdentifier :: Parser Text
escapedVariableIdentifier = Text.pack <$> (char '`' *> manyTill one (char '`')) where
  one :: Parser Char
  one = satisfy (\x -> not (isControl x) && (x /= '`') && (x /= '\n') && (x /= '\r'))

literalVariableIdentifier :: Parser Text
literalVariableIdentifier = do
  x <- unescapedVariableIdentifier
  guard (x `notElem` keywords)
  pure x

variableIdentifier :: Parser Identifier
variableIdentifier = User <$> (literalVariableIdentifier <|> escapedVariableIdentifier)

variable :: Parser Expr
variable = Variable <$> getSourcePos <*> variableIdentifier

text :: Parser Text
text = Text.pack <$> (char '\"' *> many one) <* char '\"' where
  one :: Parser Char
  one = satisfy (\x -> not (isControl x) && (x /= '"') && (x /= '\n') && (x /= '\r'))

str :: Parser Expr
str = Str <$> getSourcePos <*> text

milliseconds :: Parser Expr
milliseconds = Milliseconds <$> getSourcePos <*> decimal <* string "ms" <* notFollowedBy (satisfy isAlpha)

seconds :: Parser Expr
seconds = Seconds <$> getSourcePos <*> decimal <* string "s" <* notFollowedBy (satisfy isAlpha)

minutes :: Parser Expr
minutes = Minutes <$> getSourcePos <*> decimal <* string "m" <* notFollowedBy (satisfy isAlpha)

hours :: Parser Expr
hours = Hours <$> getSourcePos <*> decimal <* string "h" <* notFollowedBy (satisfy isAlpha)

now :: Parser Expr
now = Now <$> getSourcePos <* string "now"

epoch :: Parser Expr
epoch = Now <$> getSourcePos <* string "epoch"

true :: Parser Expr
true = Truth <$> getSourcePos <* string "true"

false :: Parser Expr
false = Falsity <$> getSourcePos <* string "false"

continueTight :: Expr -> Parser Expr
continueTight a = a <$ string ")"

continuePair :: Loc -> Expr -> Parser Expr
continuePair l a = do
  void $ string ","
  space
  b <- exprUniverse
  space
  void $ string ")"
  pure (MkPair l a b)

tightOrPair :: Parser Expr
tightOrPair = do
  l <- getSourcePos
  void $ string "("
  space
  a <- exprUniverse
  space
  try (continuePair l a) <|> continueTight a

exprAtom :: Parser Expr
exprAtom = tightOrPair
       <|> epoch
       <|> true
       <|> false
       <|> now
       <|> variable
       <|> try milliseconds
       <|> try seconds
       <|> try minutes
       <|> try hours
       <|> number
       <|> str

exprNot :: Parser Expr
exprNot = Not <$> getSourcePos <*> (string "!" *> space *> exprAtom)

range :: Parser (Expr, Expr, Maybe Expr)
range = do
  void $ string "["
  space
  a <- exprUniverse
  space
  void $ string ";"
  space
  b <- exprUniverse
  c <- optional $ do
    space
    void $ string ":"
    space
    c <- exprUniverse
    space
    pure c
  void $ string "]"
  pure (a, b, c)

exprRange :: Parser Expr
exprRange = do
  l <- getSourcePos
  head <- exprAtom
  ext <- many (try $ space *> (try (Left <$> range) <|> Right <$> labelConstraints))
  pure $ foldl' (mkRange l) head ext

labelConstraint :: Parser LabelConstraint
labelConstraint = do
  x <- text
  space
  c <- Left () <$ string "=" <|> Right () <$ string "!="
  space
  v <- text
  pure (mk x c v) where
    mk x (Left _) v  = LabelConstraintEq (x, v)
    mk x (Right _) v = LabelConstraintNotEq (x, v)

labelConstraints :: Parser (Set LabelConstraint)
labelConstraints = do
  void $ string "{"
  space
  list <- sepBy labelConstraint (space *> string "," <* space)
  space
  void $ string "}"
  pure $ Set.fromList list

setLabel :: Parser (Set Label)
setLabel = do
  void $ string "("
  space
  list <- sepBy text (space *> string "," <* space)
  space
  void $ string ")"
  pure $ Set.fromList list

headParse :: Parser Head
headParse = (
        Head.Fst <$ string "fst"
    <|> Head.Snd <$ string "snd"
    <|> Head.Min <$ string "min"
    <|> Head.Max <$ string "max"
    <|> Head.Filter <$ string "filter"
    <|> Head.Join <$ string "join"
    <|> Head.Map <$ string "map"
    <|> Head.Abs <$ string "abs"
    <|> Head.Round <$ string "round"
    <|> Head.Increase <$ string "increase"
    <|> Head.Rate <$ string "rate"
    <|> Head.AvgOverTime <$ string "avg_over_time"
    <|> Head.Avg <$ string "avg"
    <|> Head.SumOverTime <$ string "sum_over_time"
    <|> Head.QuantileOverTime <$ string "quantile_over_time"
    <|> Head.Unless <$ string "unless"
    <|> Head.QuantileBy <$> (string "quantile_by" *> space1 *> setLabel)
    <|> Head.Earliest <$> (string "earliest" *> space1 *> variableIdentifier)
    <|> Head.Latest <$> (string "latest" *> space1 *> variableIdentifier)
    <|> Head.ToScalar <$ string "to_scalar"
    <|> Head.Metrics <$ string "metrics"
       ) <* notFollowedBy unescapedVariableIdentifierNextChar

wrongNumberOfArguments :: Int -> String -> Parser a
wrongNumberOfArguments n head = fail $ "Wrong number of arguments (" <> show n <> ") for `" <> head <> "`"

applyBuiltin :: Loc -> Head -> [Expr] -> Parser Expr
applyBuiltin l Head.Fst [t] = pure $ Fst l t
applyBuiltin l Head.Snd [t] = pure $ Snd l t
applyBuiltin l Head.Min [t] = pure $ Min l t
applyBuiltin l Head.Max [t] = pure $ Max l t
applyBuiltin l Head.Avg [t] = pure $ Avg l t
applyBuiltin l Head.Filter [f, xs] = pure $ Filter l f xs
applyBuiltin l Head.Join [xs, ys] = pure $ Join l xs ys
applyBuiltin l Head.Map [f, xs] = pure $ Map l f xs
applyBuiltin l Head.Abs [t] = pure $ Abs l t
applyBuiltin l Head.Round [t] = pure $ Round l t
applyBuiltin l Head.Increase [xs] = pure $ Increase l xs
applyBuiltin l Head.Rate [xs] = pure $ Rate l xs
applyBuiltin l Head.AvgOverTime [xs] = pure $ AvgOverTime l xs
applyBuiltin l Head.SumOverTime [xs] = pure $ SumOverTime l xs
applyBuiltin l Head.QuantileOverTime [k, xs] = pure $ QuantileOverTime l k xs
applyBuiltin l Head.Unless [xs, ys] = pure $ Unless l xs ys
applyBuiltin l (Head.QuantileBy ls) [k, xs] = pure $ QuantileBy l ls k xs
applyBuiltin l (Head.Earliest x) [] = pure $ Earliest l x
applyBuiltin l (Head.Latest x) [] = pure $ Latest l x
applyBuiltin l Head.ToScalar [t] = pure $ ToScalar l t
applyBuiltin l Head.Metrics [] = pure $ Metrics l
applyBuiltin _ h args = wrongNumberOfArguments (length args) (show h)

apply :: Loc -> Either Head Expr -> [Expr] -> Parser Expr
apply l (Left t)  = applyBuiltin l t
apply l (Right t) = pure . mkApp l t

exprAppArg :: Parser Expr
exprAppArg = try exprNot <|> exprRange

exprApp :: Parser Expr
exprApp = do
  l <- getSourcePos
  h <- try (Left <$> headParse) <|> Right <$> exprAppArg
  args <- many (try (space1 *> exprAppArg))
  apply l h args

data Mul = Asterisk | Slash

mul :: Parser Mul
mul = Asterisk <$ try (string "*") <|> Slash <$ string "/"

applyMul :: Loc -> Expr -> (Mul, Expr) -> Expr
applyMul l x (Asterisk, y) = Mul l x y
applyMul l x (Slash, y)    = Div l x y

applyListMul :: Loc -> Expr -> [(Mul, Expr)] -> Expr
applyListMul l = foldl' (applyMul l)

exprMul :: Parser Expr
exprMul = do
  l <- getSourcePos
  h <- exprApp
  args <- many ((,) <$> try (space *> mul) <*> (space *> exprApp))
  pure $ applyListMul l h args

data Add = Plus | Minus deriving (Show)

add :: Parser Add
add = Plus <$ try (string "+") <|> Minus <$ string "-"

applyAdd :: Loc -> Expr -> (Add, Expr) -> Expr
applyAdd l x (Plus, y)  = Add l x y
applyAdd l x (Minus, y) = Sub l x y

applyListAdd :: Loc -> Expr -> [(Add, Expr)] -> Expr
applyListAdd l = foldl' (applyAdd l)

exprAdd :: Parser Expr
exprAdd = do
  l <- getSourcePos
  h <- exprMul
  args <- many ((,) <$> try (space *> add) <*> (space *> exprMul))
  pure $ applyListAdd l h args

data Comp = EqSign | NotEqSign | LtSign | LteSign | GtSign | GteSign

comp :: Parser Comp
comp = EqSign <$ string "=="
   <|> NotEqSign <$ string "!="
   <|> LteSign <$ string "<="
   <|> GteSign <$ string ">="
   <|> LtSign <$ string "<"
   <|> GtSign <$ string ">"

applyComp :: Loc -> Expr -> (Comp, Expr) -> Expr
applyComp l a (EqSign, b)    = Eq l a b
applyComp l a (NotEqSign, b) = NotEq l a b
applyComp l a (LtSign, b)    = Lt l a b
applyComp l a (LteSign, b)   = Lte l a b
applyComp l a (GtSign, b)    = Gt l a b
applyComp l a (GteSign, b)   = Gte l a b

applyListComp :: Loc -> Expr -> [(Comp, Expr)] -> Expr
applyListComp l = foldl' (applyComp l)

exprComp :: Parser Expr
exprComp = do
  l <- getSourcePos
  h <- exprAdd
  args <- many ((,) <$> try (space *> comp) <*> (space *> exprAdd))
  pure $ applyListComp l h args

applyAnd :: Loc -> Expr -> [Expr] -> Expr
applyAnd l = foldl' (And l)

exprAnd :: Parser Expr
exprAnd = do
  l <- getSourcePos
  h <- exprComp
  args <- many (try (space *> string "&&") *> space *> exprComp)
  pure $ applyAnd l h args

applyOr :: Loc -> Expr -> [Expr] -> Expr
applyOr l = foldl' (Or l)

exprOr :: Parser Expr
exprOr = do
  l <- getSourcePos
  h <- exprAnd
  args <- many (try (space *> string "||") *> space *> exprAnd)
  pure $ applyOr l h args

exprLet :: Parser Expr
exprLet = do
  l <- getSourcePos
  void $ string "let"
  space1
  x <- variableIdentifier
  space
  void $ string "="
  space
  rhs <- exprOr
  space
  void $ string "in"
  space1
  body <- exprUniverse
  pure $ Let l x rhs body

exprLambda :: Parser Expr
exprLambda = do
  l <- getSourcePos
  void $ string "\\"
  space
  x <- variableIdentifier
  space
  void $ string "->"
  space
  body <- exprUniverse
  pure $ Lambda l x body

exprUniverse :: Parser Expr
exprUniverse = try exprLet <|> (try exprLambda <|> exprOr)

expr :: Parser Expr
expr = exprUniverse
