module Cardano.Timeseries.Elab.Expr.Parser.Suite (parserTests) where

import           Cardano.Timeseries.Elab.Expr        (Expr (..))
import           Cardano.Timeseries.Elab.Expr.Parser (expr)

import           Data.Either                         (isLeft, isRight)
import           Data.Text                           (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec                     (eof, errorBundlePretty, parse)
import           Text.Megaparsec.Char                (space)

-- | Assert that the query string parses successfully.
parses :: Text -> Assertion
parses src =
  assertBool ("Expected parse success for: " <> show src) $
    isRight (parse (expr <* space <* eof) "input" src)

-- | Assert that the query string fails to parse.
failsToParse :: Text -> Assertion
failsToParse src =
  assertBool ("Expected parse failure for: " <> show src) $
    isLeft (parse (expr <* space <* eof) "input" src)

-- | Assert that the query string parses and the AST satisfies the predicate.
parsesTo :: Text -> (Expr -> Bool) -> Assertion
parsesTo src p = case parse (expr <* space <* eof) "input" src of
  Left e  -> assertFailure $ "Unexpected parse error: " <> errorBundlePretty e
  Right e -> assertBool ("AST shape check failed for: " <> show src) (p e)

parserTests :: TestTree
parserTests = testGroup "Parsing"
  [ atomTests
  , durationLiteralTests
  , negationTests
  , rangeExprTests
  , labelFilterTests
  , builtinApplicationTests
  , arithmeticTests
  , comparisonTests
  , booleanTests
  , letLambdaTests
  , userApplicationTests
  , operatorPrecedenceTests
  , errorCaseTests
  ]

-- ---------------------------------------------------------------------------
-- Atoms
-- ---------------------------------------------------------------------------

atomTests :: TestTree
atomTests = testGroup "Atoms"
  [ testCase "now"                 $ parses "now"
  , testCase "epoch"               $ parses "epoch"
  , testCase "true"                $ parses "true"
  , testCase "false"               $ parses "false"
  , testCase "unit ()"             $ parses "()"
  , testCase "pair (a, b)"         $ parses "(1, 2)"
  , testCase "triple (a, b, c)"    $ parses "(1, 2, 3)"
  , testCase "nested pair"         $ parses "((1, 2), 3)"
  , testCase "parenthesised expr"  $ parses "(1 + 2)"
  , testCase "integer literal"     $ parses "42"
  , testCase "negative integer"    $ parses "-42"
  , testCase "float literal"       $ parses "3.14"
  , testCase "negative float"      $ parses "-3.14"
  , testCase "string literal"      $ parses "\"hello\""
  , testCase "empty string"        $ parses "\"\""
  , testCase "unescaped identifier" $ parses "my_metric"
  , testCase "identifier with digits" $ parses "metric_0"
  , testCase "escaped identifier"  $ parses "`foo bar`"
  , testCase "escaped keyword via backtick" $ parses "`let`"
  , testCase "escaped metric keyword" $ parses "`metrics`"
  ]

-- ---------------------------------------------------------------------------
-- Duration literals
-- ---------------------------------------------------------------------------

durationLiteralTests :: TestTree
durationLiteralTests = testGroup "Duration literals"
  [ testCase "milliseconds"             $ parses "100ms"
  , testCase "zero milliseconds"        $ parses "0ms"
  , testCase "seconds"                  $ parses "5s"
  , testCase "minutes"                  $ parses "3m"
  , testCase "hours"                    $ parses "2h"
  -- suffix must not bleed into the next identifier character
  , testCase "seconds not prefix of longer id" $ parses "1s + 2"
  ]

-- ---------------------------------------------------------------------------
-- Negation
-- ---------------------------------------------------------------------------

negationTests :: TestTree
negationTests = testGroup "Negation"
  [ testCase "!true"  $ parses "! true"
  , testCase "!false" $ parses "! false"
  , testCase "!var"   $ parses "! x"
  ]

-- ---------------------------------------------------------------------------
-- Range expressions
-- ---------------------------------------------------------------------------

rangeExprTests :: TestTree
rangeExprTests = testGroup "Range expressions"
  [ testCase "basic range [a; b]"          $ parses "x[epoch; now]"
  , testCase "range with sampling rate"    $ parses "x[epoch; now : 1s]"
  , testCase "arithmetic inside bounds"    $ parses "x[now - 1h; now]"
  , testCase "range on complex head"       $ parses "(avg x)[epoch; now]"
  , testCase "chained ranges"              $ parses "x[epoch; now][epoch; now]"
  , testCase "range with full exprs"       $ parses "x[now - 30m; now : 1s]"
  ]

-- ---------------------------------------------------------------------------
-- Label filter expressions
-- ---------------------------------------------------------------------------

labelFilterTests :: TestTree
labelFilterTests = testGroup "Label filter expressions"
  [ testCase "single eq constraint"        $ parses "x{\"a\" = \"1\"}"
  , testCase "single neq constraint"       $ parses "x{\"a\" != \"1\"}"
  , testCase "two constraints"             $ parses "x{\"a\" = \"1\", \"b\" != \"2\"}"
  , testCase "empty constraint set"        $ parses "x{}"
  , testCase "filter applied to range"     $ parses "x[epoch; now]{\"a\" = \"1\"}"
  , testCase "range applied to filter"     $ parses "x{\"a\" = \"1\"}[epoch; now]"
  ]

-- ---------------------------------------------------------------------------
-- Builtin function applications
-- ---------------------------------------------------------------------------

builtinApplicationTests :: TestTree
builtinApplicationTests = testGroup "Builtin applications"
  [ testCase "fst"               $ parses "fst x"
  , testCase "snd"               $ parses "snd x"
  , testCase "min"               $ parses "min x"
  , testCase "max"               $ parses "max x"
  , testCase "avg"               $ parses "avg x"
  , testCase "abs"               $ parses "abs x"
  , testCase "round"             $ parses "round x"
  , testCase "increase"          $ parses "increase x"
  , testCase "rate"              $ parses "rate x"
  , testCase "avg_over_time"     $ parses "avg_over_time x"
  , testCase "sum_over_time"     $ parses "sum_over_time x"
  , testCase "to_scalar"         $ parses "to_scalar x"
  , testCase "metrics"           $ parses "metrics"
  , testCase "filter"            $ parses "filter f x"
  , testCase "join"              $ parses "join x y"
  , testCase "map"               $ parses "map f x"
  , testCase "unless"            $ parses "unless x y"
  , testCase "quantile_over_time" $ parses "quantile_over_time 0.5 x"
  , testCase "quantile_by empty label set"  $ parses "quantile_by () 0.5 x"
  , testCase "quantile_by one label"        $ parses "quantile_by (\"a\") 0.5 x"
  , testCase "quantile_by two labels"       $ parses "quantile_by (\"a\", \"b\") 0.5 x"
  , testCase "earliest"          $ parses "earliest my_metric"
  , testCase "latest"            $ parses "latest my_metric"
  -- builtins compose naturally
  , testCase "avg (rate x)"      $ parses "avg (rate x)"
  , testCase "map f (filter g x)" $ parses "map f (filter g x)"
  ]

-- ---------------------------------------------------------------------------
-- Arithmetic operators
-- ---------------------------------------------------------------------------

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic operators"
  [ testCase "addition"       $ parses "1 + 2"
  , testCase "subtraction"    $ parses "1 - 2"
  , testCase "multiplication" $ parses "2 * 3"
  , testCase "division"       $ parses "4 / 2"
  , testCase "chained add"    $ parses "1 + 2 + 3"
  , testCase "chained sub"    $ parses "5 - 2 - 1"
  , testCase "chained mul"    $ parses "2 * 3 * 4"
  , testCase "chained div"    $ parses "8 / 2 / 2"
  , testCase "mixed add/mul"  $ parses "1 + 2 * 3 - 4 / 2"
  ]

-- ---------------------------------------------------------------------------
-- Comparison operators
-- ---------------------------------------------------------------------------

comparisonTests :: TestTree
comparisonTests = testGroup "Comparison operators"
  [ testCase "equal (==)"        $ parses "1 == 2"
  , testCase "not equal (!=)"    $ parses "1 != 2"
  , testCase "less than (<)"     $ parses "1 < 2"
  , testCase "less or equal (<=)" $ parses "1 <= 2"
  , testCase "greater than (>)"  $ parses "1 > 2"
  , testCase "greater or eq (>=)" $ parses "1 >= 2"
  ]

-- ---------------------------------------------------------------------------
-- Boolean operators
-- ---------------------------------------------------------------------------

booleanTests :: TestTree
booleanTests = testGroup "Boolean operators"
  [ testCase "and (&&)"          $ parses "true && false"
  , testCase "or (||)"           $ parses "true || false"
  , testCase "chained and"       $ parses "true && false && true"
  , testCase "chained or"        $ parses "true || false || true"
  , testCase "mixed and/or"      $ parses "true && false || true && true"
  ]

-- ---------------------------------------------------------------------------
-- Let and lambda
-- ---------------------------------------------------------------------------

letLambdaTests :: TestTree
letLambdaTests = testGroup "Let and lambda"
  [ testCase "simple let"               $ parses "let x = 1 in x"
  , testCase "nested let"               $ parses "let x = 1 in let y = 2 in x + y"
  , testCase "let rhs is arithmetic"    $ parses "let x = 2 * 3 in x + 1"
  , testCase "let with lambda rhs"      $ parses "let f = \\x -> x + 1 in f 2"
  , testCase "let with let rhs"         $ parses "let x = let y = 1 in y in x"
  , testCase "lambda identity"          $ parses "\\x -> x"
  , testCase "lambda with body"         $ parses "\\x -> x + 1"
  , testCase "nested lambda"            $ parses "\\x -> \\y -> x + y"
  , testCase "lambda applied"           $ parses "(\\x -> x + 1) 2"
  ]

-- ---------------------------------------------------------------------------
-- User-defined function application
-- ---------------------------------------------------------------------------

userApplicationTests :: TestTree
userApplicationTests = testGroup "User-defined function application"
  [ testCase "single argument"       $ parses "f x"
  , testCase "two arguments"         $ parses "f x y"
  , testCase "applied to literal"    $ parses "f 42"
  , testCase "applied to expression" $ parses "f (x + 1)"
  , testCase "higher-order"          $ parses "let f = \\x -> x in f x"
  ]

-- ---------------------------------------------------------------------------
-- Operator precedence (verified via AST shape)
-- ---------------------------------------------------------------------------

operatorPrecedenceTests :: TestTree
operatorPrecedenceTests = testGroup "Operator precedence"
  [ testCase "mul binds tighter than add (rhs)" $
      parsesTo "1 + 2 * 3" $ \case
        Add _ _ (Mul _ _ _) -> True
        _                   -> False

  , testCase "mul binds tighter than add (lhs)" $
      parsesTo "1 * 2 + 3" $ \case
        Add _ (Mul _ _ _) _ -> True
        _                   -> False

  , testCase "add is left-associative" $
      parsesTo "1 + 2 + 3" $ \case
        Add _ (Add _ _ _) _ -> True
        _                   -> False

  , testCase "mul is left-associative" $
      parsesTo "2 * 3 * 4" $ \case
        Mul _ (Mul _ _ _) _ -> True
        _                   -> False

  , testCase "sub is left-associative" $
      parsesTo "5 - 2 - 1" $ \case
        Sub _ (Sub _ _ _) _ -> True
        _                   -> False

  , testCase "and binds tighter than or" $
      parsesTo "true && false || true" $ \case
        Or _ (And _ _ _) _ -> True
        _                  -> False

  , testCase "comp binds tighter than and" $
      parsesTo "1 < 2 && 3 > 4" $ \case
        And _ (Lt _ _ _) (Gt _ _ _) -> True
        _                            -> False

  , testCase "add binds tighter than comp" $
      parsesTo "1 + 2 < 3 + 4" $ \case
        Lt _ (Add _ _ _) (Add _ _ _) -> True
        _                             -> False

  , testCase "parentheses override mul/add" $
      parsesTo "(1 + 2) * 3" $ \case
        Mul _ (Add _ _ _) _ -> True
        _                   -> False

  , testCase "let body extends as far as possible" $
      parsesTo "let x = 1 in x + 2" $ \case
        Let _ _ _ (Add _ _ _) -> True
        _                     -> False

  , testCase "lambda body extends as far as possible" $
      parsesTo "\\x -> x + 1" $ \case
        Lambda _ _ (Add _ _ _) -> True
        _                      -> False
  ]

-- ---------------------------------------------------------------------------
-- Error cases
-- ---------------------------------------------------------------------------

errorCaseTests :: TestTree
errorCaseTests = testGroup "Error cases"
  [ -- syntactic errors
    testCase "empty input"              $ failsToParse ""
  , testCase "trailing operator"        $ failsToParse "1 +"
  , testCase "unbalanced paren"         $ failsToParse "(1 + 2"
  , testCase "unmatched closing paren"  $ failsToParse "1)"
  , testCase "bare let keyword"         $ failsToParse "let"
  , testCase "let without body"         $ failsToParse "let x = 1"
  , testCase "lambda without arrow"     $ failsToParse "\\x"
  , testCase "keyword as identifier"    $ failsToParse "in"
  , testCase "unknown operator"         $ failsToParse "1 @ 2"
  -- wrong argument count for builtins
  , testCase "fst: missing arg"         $ failsToParse "fst"
  , testCase "snd: missing arg"         $ failsToParse "snd"
  , testCase "min: missing arg"         $ failsToParse "min"
  , testCase "max: missing arg"         $ failsToParse "max"
  , testCase "avg: missing arg"         $ failsToParse "avg"
  , testCase "abs: missing arg"         $ failsToParse "abs"
  , testCase "increase: missing arg"    $ failsToParse "increase"
  , testCase "rate: missing arg"        $ failsToParse "rate"
  , testCase "avg_over_time: missing arg" $ failsToParse "avg_over_time"
  , testCase "sum_over_time: missing arg" $ failsToParse "sum_over_time"
  , testCase "to_scalar: missing arg"   $ failsToParse "to_scalar"
  , testCase "filter: one arg (needs 2)" $ failsToParse "filter f"
  , testCase "join: one arg (needs 2)"   $ failsToParse "join x"
  , testCase "map: one arg (needs 2)"    $ failsToParse "map f"
  , testCase "unless: one arg (needs 2)" $ failsToParse "unless x"
  , testCase "quantile_over_time: one arg (needs 2)" $ failsToParse "quantile_over_time 0.5"
  ]
