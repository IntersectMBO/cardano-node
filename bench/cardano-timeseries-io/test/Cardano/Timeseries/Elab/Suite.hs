module Cardano.Timeseries.Elab.Suite (elabTests) where

import           Cardano.Timeseries.Elab             (elab, initialSt)
import           Cardano.Timeseries.Elab.Expr.Parser (expr)

import           Control.Monad.Except                (runExceptT)
import           Control.Monad.State.Strict          (evalState)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec                     (eof, errorBundlePretty, parse)
import           Text.Megaparsec.Char                (space)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

runWith :: Set Text -> Text -> Either Text ()
runWith ms src = case parse (expr <* space <* eof) "input" src of
  Left e  -> Left (Text.pack (errorBundlePretty e))
  Right s -> case evalState (runExceptT (elab s)) (initialSt ms) of
    Left err -> Left err
    Right _  -> Right ()

run :: Text -> Either Text ()
run = runWith Set.empty

ok :: Text -> Assertion
ok src = case run src of
  Left err -> assertFailure $ "Expected success, got error: " <> Text.unpack err
  Right () -> pure ()

okWith :: Set Text -> Text -> Assertion
okWith ms src = case runWith ms src of
  Left err -> assertFailure $ "Expected success, got error: " <> Text.unpack err
  Right () -> pure ()

bad :: Text -> Assertion
bad src = case run src of
  Left _  -> pure ()
  Right _ -> assertFailure $ "Expected failure for: " <> show src

badWith :: Set Text -> Text -> Assertion
badWith ms src = case runWith ms src of
  Left _  -> pure ()
  Right _ -> assertFailure $ "Expected failure for: " <> show src

errorContains :: Set Text -> Text -> Text -> Assertion
errorContains ms src fragment = case runWith ms src of
  Left err ->
    assertBool
      ("Error should mention '" <> Text.unpack fragment <> "', got: " <> Text.unpack err)
      (fragment `Text.isInfixOf` err)
  Right () -> assertFailure $ "Expected failure for: " <> show src

errorMisses :: Set Text -> Text -> Text -> Assertion
errorMisses ms src fragment = case runWith ms src of
  Left err ->
    assertBool
      ("Error should NOT mention '" <> Text.unpack fragment <> "', got: " <> Text.unpack err)
      (not (fragment `Text.isInfixOf` err))
  Right () -> assertFailure $ "Expected failure for: " <> show src

-- Single metric "m" available.
m1 :: Set Text
m1 = Set.singleton "m"

-- Two metrics "m1" and "m2" available.
m2 :: Set Text
m2 = Set.fromList ["m1", "m2"]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

elabTests :: TestTree
elabTests = testGroup "Elaboration"

  [ testGroup "Literals"
      [ testCase "positive integer"  $ ok "42"
      , testCase "negative number"   $ ok "(-1)"
      , testCase "float"             $ ok "3.14"
      , testCase "true"              $ ok "true"
      , testCase "false"             $ ok "false"
      , testCase "now"               $ ok "now"
      , testCase "epoch"             $ ok "epoch"
      , testCase "unit"              $ ok "()"
      , testCase "pair of scalars"   $ ok "(1, 2)"
      , testCase "mixed pair"        $ ok "(true, now)"
      , testCase "nested pair"       $ ok "((1, 2), 3)"
      , testCase "triple (right-assoc)" $ ok "(1, 2, 3)"
      ]

  , testGroup "Duration literals"
      [ testCase "milliseconds" $ ok "100ms"
      , testCase "seconds"      $ ok "5s"
      , testCase "minutes"      $ ok "3m"
      , testCase "hours"        $ ok "2h"
      ]

  , testGroup "Arithmetic"
      [ testGroup "Well-typed"
          [ testCase "Scalar + Scalar"               $ ok "1 + 2"
          , testCase "Scalar - Scalar"               $ ok "5 - 3"
          , testCase "Scalar * Scalar"               $ ok "2 * 3"
          , testCase "Scalar / Scalar"               $ ok "10 / 2"
          , testCase "Timestamp + Duration"          $ ok "now + 1s"
          , testCase "Duration + Timestamp"          $ ok "1s + now"
          , testCase "Timestamp - Duration"          $ ok "now - 1h"
          , testCase "Duration + Duration"                    $ ok "1s + 2s"
          , testCase "? + ? : Duration (lambda args)"        $ okWith m1 "\\x -> \\y -> m [now; now : x + y]"
          , testCase "? - ? : Timestamp (step context)"      $ okWith m1 "\\x -> \\y -> m [x - y; x]"
          , testCase "InstantVector Scalar + Scalar" $ okWith m1 "m now + 1"
          , testCase "Scalar + InstantVector Scalar" $ okWith m1 "1 + m now"
          ]
      , testGroup "Ill-typed"
          [ testCase "Scalar + Timestamp"    $ bad "1 + now"
          , testCase "Timestamp + Timestamp" $ bad "now + now"
          , testCase "Bool + Bool"           $ bad "true + true"
          , testCase "Timestamp - Timestamp" $ bad "now - now"
          , testCase "Bool * Scalar"         $ bad "true * 2"
          ]
      ]

  , testGroup "Comparisons"
      [ testGroup "Well-typed"
          [ testCase "Scalar == Scalar"               $ ok "1 == 1"
          , testCase "Scalar != Scalar"               $ ok "1 != 2"
          , testCase "Scalar < Scalar"                $ ok "1 < 2"
          , testCase "Scalar <= Scalar"               $ ok "1 <= 2"
          , testCase "Scalar > Scalar"                $ ok "2 > 1"
          , testCase "Scalar >= Scalar"               $ ok "2 >= 1"
          , testCase "Bool == Bool"                   $ ok "true == false"
          , testCase "Bool != Bool"                   $ ok "true != false"
          , testCase "InstantVector Scalar == Scalar" $ okWith m1 "m now == 1"
          , testCase "Scalar == InstantVector Scalar" $ okWith m1 "1 == m now"
          ]
      , testGroup "Ill-typed"
          [ testCase "Timestamp == Timestamp" $ bad "now == now"
          , testCase "Duration == Duration"   $ bad "1s == 2s"
          ]
      ]

  , testGroup "Boolean"
      [ testGroup "Well-typed"
          [ testCase "true && false"  $ ok "true && false"
          , testCase "true || false"  $ ok "true || false"
          , testCase "! true"         $ ok "! true"
          , testCase "chained &&"     $ ok "true && false && true"
          , testCase "chained ||"     $ ok "true || false || true"
          ]
      , testGroup "Ill-typed"
          [ testCase "Scalar && Bool"    $ bad "1 && true"
          , testCase "Timestamp || Bool" $ bad "now || false"
          , testCase "! Scalar"          $ bad "! 1"
          , testCase "! Timestamp"       $ bad "! now"
          ]
      ]

  , testGroup "Let and Lambda"
      [ testGroup "Well-typed"
          [ testCase "let scalar"          $ ok "let x = 1 in x"
          , testCase "let arithmetic"      $ ok "let x = 1 in x + 1"
          , testCase "let timestamp"       $ ok "let t = now in t"
          , testCase "let bool"            $ ok "let b = true in b"
          , testCase "nested let"          $ ok "let x = 1 in let y = 2 in x + y"
          , testCase "lambda identity"     $ ok "\\x -> x"
          , testCase "lambda Duration arg (+ side)" $ ok "\\x -> now + x"
          , testCase "lambda Duration arg (- side)" $ ok "\\x -> x - 1s"
          , testCase "lambda application"  $ ok "(\\x -> x + 1) 5"
          , testCase "let lambda"          $ ok "let f = \\x -> x + 1 in f 5"
          , testCase "lambda Bool"         $ ok "\\x -> ! x"
          , testCase "higher-order"        $ ok "(\\f -> f 1) (\\x -> x + 1)"
          ]
      , testGroup "Ill-typed"
          [ testCase "type mismatch in let body" $ bad "let x = 1 in x && true"
          , testCase "apply non-function"         $ bad "1 5"
          ]
      ]

  , testGroup "Pairs and projections"
      [ testCase "fst of scalar pair"       $ ok "fst (1, 2)"
      , testCase "snd of scalar pair"       $ ok "snd (1, 2)"
      , testCase "fst of mixed pair"        $ ok "fst (true, now)"
      , testCase "snd of mixed pair"        $ ok "snd (true, now)"
      , testCase "nested fst"               $ ok "fst (fst ((1, 2), 3))"
      , testCase "fst of non-pair fails"    $ bad "fst 1"
      , testCase "snd of non-pair fails"    $ bad "snd true"
      ]

  , testGroup "to_scalar"
      [ testGroup "Well-typed"
          [ testCase "to_scalar Scalar"    $ ok "to_scalar 1"
          , testCase "to_scalar Bool"      $ ok "to_scalar true"
          , testCase "to_scalar Timestamp" $ ok "to_scalar now"
          , testCase "to_scalar Duration"  $ ok "to_scalar 1s"
          ]
      , testGroup "Ill-typed"
          [ testCase "to_scalar Pair"           $ bad      "to_scalar (1, 2)"
          , testCase "to_scalar InstantVector"  $ badWith m1 "to_scalar (m now)"
          ]
      ]

  , testGroup "abs and round"
      [ testCase "abs positive"  $ ok "abs 1"
      , testCase "abs negative"  $ ok "abs (-1)"
      , testCase "round float"   $ ok "round 3.14"
      , testCase "round zero"    $ ok "round 0"
      , testCase "abs Timestamp" $ bad "abs now"
      , testCase "round Bool"    $ bad "round true"
      ]

  , testGroup "metrics builtin"
      [ testCase "metrics" $ ok "metrics"
      ]

  , testGroup "Range vectors"
      [ testGroup "Well-typed"
          [ testCase "metric range"                  $ okWith m1 "m [now - 1h; now]"
          , testCase "metric range with step"        $ okWith m1 "m [now - 1h; now : 1m]"
          , testCase "avg_over_time"                 $ okWith m1 "avg_over_time (m [now - 1h; now])"
          , testCase "sum_over_time"                 $ okWith m1 "sum_over_time (m [now - 1h; now])"
          , testCase "increase"                      $ okWith m1 "increase (m [now - 1h; now])"
          , testCase "rate"                          $ okWith m1 "rate (m [now - 1h; now])"
          , testCase "quantile_over_time"            $ okWith m1 "quantile_over_time 0.95 (m [now - 1h; now])"
          ]
      , testGroup "Ill-typed"
          [ testCase "avg_over_time of instant"  $ badWith m1 "avg_over_time (m now)"
          , testCase "sum_over_time of instant"  $ badWith m1 "sum_over_time (m now)"
          , testCase "increase of instant"       $ badWith m1 "increase (m now)"
          , testCase "rate of instant"           $ badWith m1 "rate (m now)"
          ]
      ]

  , testGroup "Instant vector operations"
      [ testGroup "Well-typed"
          [ testCase "min"           $ okWith m1 "min (m now)"
          , testCase "max"           $ okWith m1 "max (m now)"
          , testCase "avg"           $ okWith m1 "avg (m now)"
          , testCase "filter"        $ okWith m1 "filter (\\x -> x > 0) (m now)"
          , testCase "map"           $ okWith m1 "map (\\x -> x + 1) (m now)"
          , testCase "join"          $ okWith m1 "join (m now) (m now)"
          , testCase "unless"        $ okWith m1 "unless (m now) (m now)"
          , testCase "quantile_by"   $ okWith m1 "quantile_by (\"k\") 0.95 (m now)"
          , testCase "FilterByLabel" $ okWith m1 "(m now) {\"k\" = \"v\"}"
          , testCase "multi-metric join" $ okWith m2 "join (m1 now) (m2 now)"
          ]
      , testGroup "Ill-typed"
          [ testCase "min of Bool"             $ bad "min true"
          , testCase "max of Scalar"           $ bad "max 5"
          , testCase "avg of Timestamp"        $ bad "avg now"
          , testCase "filter of range vector"  $ badWith m1 "filter (\\x -> x > 0) (m [now - 1h; now])"
          ]
      ]

  , testGroup "Metric name resolution"
      [ testCase "known metric succeeds"              $ okWith m1 "m now"
      , testCase "unknown metric against empty set"   $ bad "unknown_metric now"
      , testCase "unknown metric against known set"   $ badWith m1 "unknown_metric now"
      , testCase "error names the unknown identifier" $ errorContains Set.empty "no_metric now" "Undefined name"
      , testCase "Did you mean: close name suggested" $ errorContains m1 "nm now" "Did you mean"
      , testCase "no suggestion for distant name"     $ errorMisses  m1 "completely_different now" "Did you mean"
      ]

  ]
