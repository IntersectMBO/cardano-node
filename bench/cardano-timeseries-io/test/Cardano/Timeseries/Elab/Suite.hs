module Cardano.Timeseries.Elab.Suite (elabTests) where

import           Cardano.Timeseries.Elab             (elab, initialSt)
import           Cardano.Timeseries.Elab.Expr.Parser (expr)

import           Control.Monad.Except                (runExceptT)
import           Control.Monad.State.Strict          (evalState)
import           Data.Either                         (isLeft, isRight)
import           Data.Text                           (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec                     (eof, errorBundlePretty, parse)
import           Text.Megaparsec.Char                (space)

elabSucceeds :: Text -> Assertion
elabSucceeds src = case parse (expr <* space <* eof) "input" src of
  Left e  -> assertFailure $ "Unexpected parse error: " <> errorBundlePretty e
  Right s ->
    assertBool ("Expected elaboration to succeed for: " <> show src) $
      isRight $ evalState (runExceptT (elab s)) initialSt

elabFails :: Text -> Assertion
elabFails src = case parse (expr <* space <* eof) "input" src of
  Left e  -> assertFailure $ "Unexpected parse error: " <> errorBundlePretty e
  Right s ->
    assertBool ("Expected elaboration to fail for: " <> show src) $
      isLeft $ evalState (runExceptT (elab s)) initialSt

elabTests :: TestTree
elabTests = testGroup "Elaboration"
  [ testGroup "Well-typed"
      [ testCase "scalar arithmetic" $ elabSucceeds "1 + 2"
      , testCase "boolean literal"   $ elabSucceeds "true"
      , testCase "timestamp: now"    $ elabSucceeds "now"
      , testCase "timestamp: epoch"  $ elabSucceeds "epoch"
      , testCase "let binding"       $ elabSucceeds "let x = 1 in x"
      , testCase "lambda identity"   $ elabSucceeds "\\x -> x"
      ]
  , testGroup "Ill-typed"
      [ testCase "Scalar + Timestamp" $ elabFails "1 + now"
      ]
  ]
