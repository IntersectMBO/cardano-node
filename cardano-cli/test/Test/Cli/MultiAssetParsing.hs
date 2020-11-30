{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.MultiAssetParsing where

import           Cardano.Prelude

import           Cardano.Api.Typed
import           Cardano.CLI.Mary.Parser
import qualified Data.Text as Text
import           Test.Cli.Gen
import           Text.Parsec (ParseError)
import qualified Text.Parsec as Parsec (parse)
import           Text.Parsec.String (Parser)

import           Hedgehog (Gen, Property, checkSequential, discover, evalEither, forAll, property,
                     (===))
import           Hedgehog.Internal.Property (failWith)

{- HLINT ignore "Reduce duplication" -}

-- Lexer
lex :: Parser a -> Text -> Either ParseError a
lex p = Parsec.parse p "" . Text.unpack

-- Parser
parse :: TParser a -> Tokens -> Either ParseError a
parse p = Parsec.parse p ""

-- Lexing

prop_lexLovelace :: Property
prop_lexLovelace =
  property $ do
    (input, expectedOutput) <- forAll genLovelaceToken
    case lex lexToken input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexValue_fullySpecified :: Property
prop_lexValue_fullySpecified =
  property $ do
    (input, expectedOutput) <- forAll genValueTokenFullySpecified
    case lex valueTokenFullySpecified input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput


prop_lexValue_pid_and_asset_id :: Property
prop_lexValue_pid_and_asset_id =
  property $ do
    (input, expectedOutput) <- forAll genValueTokenPidAndAssetId
    case lex valueTokenPolicyIdAndAssetId input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexValue_pid_only :: Property
prop_lexValue_pid_only =
  property $ do
    (input, expectedOutput) <- forAll genValueTokenPidOnly
    case lex valueTokenPolicyIdOnly input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexAddition :: Property
prop_lexAddition =
  property $ do
    (input, expectedOutput) <- forAll genAdditionToken
    case lex addition input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexSubtraction :: Property
prop_lexSubtraction =
  property $ do
    (input, expectedOutput) <- forAll genSubtractionToken
    case lex subtraction input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexTokens :: Property
prop_lexTokens =
  property $ do
    (input, expectedOutput) <- forAll genTokens
    case lex lexTokens input of
      Left pe -> failWith Nothing $ show pe
      Right tokens ->
        sort (foldl' (\n (_,tk) ->  tk : n) [] tokens) === sort expectedOutput

-- Parsing

prop_parseLovelace :: Property
prop_parseLovelace =
  property $ do
    (input, expectedOutput) <- forAll genLovelaceValue
    tkn <- evalEither $ lex lexTokens input
    case parse preValueLovelace tkn of
      Left pe -> failWith Nothing $ show pe
      Right preVal -> preValToValue preVal === expectedOutput

prop_parseMultiAsset :: Property
prop_parseMultiAsset =
  property $ do
      (input, expectedOutput) <- forAll genMultiAssetValue
      tkn <- evalEither $ lex lexTokens input
      case parse preValueMultiAsset tkn of
        Left pe -> failWith Nothing $ show pe
        Right preVal -> preValToValue preVal === expectedOutput

prop_parseAddition :: Property
prop_parseAddition =
  property $ do
    (input, expectedOutput) <- forAll genAdditionValue
    tkn <- evalEither $ lex lexTokens input
    case parse preValueAddition tkn of
      Left pe -> failWith Nothing $ show pe
      Right preVal -> preValToValue preVal === expectedOutput

prop_parseSubtraction :: Property
prop_parseSubtraction =
  property $ do
    (input, expectedOutput) <- forAll genSubtractionValue
    tkn <- evalEither $ lex lexTokens input
    case parse preValueSubtraction tkn of
      Left pe -> failWith Nothing $ show pe
      Right preVal -> preValToValue preVal === expectedOutput

prop_parse :: Property
prop_parse = property $ do
  (input, expectedOutput) <- forAll genValues
  tkns <- evalEither $ lex lexTokens input
  case parse preValueParser tkns of
    Left pe -> failWith Nothing $ show pe
    Right preVals -> calculateValue preVals === expectedOutput

prop_addition_ada :: Property
prop_addition_ada =
  testAdditionOperation genAdditionValue genLovelaceValue

prop_addition_multi_asset :: Property
prop_addition_multi_asset =
  testAdditionOperation genAdditionValue genMultiAssetValue

prop_subtraction_multi_asset :: Property
prop_subtraction_multi_asset =
  testSubtractionOperation genSubtractionValue genMultiAssetValue

testAdditionOperation :: Gen (Text, Value) -> Gen (Text,Value) -> Property
testAdditionOperation binaryOperation value = property $ do
  (input1, out1) <- forAll value
  (input2, binaryOp) <- forAll binaryOperation
  (input3, out2) <- forAll value
  tkns <- evalEither $ lex lexTokens (input1 <> input2 <> input3)
  case parse preValueParser tkns of
    Left pe -> failWith Nothing $ show pe
    Right preVals -> calculateValue preVals === (out1 <> binaryOp <> out2)

testSubtractionOperation :: Gen (Text, Value) -> Gen (Text,Value) -> Property
testSubtractionOperation binaryOperation value = property $ do
  (input1, out1) <- forAll value
  (input2, binaryOp) <- forAll binaryOperation
  (input3, out2) <- forAll value
  tkns <- evalEither $ lex lexTokens (input1 <> input2 <> input3)
  case parse preValueParser tkns of
    Left pe -> failWith Nothing $ show pe
    Right preVals -> calculateValue preVals === (out1 <> binaryOp <> negateValue out2)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential $$discover

