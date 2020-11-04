{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Gen where

import           Cardano.Prelude

import qualified Data.Sequence.Strict as Strict

import           Cardano.Api.Typed hiding (MaryEra)
import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Mary.Parser (Token (..), tokenToValue)
import           Cardano.Crypto.Hash (hashToTextAsHex)
import           Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Timelock

import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import qualified Shelley.Spec.Ledger.Scripts as Shelley

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Lexing Token Generators

genVariableSpace :: Gen Text
genVariableSpace = Gen.text (Range.constant 1 10) $ return ' '

genLovelaceToken :: Gen (Text, Token)
genLovelaceToken = do
  let mBound = fromIntegral (maxBound :: Word64)
  w64 <-  Gen.integral_ (Range.constant 1 mBound)
  space1 <- genVariableSpace
  space2 <- genVariableSpace
  return (textShow w64 <> space1 <> "lovelace" <> space2, LovelaceT w64)

genValueTokenFullySpecified :: Gen (Text, Token)
genValueTokenFullySpecified = do
  sHash <- genScriptHashMaryText
  assetId <- Gen.text (Range.constant 1 15) Gen.alphaNum
  (mintedText, minted) <- genMintedText
  variableSpace <- genVariableSpace
  return ( mintedText <> variableSpace <> sHash <> "." <> assetId <> variableSpace
         , MultiAssetT sHash assetId minted
         )

genValueTokenPidAndAssetId :: Gen (Text, Token)
genValueTokenPidAndAssetId = do
  sHash <- genScriptHashMaryText
  assetId <- Gen.text (Range.constant 1 15) Gen.alphaNum
  return ( sHash <> "." <> assetId
         , MultiAssetT sHash assetId 1
         )

genValueTokenPidOnly :: Gen (Text, Token)
genValueTokenPidOnly = do
  sHash <- genScriptHashMaryText
  (mintedText, minted) <- genMintedText
  variableSpace <- genVariableSpace
  return ( mintedText <> variableSpace <> sHash
         , MultiAssetT sHash "" minted
         )

genValueTokens :: Gen [(Text, Token)]
genValueTokens = do
 valsFulSpec <- Gen.list (Range.constant 1 10) genValueTokenFullySpecified
 valsPidAssetId <- Gen.list (Range.constant 1 10) genValueTokenPidAndAssetId
 valsPidOnly <- Gen.list (Range.constant 1 10) genValueTokenPidOnly
 return  $ valsFulSpec ++ valsPidAssetId ++ valsPidOnly

genAdditionToken :: Gen (Text, Token)
genAdditionToken = do spaces1 <- genVariableSpace
                      return ("+" <> spaces1, AdditionT)

genSubtractionToken :: Gen (Text, Token)
genSubtractionToken = do spaces1 <- genVariableSpace
                         return ("-" <> spaces1, SubtractionT)

genTokens :: Gen (Text, [Token])
genTokens = do lovelaces <- Gen.list (Range.constant 1 10) genLovelaceToken
               vals <- genValueTokens
               let total = lovelaces ++ vals
               addOrSubtractTk <- Gen.choice [genAdditionToken, genSubtractionToken]
               return . sequence $ intersperse addOrSubtractTk total

genMintedText :: Gen (Text, Integer)
genMintedText = do
  let mBound = fromIntegral (maxBound :: Word64)
  minted <- Gen.integral_ (Range.constant 0 mBound)
  return (textShow minted, minted)

-- Parsing Token Generators

genValues :: Gen (Text, Value)
genValues = do lovelaces <- Gen.list (Range.constant 1 10) genLovelaceValue
               vals <- Gen.list (Range.constant 1 10) genMultiAssetValue
               add <- genAdditionValue
               let total = lovelaces ++ vals
               return . mconcat  $ intersperse add total

genLovelaceValue :: Gen (Text, Value)
genLovelaceValue = do (input, lovelaceToken) <- genLovelaceToken
                      return (input, tokenToValue lovelaceToken)

genMultiAssetValue :: Gen (Text, Value)
genMultiAssetValue = do vTkns <- genValueTokens
                        (input, maToken) <- Gen.element vTkns
                        return ( input
                               , tokenToValue maToken
                               )

genAdditionValue :: Gen (Text, Value)
genAdditionValue = do (input, AdditionT) <- genAdditionToken
                      return (input, valueFromList [])

genSubtractionValue :: Gen (Text, Value)
genSubtractionValue = do (input, SubtractionT) <- genSubtractionToken
                         return (input, valueFromList [])

genMaryScriptHash :: Gen (Shelley.ScriptHash (MaryEra StandardCrypto))
genMaryScriptHash = return . Timelock.hashTimelockScript $ Timelock.RequireAllOf Strict.empty

genScriptHashMaryText :: Gen Text
genScriptHashMaryText = do Shelley.ScriptHash h <- genMaryScriptHash
                           return $ hashToTextAsHex h
