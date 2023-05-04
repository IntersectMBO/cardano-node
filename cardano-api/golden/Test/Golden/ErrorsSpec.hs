{- HLINT ignore "Redundant do" -}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Golden.ErrorsSpec
  ( spec
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley (LeadershipError (..), OperationalCertIssueError (..),
                   ProtocolParametersError (..), ReferenceScript (..))

import           Cardano.Binary as CBOR

import           Test.Hspec

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Typeable (typeRep)

import qualified Cardano.Ledger.Alonzo.Language as Alonzo

import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger

import qualified PlutusCore.Evaluation.Machine.CostModelInterface as Plutus
import qualified PlutusLedgerApi.Common as Plutus

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified HaskellWorks.Hspec.Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H

seed1 :: ByteString
seed1 = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

seed2 :: ByteString
seed2 = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"

spec :: Spec
spec = describe "Test.Golden.Errors" $ do
  let json = Aeson.String "<JSON>"
  let text = "<text>" :: Text
  let string = "<string>" :: String
  let bytestring = "<bytestring>" :: ByteString
  let lazyBytestring = "<lazy-bytestring>" :: LBS.ByteString
  let stakePoolVerKey1 = getVerificationKey $ deterministicSigningKey AsStakePoolKey (Crypto.mkSeedFromBytes seed1)
  let stakePoolVerKey2 = getVerificationKey $ deterministicSigningKey AsStakePoolKey (Crypto.mkSeedFromBytes seed2)
  let paymentVerKey1 = getVerificationKey $ deterministicSigningKey AsPaymentKey (Crypto.mkSeedFromBytes seed1)
  let Right txid1 = deserialiseFromRawBytesHex AsTxId "210c0a4bb6391baf606843e67863d1474cc462374ab12c42d55f78a0b55b56e0"
  let txin1 = TxIn txid1 (TxIx 1)
  let scriptData1 = ScriptDataNumber 1
  let Right scriptDataHash1 = deserialiseFromCBOR AsHashableScriptData $ serialiseToCBOR scriptData1
  let hashScriptData1 = hashScriptDataBytes scriptDataHash1 -- ScriptDataHash $ Ledger.unsafeMakeSafeHash hash1
  let scriptHash = hashScript $ SimpleScript $ RequireTimeBefore $ SlotNo 1
  let costModel = CostModel [0 .. 42]
  let changeaddr1 = AddressInEra
        (ShelleyAddressInEra ShelleyBasedEraAllegra)
        (makeShelleyAddress Mainnet
          (PaymentCredentialByKey (verificationKeyHash paymentVerKey1)) NoStakeAddress)
  let txOutValue1 = TxOutAdaOnly AdaOnlyInAllegraEra 1
  let txout1 = TxOut changeaddr1 txOutValue1 TxOutDatumNone ReferenceScriptNone
  let txOutInAnyEra1 = txOutInAnyEra txout1

  it "Bech32DecodeError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/Bech32DecodeError/Bech32DecodingError.txt"
      $ displayError $ Bech32DecodingError Bech32.StringToDecodeTooLong
    flip H.diffVsGoldenFile "golden/files/errors/Bech32DecodeError/Bech32UnexpectedPrefix.txt"
      $ displayError $ Bech32UnexpectedPrefix text $ Set.singleton text
    flip H.diffVsGoldenFile "golden/files/errors/Bech32DecodeError/Bech32DataPartToBytesError.txt"
      $ displayError $ Bech32DataPartToBytesError text
    flip H.diffVsGoldenFile "golden/files/errors/Bech32DecodeError/Bech32DeserialiseFromBytesError.txt"
      $ displayError $ Bech32DeserialiseFromBytesError bytestring
    flip H.diffVsGoldenFile "golden/files/errors/Bech32DecodeError/Bech32WrongPrefix.txt"
      $ displayError $ Bech32WrongPrefix text text

  it "InputDecodeError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/InputDecodeError/InputTextEnvelopeError.txt"
      $ displayError $ InputTextEnvelopeError $ TextEnvelopeAesonDecodeError string
    flip H.diffVsGoldenFile "golden/files/errors/InputDecodeError/InputBech32DecodeError.txt"
      $ displayError $ InputBech32DecodeError $ Bech32WrongPrefix text text
    flip H.diffVsGoldenFile "golden/files/errors/InputDecodeError/InputInvalidError.txt"
      $ displayError InputInvalidError

  it "InvalidCostModel" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/InvalidCostModel/InvalidCostModel.txt"
      $ displayError $ InvalidCostModel costModel (Plutus.CMInternalWriteError string)

  it "JsonDecodeError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/JsonDecodeError/JsonDecodeError.txt"
      $ displayError $ JsonDecodeError string

  it "LeadershipError" $ H.requireTest $ do
    poolId <- H.leftFail $ deserialiseFromRawBytesHex (AsHash AsStakePoolKey)
      "9e734b6c2263c0917bfc550e9c949f41afa3fe000377243bd29df399"
    flip H.diffVsGoldenFile "golden/files/errors/LeadershipError/LeaderErrDecodeLedgerStateFailure.txt"
      $ displayError LeaderErrDecodeLedgerStateFailure
    flip H.diffVsGoldenFile "golden/files/errors/LeadershipError/LeaderErrDecodeProtocolStateFailure.txt"
      $ displayError $ LeaderErrDecodeProtocolStateFailure
          ( lazyBytestring
          , CBOR.DecoderErrorVoid
          )
    flip H.diffVsGoldenFile "golden/files/errors/LeadershipError/LeaderErrDecodeProtocolEpochStateFailure.txt"
      $ displayError $ LeaderErrDecodeProtocolEpochStateFailure CBOR.DecoderErrorVoid
    flip H.diffVsGoldenFile "golden/files/errors/LeadershipError/LeaderErrGenesisSlot.txt"
      $ displayError LeaderErrGenesisSlot
    flip H.diffVsGoldenFile "golden/files/errors/LeadershipError/LeaderErrStakePoolHasNoStake.txt"
      $ displayError $ LeaderErrStakePoolHasNoStake poolId
    flip H.diffVsGoldenFile "golden/files/errors/LeadershipError/LeaderErrStakeDistribUnstable.txt"
      $ displayError $ LeaderErrStakeDistribUnstable 1 2 3 4
    flip H.diffVsGoldenFile "golden/files/errors/LeadershipError/LeaderErrSlotRangeCalculationFailure.txt"
      $ displayError $ LeaderErrSlotRangeCalculationFailure text
    flip H.diffVsGoldenFile "golden/files/errors/LeadershipError/LeaderErrCandidateNonceStillEvolving.txt"
      $ displayError LeaderErrCandidateNonceStillEvolving

  it "OperationalCertIssueError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/OperationalCertIssueError/OperationalCertKeyMismatch.txt"
      $ displayError $ OperationalCertKeyMismatch stakePoolVerKey1 stakePoolVerKey2

  it "ProtocolParametersError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/ProtocolParametersError/PParamsErrorMissingMinUTxoValue.txt"
      $ displayError $ PParamsErrorMissingMinUTxoValue (AnyCardanoEra ConwayEra)
    flip H.diffVsGoldenFile "golden/files/errors/ProtocolParametersError/PParamsErrorMissingAlonzoProtocolParameter.txt"
      $ displayError PParamsErrorMissingAlonzoProtocolParameter

  it "RawBytesHexError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/RawBytesHexError/RawBytesHexErrorBase16DecodeFail.txt"
      $ displayError $ RawBytesHexErrorBase16DecodeFail bytestring string
    flip H.diffVsGoldenFile "golden/files/errors/RawBytesHexError/RawBytesHexErrorRawBytesDecodeFail.txt"
      $ displayError $ RawBytesHexErrorRawBytesDecodeFail
          bytestring
          (typeRep (AsVerificationKey AsGenesisKey))
          (SerialiseAsRawBytesError string)

  it "ScriptDataJsonBytesError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonBytesError/ScriptDataJsonBytesErrorValue.txt"
      $ displayError $ ScriptDataJsonBytesErrorValue $ ScriptDataJsonSchemaError Aeson.Null ScriptDataJsonNullNotAllowed
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonBytesError/ScriptDataJsonBytesErrorInvalid.txt"
      $ displayError $ ScriptDataJsonBytesErrorInvalid $ ScriptDataConstructorOutOfRange 0

  it "ScriptDataJsonError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonError/ScriptDataJsonSchemaError.txt"
      $ displayError $ ScriptDataJsonSchemaError (Aeson.String "<JSON>") ScriptDataJsonNullNotAllowed
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonError/ScriptDataRangeError.txt"
      $ displayError $ ScriptDataRangeError (Aeson.String "<JSON>") (ScriptDataConstructorOutOfRange 1)

  it "ScriptDataJsonSchemaError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonSchemaError/ScriptDataJsonSchemaError.txt"
      $ displayError $ ScriptDataJsonSchemaError Aeson.Null ScriptDataJsonNullNotAllowed
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonSchemaError/ScriptDataJsonNullNotAllowed.txt"
      $ displayError ScriptDataJsonNullNotAllowed
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonSchemaError/ScriptDataJsonBoolNotAllowed.txt"
      $ displayError ScriptDataJsonBoolNotAllowed
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonSchemaError/ScriptDataJsonNumberNotInteger.txt"
      $ displayError $ ScriptDataJsonNumberNotInteger 0.0
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonSchemaError/ScriptDataJsonNotObject.txt"
      $ displayError $ ScriptDataJsonNotObject json
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonSchemaError/ScriptDataJsonBadObject.txt"
      $ displayError $ ScriptDataJsonBadObject (replicate 5 (text, json))
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonSchemaError/ScriptDataJsonBadMapPair.txt"
      $ displayError $ ScriptDataJsonBadMapPair json
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataJsonSchemaError/ScriptDataJsonTypeMismatch.txt"
      $ displayError $ ScriptDataJsonTypeMismatch text json

  it "ScriptDataRangeError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/ScriptDataRangeError/ScriptDataConstructorOutOfRange.txt"
      $ displayError $ ScriptDataConstructorOutOfRange 1

  it "ScriptExecutionError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorMissingTxIn.txt"
      $ displayError $ ScriptErrorMissingTxIn txin1
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorTxInWithoutDatum.txt"
      $ displayError $ ScriptErrorTxInWithoutDatum txin1
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorWrongDatum.txt"
      $ displayError $ ScriptErrorWrongDatum hashScriptData1
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorEvaluationFailed.txt"
      $ displayError $ ScriptErrorEvaluationFailed Plutus.CostModelParameterMismatch (replicate 5 text)
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorExecutionUnitsOverflow.txt"
      $ displayError ScriptErrorExecutionUnitsOverflow
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorNotPlutusWitnessedTxIn.txt"
      $ displayError $ ScriptErrorNotPlutusWitnessedTxIn (ScriptWitnessIndexTxIn 0) scriptHash
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorRedeemerPointsToUnknownScriptHash.txt"
      $ displayError $ ScriptErrorRedeemerPointsToUnknownScriptHash (ScriptWitnessIndexTxIn 0)
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorMissingScript.txt"
      $ displayError $ ScriptErrorMissingScript (Ledger.RdmrPtr Ledger.Mint 0) Map.empty
    flip H.diffVsGoldenFile "golden/files/errors/ScriptExecutionError/ScriptErrorMissingCostModel.txt"
      $ displayError $ ScriptErrorMissingCostModel Alonzo.PlutusV2

  it "StakePoolMetadataValidationError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/StakePoolMetadataValidationError/StakePoolMetadataJsonDecodeError.txt"
      $ displayError $ StakePoolMetadataJsonDecodeError string
    flip H.diffVsGoldenFile "golden/files/errors/StakePoolMetadataValidationError/StakePoolMetadataInvalidLengthError.txt"
      $ displayError $ StakePoolMetadataInvalidLengthError 0 1

  it "TextEnvelopeCddlError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeCddlError/TextEnvelopeCddlErrCBORDecodingError.txt"
      $ displayError $ TextEnvelopeCddlErrCBORDecodingError CBOR.DecoderErrorVoid
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeCddlError/TextEnvelopeCddlAesonDecodeError.txt"
      $ displayError $ TextEnvelopeCddlAesonDecodeError string string
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeCddlError/TextEnvelopeCddlUnknownKeyWitness.txt"
      $ displayError TextEnvelopeCddlUnknownKeyWitness
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeCddlError/TextEnvelopeCddlTypeError.txt"
      $ displayError $ TextEnvelopeCddlTypeError [text] text
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeCddlError/TextEnvelopeCddlErrUnknownType.txt"
      $ displayError $ TextEnvelopeCddlErrUnknownType text
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeCddlError/TextEnvelopeCddlErrByronKeyWitnessUnsupported.txt"
      $ displayError TextEnvelopeCddlErrByronKeyWitnessUnsupported

  it "TextEnvelopeError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeError/TextEnvelopeTypeError.txt"
      $ displayError $ TextEnvelopeTypeError [TextEnvelopeType string, TextEnvelopeType string] (TextEnvelopeType string)
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeError/TextEnvelopeDecodeError.txt"
      $ displayError $ TextEnvelopeDecodeError CBOR.DecoderErrorVoid
    flip H.diffVsGoldenFile "golden/files/errors/TextEnvelopeError/TextEnvelopeAesonDecodeError.txt"
      $ displayError $ TextEnvelopeAesonDecodeError string

  it "TransactionValidityError" $ H.requireTest $ do
    -- TODO implement this
    -- flip H.diffVsGoldenFile "golden/files/errors/TransactionValidityError/TransactionValidityIntervalError.txt"
    --   $ displayError $ TransactionValidityIntervalError $
    --       Qry.PastHorizon
    --       { Qry.pastHorizonCallStack = GHC.callStack
    --       , Qry.pastHorizonExpression = error "" -- Some $ Qry.ClosedExpr $ Qry.ELit 0
    --       , Qry.pastHorizonSummary = []
    --       }
    flip H.diffVsGoldenFile "golden/files/errors/TransactionValidityError/TransactionValidityTranslationError.txt"
      $ displayError $ TransactionValidityTranslationError $ Ledger.TimeTranslationPastHorizon text
    flip H.diffVsGoldenFile "golden/files/errors/TransactionValidityError/TransactionValidityCostModelError.txt"
      $ displayError $ TransactionValidityCostModelError
          (Map.fromList [(AnyPlutusScriptVersion PlutusScriptV2, costModel)])
          string

  it "TxBodyError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyEmptyTxIns.txt"
      $ displayError TxBodyEmptyTxIns
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyEmptyTxInsCollateral.txt"
      $ displayError TxBodyEmptyTxInsCollateral
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyEmptyTxOuts.txt"
      $ displayError TxBodyEmptyTxOuts
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyOutputNegative.txt"
      $ displayError $ TxBodyOutputNegative 1 txOutInAnyEra1
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyOutputOverflow.txt"
      $ displayError $ TxBodyOutputOverflow 1 txOutInAnyEra1
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyMetadataError.txt"
      $ displayError $ TxBodyMetadataError [(1, TxMetadataBytesTooLong 2)]
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyMintAdaError.txt"
      $ displayError TxBodyMintAdaError
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyMissingProtocolParams.txt"
      $ displayError TxBodyMissingProtocolParams
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyError/TxBodyInIxOverflow.txt"
      $ displayError $ TxBodyInIxOverflow txin1

  it "TxBodyErrorAutoBalance" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyError.txt"
      $ displayError $ TxBodyError TxBodyEmptyTxIns
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyScriptExecutionError.txt"
      $ displayError $ TxBodyScriptExecutionError [(ScriptWitnessIndexTxIn 1, ScriptErrorExecutionUnitsOverflow)]
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyScriptBadScriptValidity.txt"
      $ displayError TxBodyScriptBadScriptValidity
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyErrorAdaBalanceNegative.txt"
      $ displayError $ TxBodyErrorAdaBalanceNegative 1
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyErrorAdaBalanceTooSmall.txt"
      $ displayError $ TxBodyErrorAdaBalanceTooSmall txOutInAnyEra1 0 1
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyErrorByronEraNotSupported.txt"
      $ displayError TxBodyErrorByronEraNotSupported
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyErrorMissingParamMinUTxO.txt"
      $ displayError TxBodyErrorMissingParamMinUTxO
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyErrorValidityInterval.txt"
      $ displayError $ TxBodyErrorValidityInterval $ TransactionValidityCostModelError Map.empty string
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyErrorMinUTxONotMet.txt"
      $ displayError $ TxBodyErrorMinUTxONotMet txOutInAnyEra1 1
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyErrorNonAdaAssetsUnbalanced.txt"
      $ displayError $ TxBodyErrorNonAdaAssetsUnbalanced (valueFromList [(AdaAssetId, Quantity 1)])
    flip H.diffVsGoldenFile "golden/files/errors/TxBodyErrorAutoBalance/TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap.txt"
      $ displayError $ TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap
          (ScriptWitnessIndexTxIn 1)
          (Map.fromList [(ScriptWitnessIndexTxIn 2, ExecutionUnits 1 1)])

  it "TxMetadataJsonError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/TxMetadataJsonError/TxMetadataJsonToplevelNotMap.txt"
      $ displayError TxMetadataJsonToplevelNotMap
    flip H.diffVsGoldenFile "golden/files/errors/TxMetadataJsonError/TxMetadataJsonToplevelBadKey.txt"
      $ displayError $ TxMetadataJsonToplevelBadKey text
    flip H.diffVsGoldenFile "golden/files/errors/TxMetadataJsonError/TxMetadataJsonSchemaError.txt"
      $ displayError $ TxMetadataJsonSchemaError 0 json TxMetadataJsonNullNotAllowed
    flip H.diffVsGoldenFile "golden/files/errors/TxMetadataJsonError/TxMetadataRangeError.txt"
      $ displayError $ TxMetadataRangeError 0 json (TxMetadataBytesTooLong 0)

  it "TxMetadataRangeError" $ H.requireTest $ do
    flip H.diffVsGoldenFile "golden/files/errors/TxMetadataRangeError/TxMetadataJsonToplevelNotMap.txt"
      $ displayError $ TxMetadataBytesTooLong 0
