{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Era-generic transaction builder for tx-firehose.
--
-- The transaction is assembled with the ledger 'EraTx' \/ 'EraTxBody' \/
-- 'EraTxOut' type classes so the same code covers every Shelley-based era
-- (Shelley .. Dijkstra). Signing goes through cardano-api's
-- 'makeShelleyKeyWitness'' - that path is era-generic and, unlike
-- 'createTransactionBody', does not trip over the Dijkstra witness
-- extraction case-analysis.
module Cardano.Benchmarking.TxFirehose.Tx where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.TxFirehose.Color (Color, colorBytes, colorMetadataLabel)
import Cardano.Ledger.Api
  ( addrTxWitsL
  , auxDataHashTxBodyL
  , auxDataTxL
  , feeTxBodyL
  , inputsTxBodyL
  , mkBasicTx
  , mkBasicTxBody
  , outputsTxBodyL
  , sizeTxF
  , txIdTx
  , witsTxL
  )
import Cardano.Ledger.Api.Tx.AuxData (EraTxAuxData (TxAuxData), hashTxAuxData)
import Cardano.Ledger.Api.Tx.In (TxId, TxIn, mkTxInPartial)
import Cardano.Ledger.Coin (Coin (Coin))
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe, maybeToStrictMaybe)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Word (Word32)
import Lens.Micro ((%~), (.~), (^.))
import Numeric.Natural (Natural)

-- | A spendable UTxO we own: ledger-level 'TxIn' + its lovelace value.
-- Keeping the internal fund set ledger-typed lets the tx builder stay
-- pure ledger; api conversion only happens at query \/ submit boundaries.
data Fund = Fund
  { fundTxIn :: !TxIn
  , fundValue :: !Integer
  }
  deriving (Eq, Ord, Show)

-- | A built and signed transaction, together with observability
-- metadata we want to trace on submit.
data BuiltTx era = BuiltTx
  { btxSigned :: !(Api.Tx era)
  , btxId :: !TxId
  , btxSize :: !Word32
  , btxInputs :: !Int
  , btxOutputs :: ![Fund]
  }

-- | Build and sign an era-generic 1..n-input, m-output transaction that
-- sends the change back to @destAddr@ under @signingKey@.
buildTx ::
  forall era.
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  Api.AddressInEra era ->
  Api.SigningKey Api.PaymentKey ->
  [Fund] ->
  Natural ->
  Coin ->
  Maybe Color ->
  Either String (BuiltTx era)
buildTx sbe destAddr signingKey inFunds numOutputs fee mColor
  | null inFunds = Left "buildTx: no input funds"
  | numOutputs == 0 = Left "buildTx: outputs_per_tx must be >= 1"
  | feeLovelace < 0 = Left "buildTx: fee must be >= 0"
  | changeTotal <= 0 =
      Left $
        "buildTx: insufficient funds - total inputs ("
          ++ show totalIn
          ++ " lovelace) do not cover fee ("
          ++ show feeLovelace
          ++ ")"
  | minOutputLovelace <= 0 =
      Left $
        "buildTx: output value too low - "
          ++ show numOutputs
          ++ " outputs from "
          ++ show changeTotal
          ++ " lovelace yields "
          ++ show minOutputLovelace
          ++ " per output"
  | otherwise = Right built
 where
  -- Optional colour, carried as transaction metadata so a mempool observer
  -- can attribute the tx to the firehose that made it.
  -- Via cardano-api for the same reason signing is: it case-analyses the era,
  -- so the aux-data constraints resolve where an era-generic build cannot.
  mAuxData :: StrictMaybe (TxAuxData (Api.ShelleyLedgerEra era))
  mAuxData = maybeToStrictMaybe (Api.toAuxiliaryData sbe metadataInEra Api.TxAuxScriptsNone)

  metadataInEra = case mColor of
    Nothing -> Api.TxMetadataNone
    Just color ->
      Api.TxMetadataInEra sbe . Api.makeTransactionMetadata $
        Map.singleton colorMetadataLabel (Api.TxMetaBytes (colorBytes color))

  -- Body: pure ledger, era-generic via EraTxBody. The aux-data hash has to be
  -- in place before signing, since the witness covers it.
  body =
    mkBasicTxBody
      & inputsTxBodyL .~ Set.fromList (map fundTxIn inFunds)
      & outputsTxBodyL %~ (<> StrictSeq.fromList (map mkOut outAmounts))
      & feeTxBodyL .~ fee
      & auxDataHashTxBodyL .~ (hashTxAuxData <$> mAuxData)

  -- Signing via cardano-api - era-generic and Dijkstra-safe.
  witVKey = case Api.makeShelleyKeyWitness'
    sbe
    body
    (Api.WitnessPaymentKey signingKey) of
    Api.ShelleyKeyWitness _ w -> w
    _ -> error "buildTx: unexpected non-Shelley witness"

  ledgerTx =
    mkBasicTx body
      & witsTxL . addrTxWitsL .~ Set.singleton witVKey
      & auxDataTxL .~ mAuxData

  ledgerTxId = txIdTx ledgerTx

  outFunds =
    [ Fund{fundTxIn = mkTxInPartial ledgerTxId ix, fundValue = amt}
    | (ix, amt) <- zip [0 ..] outAmounts
    ]

  built =
    BuiltTx
      { btxSigned = Api.ShelleyTx sbe ledgerTx
      , btxId = ledgerTxId
      , btxSize = ledgerTx ^. sizeTxF
      , btxInputs = length inFunds
      , btxOutputs = outFunds
      }

  -- Money math.
  totalIn = sum (map fundValue inFunds)
  Coin feeLovelace = fee
  changeTotal = totalIn - feeLovelace
  n = fromIntegral numOutputs :: Integer
  minOutputLovelace = changeTotal `div` n
  outAmounts = (base + remainder) : replicate (fromIntegral numOutputs - 1) base
   where
    base = changeTotal `div` n
    remainder = changeTotal `mod` n

  mkOut lovelace = Api.toShelleyTxOutAny sbe apiOut
   where
    apiOut =
      Api.TxOut
        destAddr
        (Api.lovelaceToTxOutValue sbe (Coin lovelace))
        Api.TxOutDatumNone
        Api.ReferenceScriptNone
