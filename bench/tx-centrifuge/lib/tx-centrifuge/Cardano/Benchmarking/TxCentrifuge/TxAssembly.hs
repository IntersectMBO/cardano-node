{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.TxAssembly
  ( buildTx
  , BuildError (..)
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Function ((&))
import Data.List (nubBy)
import Numeric.Natural (Natural)
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
-------------------------
-- cardano-ledger-core --
-------------------------
import Cardano.Ledger.Coin qualified as L
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Fund ( Fund(..) )

--------------------------------------------------------------------------------

-- | Why 'buildTx' could not produce a transaction. The caller uses the
-- distinction to decide whether to recover or fail. 'InsufficientValue' is a
-- per-batch condition (these particular inputs are too small), so dropping the
-- batch and trying the next is correct. 'InvalidInput' (a bad argument) and
-- 'LedgerFailure' (an opaque ledger rejection) do not depend on the inputs, so
-- they are constant across batches and must surface loudly instead of retried.
data BuildError
  = -- | The function was called with invalid arguments: no input funds, zero
    -- outputs, or a negative fee. Precondition violations known immediately
    -- from the arguments (and guarded upstream in 'interpretBuilder'), so
    -- reaching one is a caller or config bug, not a recoverable per-batch
    -- condition.
    InvalidInput !String
  | -- | The input funds cannot cover the fee plus one valid (non-zero) output
    -- each: the change is at or below zero, or too small to split into
    -- @numOutputs@ outputs. A per-batch condition that depends on the specific
    -- inputs, so the caller can drop this batch and try the next.
    InsufficientValue !String
  | -- | The ledger rejected the transaction in 'Api.createTransactionBody'.
    -- Internal to cardano-api and opaque to us, and constant across batches for
    -- our fixed tx shape, so it must surface loudly rather than be retried.
    LedgerFailure !String
  deriving Show

-- | Build and sign a transaction consuming the given funds and producing
-- @numOutputs@ outputs to @destAddr@. Returns the signed transaction and
-- recycled funds (one per output, keyed with @outKey@ for future spending).
--
-- Signing keys are extracted from the input funds. If inputs belong to
-- different keys, all unique keys are used as witnesses.
--
-- Fixed to ConwayEra. No Plutus, no metadata, fixed fee.
buildTx
  -- | Destination address for outputs (embeds the network identifier).
  :: Api.AddressInEra Api.ConwayEra
  -- | Signing key for recycled output funds.
  -> Api.SigningKey Api.PaymentKey
  -- | Input funds.
  -> [Fund]
  -- | Number of outputs.
  -> Natural
  -- | Fee.
  -> L.Coin
  -> Either BuildError (Api.Tx Api.ConwayEra, [Fund])
buildTx destAddr outKey inFunds numOutputs fee
  | null inFunds     = Left (InvalidInput "no input funds")
  | numOutputs  == 0 = Left (InvalidInput "outputs_per_tx must be >= 1")
  | feeLovelace  < 0 = Left (InvalidInput "fee must be >= 0")
  | changeTotal <= 0 = Left $ InsufficientValue $
      "total inputs (" ++ show totalIn ++ " lovelace) do not cover fee ("
      ++ show feeLovelace ++ " lovelace)"
    -- Guard against outputs that would be below the Cardano minimum UTxO
    -- value. We cannot check the actual protocol-parameter minimum here (it
    -- depends on the serialised output size and the current coinsPerUTxOByte),
    -- but we can catch the obviously-invalid case where integer division
    -- produces zero-value or negative outputs. A real minimum UTxO check
    -- should be added once the protocol parameters are threaded through to this
    -- function.
  | minOutputLovelace <= 0 = Left $ InsufficientValue $
      show numOutputs ++ " outputs from " ++ show changeTotal
      ++ " lovelace change yields " ++ show minOutputLovelace
      ++ " lovelace per output"
  | otherwise =
      let maybeTxBody = Api.createTransactionBody
                         (Api.shelleyBasedEra @Api.ConwayEra)
                         txBodyContent
      in case maybeTxBody of
        Left err ->
          Left (LedgerFailure ("createTransactionBody: " ++ show err))
        Right txBody ->
          let signedTx = Api.signShelleyTransaction
                           (Api.shelleyBasedEra @Api.ConwayEra)
                           txBody
                           (map Api.WitnessPaymentKey uniqueKeys)
              txId = Api.getTxId txBody
              outFunds = [ Fund { fundTxIn    = Api.TxIn txId (Api.TxIx ix)
                                , fundValue   = amt
                                , fundSignKey = outKey
                                }
                         | (ix, amt) <- zip [0..] outAmounts
                         ]
          in Right (signedTx, outFunds)
  where

    totalIn :: Integer
    totalIn = sum (map fundValue inFunds)

    feeLovelace :: Integer
    feeLovelace = let L.Coin c = fee in c

    changeTotal :: Integer
    changeTotal = totalIn - feeLovelace

    -- Minimum per-output lovelace amount (used for the zero-value guard above).
    minOutputLovelace :: Integer
    minOutputLovelace = changeTotal `div` fromIntegral numOutputs

    -- Split change evenly; first output absorbs the remainder.
    outAmounts :: [Integer]
    outAmounts =
      let base = changeTotal `div` fromIntegral numOutputs
          remainder = changeTotal `mod` fromIntegral numOutputs
      in (base + remainder) : replicate (fromIntegral numOutputs - 1) base

    -- Unique signing keys from input funds (deduplicated by verification key
    -- hash). After recycling, all inputs share the builder's single key, so
    -- this produces 1 witness instead of N, making steady-state transactions
    -- smaller than the initial batch (e.g. 270 vs 371 bytes for 2-in/2-out).
    uniqueKeys :: [Api.SigningKey Api.PaymentKey]
    uniqueKeys = nubBy sameKey (map fundSignKey inFunds)
      where
        sameKey
          :: Api.SigningKey Api.PaymentKey
          -> Api.SigningKey Api.PaymentKey
          -> Bool
        sameKey a b = Api.verificationKeyHash (Api.getVerificationKey a)
                   == Api.verificationKeyHash (Api.getVerificationKey b)

    txIns
      :: [ ( Api.TxIn
           , Api.BuildTxWith Api.BuildTx
               (Api.Witness Api.WitCtxTxIn Api.ConwayEra)
           )
         ]
    txIns = map
      (\f ->
        ( fundTxIn f
        , Api.BuildTxWith
            (Api.KeyWitness Api.KeyWitnessForSpending)
        )
      ) inFunds

    mkTxOut :: Integer -> Api.TxOut Api.CtxTx Api.ConwayEra
    mkTxOut lovelace = Api.TxOut
      destAddr
      ( Api.shelleyBasedEraConstraints
          (Api.shelleyBasedEra @Api.ConwayEra) $
          Api.lovelaceToTxOutValue
            (Api.shelleyBasedEra @Api.ConwayEra)
            (Api.Coin lovelace)
      )
      Api.TxOutDatumNone
      Api.ReferenceScriptNone

    txBodyContent :: Api.TxBodyContent Api.BuildTx Api.ConwayEra
    txBodyContent = Api.defaultTxBodyContent Api.ShelleyBasedEraConway
      & Api.setTxIns txIns
      & Api.setTxInsCollateral Api.TxInsCollateralNone
      & Api.setTxOuts (map mkTxOut outAmounts)
      & Api.setTxFee
          ( Api.TxFeeExplicit
              (Api.shelleyBasedEra @Api.ConwayEra)
              (Api.Coin feeLovelace)
          )
      & Api.setTxValidityLowerBound Api.TxValidityNoLowerBound
      & Api.setTxValidityUpperBound
          ( Api.defaultTxValidityUpperBound
              Api.ShelleyBasedEraConway
          )
      & Api.setTxMetadata Api.TxMetadataNone
      -- We are using an explicit fee!
      -- Using `Nothing` instead of `ledgerPP :: Api.LedgerProtocolParameters Api.ConwayEra`.
      -- TODO: Will need something else for plutus scripts!
      & Api.setTxProtocolParams (Api.BuildTxWith Nothing)
