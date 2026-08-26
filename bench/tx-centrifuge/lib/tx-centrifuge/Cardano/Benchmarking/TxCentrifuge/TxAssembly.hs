{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- @numOutputs@ outputs, distributed round-robin over the destinations: output
-- @i@ pays to destination @i \`mod\` n@, where @n@ is the number of
-- destinations given, and its recycled fund is keyed with that destination's
-- signing key. Returns the signed transaction and recycled funds (one per
-- output).
--
-- Signing keys are extracted from the input funds. If inputs belong to
-- different keys, all unique keys are used as witnesses. Multiple destinations
-- make multi-witness batches possible after recycling, but do not guarantee
-- them: a batch's witness count is the number of distinct keys among the inputs
-- it happens to draw, which depends on queue order. Note the index restarts
-- every transaction, so with @numOutputs < n@ the tail destinations are never
-- used. A single destination always collapses recycled batches to one witness.
--
-- Era-generic: the caller passes the target 'Api.ShelleyBasedEra', so the same
-- code builds transactions in any Shelley-based era.
-- No Plutus, no metadata, fixed fee.
buildTx
  -- | Target era (also fixes the address and output transaction types).
  :: forall era. Api.ShelleyBasedEra era
  -- | Destinations for outputs: signing key and its address (which embeds
  -- the network identifier). Must be non-empty.
  -> [(Api.SigningKey Api.PaymentKey, Api.AddressInEra era)]
  -- | Input funds.
  -> [Fund]
  -- | Number of outputs.
  -> Natural
  -- | Fee.
  -> L.Coin
  -> Either BuildError (Api.Tx era, [Fund])
buildTx sbe destinations inFunds numOutputs fee
  | null destinations = Left (InvalidInput "no destinations")
  | null inFunds      = Left (InvalidInput "no input funds")
  | numOutputs  == 0  = Left (InvalidInput "outputs_per_tx must be >= 1")
  | feeLovelace  < 0  = Left (InvalidInput "fee must be >= 0")
  | changeTotal <= 0  = Left $ InsufficientValue $
      "total inputs (" ++ show totalIn ++ " lovelace) do not cover fee ("
      ++ show feeLovelace ++ " lovelace)"
    -- Guard against outputs that would be below the Cardano minimum UTxO
    -- value. We cannot check the actual protocol-parameter minimum here (it
    -- depends on the serialised output size and the current coinsPerUTxOByte),
    -- but we can catch the obviously-invalid case where integer division
    -- produces zero-value or negative outputs. A real minimum UTxO check should
    -- be added once the protocol parameters are threaded through to this
    -- function.
  | minOutputLovelace <= 0 = Left $ InsufficientValue $
      show numOutputs ++ " outputs from " ++ show changeTotal
      ++ " lovelace change yields " ++ show minOutputLovelace
      ++ " lovelace per output"
  | otherwise =
      let maybeTxBody = Api.createTransactionBody sbe txBodyContent
      in case maybeTxBody of
        Left err ->
          Left (LedgerFailure ("createTransactionBody: " ++ show err))
        Right txBody ->
          let signedTx = Api.signShelleyTransaction
                           sbe
                           txBody
                           (map Api.WitnessPaymentKey uniqueKeys)
              txId = Api.getTxId txBody
              outFunds = [ Fund { fundTxIn    = Api.TxIn txId (Api.TxIx ix)
                                , fundValue   = amt
                                , fundSignKey = fst (destinationFor ix)
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

    -- Destination for output index i (round-robin).
    destinationFor
      :: Integral i
      => i -> (Api.SigningKey Api.PaymentKey, Api.AddressInEra era)
    destinationFor ix =
      destinations !! (fromIntegral ix `mod` length destinations)

    -- Unique signing keys from input funds (deduplicated by verification key
    -- hash). Steady-state witness count equals the number of distinct
    -- destination keys among the consumed inputs: with a single destination
    -- all recycled inputs share one key (1 witness instead of N, e.g. 270 vs
    -- 371 bytes for 2-in/2-out); with round-robin destinations it depends on
    -- which funds the batch drew (mixed keys typically, but nothing enforces
    -- the interleave).
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
               (Api.Witness Api.WitCtxTxIn era)
           )
         ]
    txIns = map
      (\f ->
        ( fundTxIn f
        , Api.BuildTxWith
            (Api.KeyWitness Api.KeyWitnessForSpending)
        )
      ) inFunds

    mkTxOut :: Api.AddressInEra era -> Integer -> Api.TxOut Api.CtxTx era
    mkTxOut destAddr lovelace = Api.TxOut
      destAddr
      (Api.lovelaceToTxOutValue sbe (Api.Coin lovelace))
      Api.TxOutDatumNone
      Api.ReferenceScriptNone

    txBodyContent :: Api.TxBodyContent Api.BuildTx era
    txBodyContent = Api.defaultTxBodyContent sbe
      & Api.setTxIns txIns
      & Api.setTxInsCollateral Api.TxInsCollateralNone
      & Api.setTxOuts
          (zipWith
            (\ix amt -> mkTxOut (snd (destinationFor (ix :: Int))) amt)
            [0..] outAmounts
          )
      & Api.setTxFee
          ( Api.TxFeeExplicit
              sbe
              (Api.Coin feeLovelace)
          )
      & Api.setTxValidityLowerBound Api.TxValidityNoLowerBound
      & Api.setTxValidityUpperBound
          ( Api.defaultTxValidityUpperBound sbe )
      & Api.setTxMetadata Api.TxMetadataNone
      -- We are using an explicit fee!
      -- Using `Nothing` instead of `ledgerPP :: Api.LedgerProtocolParameters era`.
      -- TODO: Will need something else for plutus scripts!
      & Api.setTxProtocolParams (Api.BuildTxWith Nothing)
