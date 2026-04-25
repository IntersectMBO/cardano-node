{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.TxAssembly
  ( buildTx
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
  -> Either String (Api.Tx Api.ConwayEra, [Fund])
buildTx destAddr outKey inFunds numOutputs fee
  | null inFunds     = Left "buildTx: no input funds"
  | numOutputs  == 0 = Left "buildTx: outputs_per_tx must be >= 1"
  | feeLovelace  < 0 = Left "buildTx: fee must be >= 0"
  | changeTotal <= 0 = Left $ "buildTx: insufficient funds — total inputs ("
                            ++ show totalIn ++ " lovelace) do not cover fee ("
                            ++ show feeLovelace ++ " lovelace)"
    -- Guard against outputs that would be below the Cardano minimum UTxO
    -- value. We cannot check the actual protocol-parameter minimum here (it
    -- depends on the serialised output size and the current coinsPerUTxOByte),
    -- but we can catch the obviously-invalid case where integer division
    -- produces zero-value or negative outputs. A real minimum UTxO check
    -- should be added once the protocol parameters are threaded through to this
    -- function.
  | minOutputLovelace <= 0 = Left $ "buildTx: output value too low — "
                            ++ show numOutputs ++ " outputs from "
                            ++ show changeTotal ++ " lovelace change yields "
                            ++ show minOutputLovelace ++ " lovelace per output"
  | otherwise =
      let maybeTxBody = Api.createTransactionBody
                         (Api.shelleyBasedEra @Api.ConwayEra)
                         txBodyContent
      in case maybeTxBody of
        Left err -> Left ("buildTx: " ++ show err)
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

    -- Unique signing keys from input funds
    -- (deduplicated by verification key hash).
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
