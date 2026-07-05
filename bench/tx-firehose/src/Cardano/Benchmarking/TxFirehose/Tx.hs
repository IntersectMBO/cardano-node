{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Benchmarking.TxFirehose.Tx
  ( Fund (..)
  , buildTx
  ) where

import Data.Function ((&))
import Numeric.Natural (Natural)

import Cardano.Api qualified as Api

import Cardano.Ledger.Coin qualified as L

-- | A spendable UTxO under our signing key: reference + value.
data Fund = Fund
  { fundTxIn  :: !Api.TxIn
  , fundValue :: !Integer
  }
  deriving (Eq, Ord, Show)

-- | Build and sign a Dijkstra-era transaction spending the given funds and
-- producing @numOutputs@ new UTxOs at @destAddr@. Returns the signed
-- transaction and the resulting output funds (indexed 0..numOutputs-1).
--
-- No Plutus, no metadata, explicit fee. Change is split evenly across
-- outputs with any remainder folded into the first output.
buildTx
  :: Api.AddressInEra Api.DijkstraEra
  -- ^ Destination address (also the change address).
  -> Api.SigningKey Api.PaymentKey
  -- ^ Key witnessing both inputs and (implicitly) future spends of outputs.
  -> [Fund]
  -> Natural
  -- ^ Number of outputs.
  -> L.Coin
  -- ^ Fee.
  -> Either String (Api.Tx Api.DijkstraEra, [Fund])
buildTx destAddr signingKey inFunds numOutputs fee
  | null inFunds = Left "buildTx: no input funds"
  | numOutputs == 0 = Left "buildTx: outputs_per_tx must be >= 1"
  | feeLovelace < 0 = Left "buildTx: fee must be >= 0"
  | changeTotal <= 0 = Left $
      "buildTx: insufficient funds — total inputs (" ++ show totalIn
      ++ " lovelace) do not cover fee (" ++ show feeLovelace ++ ")"
  | minOutputLovelace <= 0 = Left $
      "buildTx: output value too low — " ++ show numOutputs
      ++ " outputs from " ++ show changeTotal
      ++ " lovelace yields " ++ show minOutputLovelace ++ " per output"
  | otherwise = case Api.createTransactionBody sbe txBodyContent of
      Left err -> Left $ "buildTx: " ++ show err
      Right txBody ->
        let signedTx = Api.signShelleyTransaction sbe txBody
                         [Api.WitnessPaymentKey signingKey]
            txId = Api.getTxId txBody
            outFunds =
              [ Fund { fundTxIn = Api.TxIn txId (Api.TxIx ix)
                     , fundValue = amt
                     }
              | (ix, amt) <- zip [0..] outAmounts
              ]
        in Right (signedTx, outFunds)
  where
    sbe = Api.shelleyBasedEra @Api.DijkstraEra

    totalIn = sum (map fundValue inFunds)
    feeLovelace = let L.Coin c = fee in c
    changeTotal = totalIn - feeLovelace
    n = fromIntegral numOutputs :: Integer
    minOutputLovelace = changeTotal `div` n
    outAmounts =
      let base = changeTotal `div` n
          remainder = changeTotal `mod` n
      in (base + remainder) : replicate (fromIntegral numOutputs - 1) base

    txIns =
      [ ( fundTxIn f
        , Api.BuildTxWith
            (Api.KeyWitness Api.KeyWitnessForSpending)
        )
      | f <- inFunds
      ]

    mkTxOut lovelace = Api.TxOut
      destAddr
      (Api.lovelaceToTxOutValue sbe (Api.Coin lovelace))
      Api.TxOutDatumNone
      Api.ReferenceScriptNone

    txBodyContent = Api.defaultTxBodyContent sbe
      & Api.setTxIns txIns
      & Api.setTxInsCollateral Api.TxInsCollateralNone
      & Api.setTxOuts (map mkTxOut outAmounts)
      & Api.setTxFee (Api.TxFeeExplicit sbe (Api.Coin feeLovelace))
      & Api.setTxValidityLowerBound Api.TxValidityNoLowerBound
      & Api.setTxValidityUpperBound
          (Api.defaultTxValidityUpperBound Api.ShelleyBasedEraDijkstra)
      & Api.setTxMetadata Api.TxMetadataNone
      & Api.setTxProtocolParams (Api.BuildTxWith Nothing)
