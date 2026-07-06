{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- | Dijkstra-era transaction builder.
--
-- Uses the experimental 'Cardano.Api.Experimental' API because the stable
-- 'Api.createTransactionBody' path is not yet Dijkstra-complete
-- (@caseShelleyToBabbageOrConwayEraOnwards@ errors out on the Dijkstra
-- witness-extraction branch inside 'collectTxBodyScriptWitnessRequirements').
--
-- 'Exp.makeUnsignedTx' bypasses that path entirely: it constructs the ledger
-- transaction directly from the experimental 'TxBodyContent (LedgerEra era)'.
-- We wrap the resulting 'Exp.SignedTx' back into an old-style 'Api.Tx' via the
-- 'Api.ShelleyTx' constructor so the caller can hand it to 'TxInMode' / the
-- LocalTxSubmission client without further changes.
--
-- TxId and wire size come straight from cardano-ledger-api on the raw ledger
-- Tx we already have in hand — no need to round-trip through the deprecated
-- 'Api.getTxBody'.
module Cardano.Benchmarking.TxFirehose.Tx
  ( Fund (..)
  , BuiltTx (..)
  , buildTx
  ) where

import Data.Function ((&))
import Lens.Micro ((^.))
import Data.Word (Word32)
import Numeric.Natural (Natural)

import Cardano.Api qualified as Api

import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp

import Cardano.Ledger.Api (sizeTxF, txIdTx)
import Cardano.Ledger.Api.Tx.In (TxId)
import Cardano.Ledger.Coin qualified as L

-- | A spendable UTxO under our signing key.
data Fund = Fund
  { fundTxIn  :: !Api.TxIn
  , fundValue :: !Integer
  }
  deriving (Eq, Ord, Show)

-- | A built and signed transaction, together with the ledger-native
-- txId and wire size (bytes). Callers wanting to submit it hand
-- 'btxSigned' to 'TxInMode'; observability uses 'btxId' / 'btxSize'.
data BuiltTx = BuiltTx
  { btxSigned  :: !(Api.Tx Api.DijkstraEra)
  , btxId      :: !TxId
  , btxSize    :: !Word32
  , btxOutputs :: ![Fund]
  }

-- | Build and sign a Dijkstra-era transaction.
buildTx
  :: Api.AddressInEra Api.DijkstraEra
  -> Api.SigningKey Api.PaymentKey
  -> [Fund]
  -> Natural
  -> L.Coin
  -> Either String BuiltTx
buildTx destAddr signingKey inFunds numOutputs fee
  | null inFunds = Left "buildTx: no input funds"
  | numOutputs == 0 = Left "buildTx: outputs_per_tx must be >= 1"
  | feeLovelace < 0 = Left "buildTx: fee must be >= 0"
  | changeTotal <= 0 = Left $
      "buildTx: insufficient funds - total inputs (" ++ show totalIn
      ++ " lovelace) do not cover fee (" ++ show feeLovelace ++ ")"
  | minOutputLovelace <= 0 = Left $
      "buildTx: output value too low - " ++ show numOutputs
      ++ " outputs from " ++ show changeTotal
      ++ " lovelace yields " ++ show minOutputLovelace ++ " per output"
  | otherwise = case Exp.makeUnsignedTx Exp.DijkstraEra bodyContent of
      Left err -> Left $ "buildTx: " ++ show err
      Right unsigned ->
        let keyWit = Exp.makeKeyWitness Exp.DijkstraEra unsigned
                       (Api.WitnessPaymentKey signingKey)
            Exp.SignedTx ledgerTx = Exp.signTx Exp.DijkstraEra [] [keyWit] unsigned
            signedTx = Api.ShelleyTx sbe ledgerTx
            ledgerTxId = txIdTx ledgerTx
            apiTxId    = Api.fromShelleyTxId ledgerTxId
            outFunds =
              [ Fund { fundTxIn = Api.TxIn apiTxId (Api.TxIx ix)
                     , fundValue = amt
                     }
              | (ix, amt) <- zip [0..] outAmounts
              ]
        in Right BuiltTx
             { btxSigned  = signedTx
             , btxId      = ledgerTxId
             , btxSize    = ledgerTx ^. sizeTxF
             , btxOutputs = outFunds
             }
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

    mkOut :: Integer -> Exp.TxOut (Exp.LedgerEra Api.DijkstraEra)
    mkOut lovelace =
      let apiOut = Api.TxOut
            destAddr
            (Api.lovelaceToTxOutValue sbe (Api.Coin lovelace))
            Api.TxOutDatumNone
            Api.ReferenceScriptNone
      in Exp.TxOut (Api.toShelleyTxOutAny sbe apiOut)

    bodyContent =
      Exp.defaultTxBodyContent
        & Exp.setTxIns
            [ (fundTxIn f, Exp.AnyKeyWitnessPlaceholder) | f <- inFunds ]
        & Exp.setTxOuts (map mkOut outAmounts)
        & Exp.setTxFee fee
