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
import Data.Bits (xor)
import Data.Function ((&))
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Numeric.Natural (Natural)
------------------
-- bytestring --
------------------
import Data.ByteString qualified as BS
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
-- @metadataBytes@ pads the transaction with that many bytes of filler
-- metadata, which grows the transaction without touching the number of inputs
-- or outputs, so the UTxO set stays the size it was. 0 attaches no metadata at
-- all and is the historical behaviour.
--
-- Fixed to ConwayEra. No Plutus, fixed fee.
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
  -- | Bytes of filler metadata to attach.
  -> Natural
  -> Either String (Api.Tx Api.ConwayEra, [Fund])
buildTx destAddr outKey inFunds numOutputs fee metadataBytes
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

    -- The ledger caps each metadata byte string at 64 bytes (enforced in the
    -- decoder since Allegra), so a payload of any size has to be split into a
    -- list of chunks that size. Each chunk costs its 64 bytes plus a 2 byte
    -- CBOR header, so the transaction grows by about 3% more than the figure
    -- asked for, plus a few bytes for the list and map headers and 32 for the
    -- auxiliary data hash in the body.
    metadataChunkSize :: Int
    metadataChunkSize = 64

    -- No CIP assigns meaning to this label; the payload is filler.
    metadataLabel :: Word64
    metadataLabel = 0

    txMetadata :: Api.TxMetadataInEra Api.ConwayEra
    txMetadata
      | metadataBytes == 0 = Api.TxMetadataNone
      | otherwise =
          Api.TxMetadataInEra
            (Api.shelleyBasedEra @Api.ConwayEra)
            ( Api.TxMetadata
                (Map.singleton metadataLabel (Api.TxMetaList metadataChunks))
            )

    -- Filler is seeded from the first input's transaction id and varied per
    -- chunk, so two transactions do not carry the same metadata and no chunk
    -- repeats within one. A constant payload would compress or deduplicate
    -- away wherever anything on the path does either, and would make the
    -- auxiliary data hash identical on every transaction. Deterministic for a
    -- given set of inputs, so a run is still reproducible.
    fillerSeed :: BS.ByteString
    fillerSeed = case inFunds of
      Fund{fundTxIn = Api.TxIn txid _} : _ -> Api.serialiseToRawBytes txid
      -- Unreachable: the null inFunds guard above returns before this.
      [] -> BS.singleton 0x5a

    chunkBytes :: Int -> Int -> BS.ByteString
    chunkBytes i n =
      BS.pack
        [ BS.index fillerSeed ((i + j) `mod` seedLen) `xor` fromIntegral i
        | j <- [0 .. n - 1]
        ]
     where
      seedLen = max 1 (BS.length fillerSeed)

    metadataChunks :: [Api.TxMetadataValue]
    metadataChunks = go 0 (fromIntegral metadataBytes)
     where
      go i remaining
        | remaining <= 0 = []
        | otherwise =
            let n = min metadataChunkSize remaining
            in Api.TxMetaBytes (chunkBytes i n) : go (i + 1) (remaining - n)

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
      & Api.setTxMetadata txMetadata
      -- We are using an explicit fee!
      -- Using `Nothing` instead of `ledgerPP :: Api.LedgerProtocolParameters Api.ConwayEra`.
      -- TODO: Will need something else for plutus scripts!
      & Api.setTxProtocolParams (Api.BuildTxWith Nothing)
