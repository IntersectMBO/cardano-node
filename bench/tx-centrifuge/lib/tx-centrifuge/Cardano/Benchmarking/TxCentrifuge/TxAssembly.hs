{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.TxAssembly
  ( buildTx
  , buildTxSized
  , TxLimits(..)
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Function ((&))
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import Lens.Micro ((^.))
import Cardano.Ledger.Api qualified as Ledger
import Numeric.Natural (Natural)
------------------
-- bytestring --
------------------
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Codec.Serialise qualified as Serialise
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Hash.SHA512 (SHA512)
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
buildTx destAddr outKey inFunds numOutputs fee metadataBytes =
  buildTxWithMetadata destAddr outKey inFunds numOutputs fee (fillerMetadata inFunds metadataBytes)

-- | Parameters needed by these script-free, ADA-only transactions. Extracted
-- from the node's current era, which can be newer than the Conway wire format.
data TxLimits = TxLimits
  { maxTxSize :: !Word32
  , minFeePerByte :: !Integer
  , minFeeConstant :: !Integer
  } deriving (Eq, Show)

-- | Minimum full signed CBOR size. Choose the smallest filler payload that
-- reaches it; the first metadata entry and CBOR headers can overshoot. A
-- naturally larger transaction is preserved. The fixed fee is never raised.
buildTxSized
  :: Api.AddressInEra Api.ConwayEra
  -> Api.SigningKey Api.PaymentKey
  -> [Fund]
  -> Natural
  -> L.Coin
  -> Natural
  -> TxLimits
  -> Either String (Api.Tx Api.ConwayEra, [Fund])
buildTxSized destAddr outKey inFunds numOutputs fee target limits
  | target == 0 = Left "tx_size must be >= 1"
  | target > fromIntegral (maxTxSize limits) = Left "tx_size exceeds the node's maxTxSize"
  | otherwise = do
      base <- build 0
      result <- if signedSize base >= requested
        then Right base
        else do
          -- The first metadata entry introduces the auxiliary-data hash and
          -- era wrapper. Measure that overhead rather than assuming its size.
          first <- build 1
          if signedSize first >= requested
            then Right first
            else do
              let sizeWith n = signedSize first + metadataSize n - metadataSize 1
                  search lo hi
                    | lo == hi = lo
                    | sizeWith mid >= requested = search lo mid
                    | otherwise = search (mid + 1) hi
                   where mid = lo + (hi - lo) `div` 2
              build (search 1 target)
      let actual = signedSize result
          Api.ShelleyTx _ ledgerTx = fst result
          ledgerSize = ledgerTx ^. Ledger.sizeTxF
          requiredFee = minFeeConstant limits + minFeePerByte limits * fromIntegral ledgerSize
          L.Coin actualFee = fee
      if actual < requested
        then Left "buildTx: signed transaction did not reach tx_size"
        else if actual > fromIntegral (maxTxSize limits) || ledgerSize > maxTxSize limits
          then Left "buildTx: padded transaction exceeds the node's maxTxSize"
          else if actualFee < requiredFee
            then Left $ "buildTx: fee is too small; requires at least " ++ show requiredFee ++ " lovelace"
            else Right result
 where
  requested = fromIntegral target :: Int
  metadataFor = fillerMetadata inFunds
  build n = buildTxWithMetadata destAddr outKey inFunds numOutputs fee (metadataFor n)
  signedSize = BS.length . Api.serialiseToCBOR . fst
  metadataSize n = case metadataFor n of
    Api.TxMetadataNone -> 0
    Api.TxMetadataInEra _ metadata -> BS.length (Api.serialiseToCBOR metadata)

-- | The same deterministic payload is used by raw-byte and total-size modes.
-- Hash all input references and an unbounded chunk counter, sharing hashes
-- between sizing candidates. Each metadata byte string is at most 64 bytes.
fillerMetadata :: [Fund] -> Natural -> Api.TxMetadataInEra Api.ConwayEra
fillerMetadata inFunds = metadataFor
 where
  metadataFor 0 = Api.TxMetadataNone
  metadataFor n = Api.TxMetadataInEra Api.ShelleyBasedEraConway $
    Api.TxMetadata (Map.singleton 0 (Api.TxMetaList (chunks n hashes)))
  seed = BSL.toStrict $ Serialise.serialise
    [ (Api.serialiseToRawBytes txid, ix)
    | Fund{fundTxIn = Api.TxIn txid (Api.TxIx ix)} <- inFunds
    ]
  hashes =
    [ Hash.hashToBytes $ Hash.hashWith @SHA512 id $
        BSL.toStrict $ Serialise.serialise (seed, i)
    | i <- [0 :: Natural ..]
    ]
  chunks 0 _ = []
  chunks n (h : hs) =
    let count = min 64 n
    in Api.TxMetaBytes (BS.take (fromIntegral count) h) : chunks (n - count) hs
  chunks _ [] = error "fillerMetadata: exhausted infinite hash stream"

buildTxWithMetadata
  :: Api.AddressInEra Api.ConwayEra
  -> Api.SigningKey Api.PaymentKey
  -> [Fund]
  -> Natural
  -> L.Coin
  -> Api.TxMetadataInEra Api.ConwayEra
  -> Either String (Api.Tx Api.ConwayEra, [Fund])
buildTxWithMetadata destAddr outKey inFunds numOutputs fee txMetadata
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
