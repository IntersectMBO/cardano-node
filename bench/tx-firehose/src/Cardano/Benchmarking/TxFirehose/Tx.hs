{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Hash.SHA512 (SHA512)
import Cardano.Ledger.Api
  ( PParams
  , addrTxWitsL
  , auxDataHashTxBodyL
  , auxDataTxL
  , bodyTxL
  , coinTxOutL
  , feeTxBodyL
  , getMinCoinTxOut
  , getMinFeeTx
  , inputsTxBodyL
  , mkBasicTx
  , mkBasicTxBody
  , outputsTxBodyL
  , ppMaxTxSizeL
  , sizeTxF
  , txIdTx
  , witsTxL
  )
import Cardano.Ledger.Api.Tx.AuxData (EraTxAuxData (TxAuxData), hashTxAuxData)
import Cardano.Ledger.Api.Tx.In (TxId, TxIn, mkTxInPartial)
import Cardano.Ledger.Coin (Coin (Coin))
import Codec.Serialise qualified as Serialise
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
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
  , btxSerializedSize :: !Int
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
  Color ->
  Either String (BuiltTx era)
buildTx sbe destAddr signingKey inFunds numOutputs fee color =
  buildTxWithMetadata sbe destAddr signingKey inFunds numOutputs fee (metadataWithPadding color [])

-- | Upstream payload-size padding, deliberately retaining its zero-filled
-- payload and label 1023. This is distinct from the signed-size target.
buildTxPadded ::
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  Api.AddressInEra era ->
  Api.SigningKey Api.PaymentKey ->
  [Fund] ->
  Natural ->
  Coin ->
  Color ->
  Natural ->
  Either String (BuiltTx era)
buildTxPadded sbe addr sk funds outputs fee color padding =
  buildTxWithMetadata sbe addr sk funds outputs fee $
    Api.makeTransactionMetadata $
      Map.fromList $
        (colorMetadataLabel, Api.TxMetaBytes (colorBytes color))
          : [ ( 1023
              , Api.TxMetaList
                  [ Api.TxMetaBytes (BS.replicate (fromIntegral (min 64 (padding - off))) 0)
                  | off <- [0, 64 .. padding - 1]
                  ]
              )
            | padding > 0
            ]

-- | Pad to at least the requested signed CBOR size. CBOR length headers can
-- skip a few sizes; choose the smallest payload that reaches the target.
-- A naturally larger transaction is left intact. Parameters are queried once
-- at startup by the caller; the fixed fee is never silently increased.
buildTxSized ::
  forall era.
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  Api.AddressInEra era ->
  Api.SigningKey Api.PaymentKey ->
  [Fund] ->
  Natural ->
  Coin ->
  Color ->
  Natural ->
  PParams (Api.ShelleyLedgerEra era) ->
  Either String (BuiltTx era)
buildTxSized sbe destAddr signingKey inFunds numOutputs fee color target pp
  | target == 0 = Left "--tx-size must be >= 1"
  | target > fromIntegral maxSize = Left "--tx-size exceeds the node's maxTxSize"
  | otherwise = do
      base <- buildTx sbe destAddr signingKey inFunds numOutputs fee color
      built <-
        if btxSerializedSize base >= requested
          then Right base
          else do
            let baseMetadataSize = metadataSize (metadataFor 0)
                sizeWith n = btxSerializedSize base + metadataSize (metadataFor n) - baseMetadataSize
                -- The colour already supplies the auxiliary-data hash and wrapper.
                -- Only the metadata bytes change, so we can search without signing
                -- every candidate. Hash chunks are shared across those candidates.
                search lo hi
                  | lo == hi = lo
                  | sizeWith mid >= requested = search lo mid
                  | otherwise = search (mid + 1) hi
                 where
                  mid = lo + (hi - lo) `div` 2
                paddingBytes = search 1 (requested - btxSerializedSize base)
            buildTxWithMetadata sbe destAddr signingKey inFunds numOutputs fee (metadataFor paddingBytes)
      -- Measure the final signed transaction, independently of the metadata
      -- sizing calculation. Use the ledger's own fee calculation (including
      -- its era-specific size convention), with no reference scripts.
      if btxSerializedSize built < requested
        then Left "buildTx: signed transaction did not reach --tx-size"
        else
          if btxSerializedSize built > fromIntegral maxSize || btxSize built > maxSize
            then Left $ "buildTx: padded transaction exceeds maxTxSize (" ++ show maxSize ++ ")"
            else case btxSigned built of
              Api.ShelleyTx _ ledgerTx
                | let requiredFee = getMinFeeTx pp ledgerTx 0
                , fee < requiredFee ->
                    Left $
                      "buildTx: --fee "
                        ++ show feeLovelace
                        ++ " is too small for the signed transaction; requires at least "
                        ++ show (Api.unCoin requiredFee)
                        ++ " lovelace"
                | otherwise -> Right built
 where
  maxSize = pp ^. ppMaxTxSizeL
  requested = fromIntegral target :: Int
  Coin feeLovelace = fee
  metadataSize = BS.length . Api.serialiseToCBOR
  metadataFor n = metadataWithPadding color (chunks n hashes)
  -- Encode all input references, including output indices. This avoids the
  -- sibling-input collisions and repeating filler that centrifuge once had.
  seed =
    BSL.toStrict $
      Serialise.serialise
        [ (Api.serialiseToRawBytes txid, ix)
        | Fund tin _ <- inFunds
        , let Api.TxIn txid (Api.TxIx ix) = Api.fromShelleyTxIn tin
        ]
  hashes =
    [ Hash.hashToBytes $
        Hash.hashWith @SHA512 id $
          BSL.toStrict $
            Serialise.serialise (seed, i)
    | i <- [0 :: Natural ..]
    ]
  chunks 0 _ = []
  chunks n (h : hs) = Api.TxMetaBytes (BS.take (min 64 n) h) : chunks (max 0 (n - 64)) hs
  chunks _ [] = error "buildTxSized: exhausted infinite padding stream"

metadataWithPadding :: Color -> [Api.TxMetadataValue] -> Api.TxMetadata
metadataWithPadding color padding =
  Api.makeTransactionMetadata $
    Map.fromList $
      (colorMetadataLabel, Api.TxMetaBytes (colorBytes color))
        : [(0, Api.TxMetaList padding) | not (null padding)]

buildTxWithMetadata ::
  forall era.
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  Api.AddressInEra era ->
  Api.SigningKey Api.PaymentKey ->
  [Fund] ->
  Natural ->
  Coin ->
  Api.TxMetadata ->
  Either String (BuiltTx era)
buildTxWithMetadata sbe destAddr signingKey inFunds numOutputs fee metadata
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

  built =
    buildOutputs
      sbe
      signingKey
      inFunds
      fee
      metadata
      [(destAddr, amt) | amt <- outAmounts]

-- | Explicit outputs, shared by ordinary payments and retained-output growth.
-- Callers validate conservation and output values before constructing these.
buildOutputs ::
  forall era.
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  Api.SigningKey Api.PaymentKey ->
  [Fund] ->
  Coin ->
  Api.TxMetadata ->
  [(Api.AddressInEra era, Integer)] ->
  BuiltTx era
buildOutputs sbe signingKey inFunds fee metadata destinations = built
 where
  -- Colour tag, carried as transaction metadata so a mempool observer can
  -- attribute the tx to the firehose that made it.
  -- Via cardano-api for the same reason signing is: it case-analyses the era,
  -- so the aux-data constraints resolve where an era-generic build cannot.
  mAuxData :: StrictMaybe (TxAuxData (Api.ShelleyLedgerEra era))
  mAuxData = maybeToStrictMaybe (Api.toAuxiliaryData sbe metadataInEra Api.TxAuxScriptsNone)

  metadataInEra = Api.TxMetadataInEra sbe metadata

  -- Body: pure ledger, era-generic via EraTxBody. The aux-data hash has to be
  -- in place before signing, since the witness covers it.
  body =
    mkBasicTxBody
      & inputsTxBodyL .~ Set.fromList (map fundTxIn inFunds)
      & outputsTxBodyL %~ (<> StrictSeq.fromList (map mkOut destinations))
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
    | (ix, amt) <- zip [0 ..] (map snd destinations)
    ]

  built =
    BuiltTx
      { btxSigned = Api.ShelleyTx sbe ledgerTx
      , btxId = ledgerTxId
      , btxSize = ledgerTx ^. sizeTxF
      , btxSerializedSize = BS.length (Api.serialiseToCBOR (Api.ShelleyTx sbe ledgerTx))
      , btxInputs = length inFunds
      , btxOutputs = outFunds
      }

  mkOut (address, lovelace) =
    Api.toShelleyTxOutAny sbe $
      Api.TxOut
        address
        (Api.lovelaceToTxOutValue sbe (Coin lovelace))
        Api.TxOutDatumNone
        Api.ReferenceScriptNone

-- | Validate the actual signed transaction against the queried parameters.
validateTx ::
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  PParams (Api.ShelleyLedgerEra era) ->
  BuiltTx era ->
  Either String (BuiltTx era)
validateTx _ pp built
  | btxSerializedSize built > fromIntegral limit || btxSize built > limit =
      Left "buildTx: transaction exceeds maxTxSize"
  | otherwise = case btxSigned built of
      Api.ShelleyTx _ tx
        | tx ^. bodyTxL . feeTxBodyL < getMinFeeTx pp tx 0 ->
            Left "buildTx: --fee is below the ledger minimum"
        | any
            (\out -> out ^. coinTxOutL < getMinCoinTxOut pp out)
            (tx ^. bodyTxL . outputsTxBodyL) ->
            Left "buildTx: output is below minimum UTxO"
        | otherwise -> Right built
 where
  limit = pp ^. ppMaxTxSizeL

-- | One input, N retained outputs, then exactly one recyclable change output.
-- Explicit fee/count makes journal replay independent of later parameters.
buildFanout ::
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  Api.AddressInEra era ->
  Api.AddressInEra era ->
  Api.SigningKey Api.PaymentKey ->
  Fund ->
  Int ->
  Integer ->
  Coin ->
  Color ->
  Either String (BuiltTx era)
buildFanout sbe changeAddr retainedAddr sk input count amount fee color
  | count <= 0 || count > 65534 = Left "fanout count must be 1..65534"
  | amount <= 0 || Api.unCoin fee < 0 || change <= 0 = Left "insufficient fanout funding"
  | otherwise =
      Right $
        buildOutputs sbe sk [input] fee (metadataWithPadding color []) $
          replicate count (retainedAddr, amount) ++ [(changeAddr, change)]
 where
  change = fundValue input - toInteger count * amount - Api.unCoin fee

-- | Solve fees from signed bytes; changing fee/change CBOR widths can change
-- the required fee again. Never lower the fee during convergence.
autoFee ::
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  PParams (Api.ShelleyLedgerEra era) ->
  (Coin -> Either String (BuiltTx era)) ->
  Either String (BuiltTx era)
autoFee sbe pp build = go (8 :: Int) (Coin 0)
 where
  go 0 _ = Left "fee calculation did not converge"
  go n fee = do
    tx <- build fee
    let required = case btxSigned tx of
          Api.ShelleyTx _ ledgerTx -> getMinFeeTx pp ledgerTx 0
    if required > fee then go (n - 1) required else validateTx sbe pp tx

-- | Bound allocation before building, and find the largest fitting fanout.
-- Validation also checks minimum coin on change, not just retained outputs.
fitFanout ::
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  PParams (Api.ShelleyLedgerEra era) ->
  Api.AddressInEra era ->
  Api.AddressInEra era ->
  Api.SigningKey Api.PaymentKey ->
  Fund ->
  Int ->
  Integer ->
  Color ->
  Either String (BuiltTx era)
fitFanout sbe pp changeAddr retainedAddr sk input requested amount color
  | requested <= 0 = Left "fanout count must be positive"
  | upper <= 0 = Left "maxTxSize is too small for fanout"
  | otherwise = case candidate upper of
      Right tx -> Right tx
      Left _ -> do
        first <- candidate 1
        search first 1 (upper - 1)
 where
  -- Usually the requested batch fits. Avoid signing every intermediate
  -- binary-search candidate on the normal path.
  upper = min 65534 (min requested (fromIntegral (pp ^. ppMaxTxSizeL) `div` 30))
  candidate count = autoFee sbe pp $ \fee ->
    buildFanout sbe changeAddr retainedAddr sk input count amount fee color
  search best lo hi
    | lo >= hi = Right best
    | otherwise = case candidate mid of
        Right tx -> search tx mid hi
        Left _ -> search best lo (mid - 1)
   where
    mid = lo + (hi - lo + 1) `div` 2
