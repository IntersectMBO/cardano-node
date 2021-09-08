{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.GeneratorTx.SizedMetadata
where

import           Prelude

import           Cardano.Api
import           Cardano.Benchmarking.GeneratorTx.Tx
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)

maxMapSize :: Int
maxMapSize = 1000
maxBSSize :: Int
maxBSSize = 64

-- Properties of the underlying/opaque CBOR encoding.
assume_cbor_properties :: Bool
assume_cbor_properties
  =    prop_mapCostsShelley
    && prop_mapCostsAllegra
    && prop_mapCostsMary
    && prop_mapCostsAlonzo
    && prop_bsCostsShelley
    && prop_bsCostsAllegra
    && prop_bsCostsMary
    && prop_bsCostsAlonzo

-- The cost of map entries in metadata follows a step function.
-- This assums the map indecies are [0..n].
prop_mapCostsShelley :: Bool
prop_mapCostsAllegra :: Bool
prop_mapCostsMary    :: Bool
prop_mapCostsAlonzo  :: Bool
prop_mapCostsShelley = measureMapCosts AsShelleyEra == assumeMapCosts AsShelleyEra
prop_mapCostsAllegra = measureMapCosts AsAllegraEra == assumeMapCosts AsAllegraEra
prop_mapCostsMary    = measureMapCosts AsMaryEra    == assumeMapCosts AsMaryEra
prop_mapCostsAlonzo  = measureMapCosts AsAlonzoEra  == assumeMapCosts AsAlonzoEra

assumeMapCosts :: forall era . IsShelleyBasedEra era => AsType era -> [Int]
assumeMapCosts _proxy = stepFunction [
      (   1 , 0)          -- An empty map of metadata has the same cost as TxMetadataNone.
    , (   1 , firstEntry) -- Using Metadata costs 37 or 39 bytes  (first map entry).
    , (  22 , 2)          -- The next 22 entries cost 2 bytes each.
    , ( 233 , 3)          -- 233 entries at 3 bytes.
    , ( 744 , 4)          -- 744 entries at 4 bytes.
    ]
  where
    firstEntry = case shelleyBasedEra @ era of
      ShelleyBasedEraShelley -> 37
      ShelleyBasedEraAllegra -> 39
      ShelleyBasedEraMary    -> 39
 -- Unconfirmed ! update when alonzo is runnable.
      ShelleyBasedEraAlonzo  -> error "39"

-- Bytestring costs are not LINEAR !!
-- Costs are piecewise linear for payload sizes [0..23] and [24..64].
prop_bsCostsShelley  :: Bool
prop_bsCostsAllegra :: Bool
prop_bsCostsMary    :: Bool
prop_bsCostsAlonzo  :: Bool
prop_bsCostsShelley  = measureBSCosts AsShelleyEra == [37..60] ++ [62..102]
prop_bsCostsAllegra = measureBSCosts AsAllegraEra == [39..62] ++ [64..104]
prop_bsCostsMary    = measureBSCosts AsMaryEra    == [39..62] ++ [64..104]
 -- Unconfirmed ! update when alonzo is runnable.
prop_bsCostsAlonzo  = measureBSCosts AsAlonzoEra  == error "[39..62] ++ [64..104]"

stepFunction :: [(Int, Int)] -> [Int]
stepFunction f = scanl1 (+) steps
 where steps = concatMap (\(count,step) -> replicate count step) f

-- Measure the cost of metadata map entries.
-- This is the cost of the index with an empty BS as payload.
measureMapCosts :: forall era . IsShelleyBasedEra era => AsType era -> [Int]
measureMapCosts era = map (metadataSize era . Just . replicateEmptyBS) [0..maxMapSize]
 where
  replicateEmptyBS :: Int -> TxMetadata
  replicateEmptyBS n = listMetadata $ replicate n $ TxMetaBytes BS.empty

listMetadata :: [TxMetadataValue] -> TxMetadata
listMetadata l = makeTransactionMetadata $ Map.fromList $ zip [0..] l

-- Cost of metadata with a single BS of size [0..maxBSSize].
measureBSCosts :: forall era . IsShelleyBasedEra era => AsType era -> [Int]
measureBSCosts era = map (metadataSize era . Just . bsMetadata) [0..maxBSSize]
 where bsMetadata s = listMetadata [TxMetaBytes $ BS.replicate s 0]

metadataSize :: forall era . IsShelleyBasedEra era => AsType era -> Maybe TxMetadata -> Int
metadataSize p m = dummyTxSize p m - dummyTxSize p Nothing

dummyTxSizeInEra :: forall era . IsShelleyBasedEra era => TxMetadataInEra era -> Int
dummyTxSizeInEra metadata = case makeTransactionBody dummyTx of
  Right b -> BS.length $ serialiseToCBOR b
  Left err -> error $ "metaDataSize " ++ show err
 where
  dummyTx :: TxBodyContent BuildTx era
  dummyTx = TxBodyContent {
      txIns = [( TxIn "dbaff4e270cfb55612d9e2ac4658a27c79da4a5271c6f90853042d1403733810" (TxIx 0)
               , BuildTxWith $ KeyWitness KeyWitnessForSpending )]
    , txInsCollateral = TxInsCollateralNone
    , txOuts = []
    , txFee = mkFee 0
    , txValidityRange = (TxValidityNoLowerBound, mkValidityUpperBound 0)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }

dummyTxSize :: forall era . IsShelleyBasedEra era => AsType era -> Maybe TxMetadata -> Int
dummyTxSize _p m = (dummyTxSizeInEra @ era) $ metadataInEra m

metadataInEra :: forall era . IsShelleyBasedEra era => Maybe TxMetadata -> TxMetadataInEra era
metadataInEra Nothing = TxMetadataNone
metadataInEra (Just m) = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxMetadataInEra TxMetadataInShelleyEra m
  ShelleyBasedEraAllegra -> TxMetadataInEra TxMetadataInAllegraEra m
  ShelleyBasedEraMary    -> TxMetadataInEra TxMetadataInMaryEra m
  ShelleyBasedEraAlonzo  -> TxMetadataInEra TxMetadataInAlonzoEra m

mkMetadata :: forall era . IsShelleyBasedEra era => Int -> Either String (TxMetadataInEra era)
mkMetadata 0 = Right $ metadataInEra Nothing
mkMetadata size
  = if size < minSize
      then Left $ "Error : metadata must be 0 or at least " ++ show minSize ++ " bytes in this era."
      else Right $ metadataInEra $ Just metadata
 where
  minSize = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> 37
    ShelleyBasedEraAllegra -> 39
    ShelleyBasedEraMary    -> 39
    ShelleyBasedEraAlonzo  -> 39 -- TODO: check minSize for Alonzo
  nettoSize = size - minSize

  -- At 24 the CBOR representation changes.
  maxLinearByteStringSize = 23
  fullChunkSize = maxLinearByteStringSize + 1

  -- A full chunk consists of 4 bytes for the index and 20 bytes for the bytestring.
  -- Each full chunk adds exactly `fullChunkSize` (== 24) bytes.
  -- The remainder is added in the first chunk.
  mkFullChunk ix = (ix, TxMetaBytes $ BS.replicate (fullChunkSize - 4) 0)

  fullChunkCount :: Word64
  fullChunkCount = fromIntegral $ nettoSize `div` fullChunkSize

  -- Full chunks use indices starting at 1000, to enforce 4-byte encoding of the index.
  -- At some index the encoding will change to 5 bytes and this will break.
  fullChunks = map mkFullChunk [1000 .. 1000 + fullChunkCount -1]

  -- The first chunk has a variable size.
  firstChunk =
    ( 0  -- the first chunk uses index 0
    , TxMetaBytes $ BS.replicate (nettoSize `mod` fullChunkSize) 0
    )

  metadata = makeTransactionMetadata $ Map.fromList (firstChunk : fullChunks)
