{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.GeneratorTx.SizedMetadata
where

import           Cardano.Api
import           Cardano.Api.Experimental (AnyWitness (..), IsEra (useEra), SignedTx (..),
                   makeUnsignedTx, obtainCommonConstraints, signTx)
import qualified Cardano.Api.Experimental.Tx as Exp

import           Cardano.TxGenerator.Utils

import           Prelude

import qualified Data.ByteString as BS
import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)


maxMapSize :: Int
maxMapSize = 1000
maxBSSize :: Int
maxBSSize = 64

-- Properties of the underlying/opaque CBOR encoding.
assume_cbor_properties :: Bool
assume_cbor_properties
  =    prop_mapCostsConway
    && prop_mapCostsDijkstra
    && prop_bsCostsConway
    && prop_bsCostsDijkstra

-- The cost of map entries in metadata follows a step function.
-- This assumes the map indices are [0..n].
prop_mapCostsConway    :: Bool
prop_mapCostsDijkstra  :: Bool
prop_mapCostsConway    = measureMapCosts AsConwayEra   == assumeMapCosts AsConwayEra
prop_mapCostsDijkstra  = measureMapCosts AsDijkstraEra == assumeMapCosts AsDijkstraEra

assumeMapCosts :: AsType era -> [Int]
assumeMapCosts _proxy = stepFunction [
      (   1 , 0)          -- An empty map of metadata has the same cost as TxMetadataNone.
    , (   1 , 42)         -- Using Metadata costs 42 bytes (first map entry).
    , (  22 , 2)          -- The next 22 entries cost 2 bytes each.
    , ( 233 , 3)          -- 233 entries at 3 bytes.
    , ( 744 , 4)          -- 744 entries at 4 bytes.
    ]

-- Bytestring costs are not LINEAR !!
-- Costs are piecewise linear for payload sizes [0..23] and [24..64].
prop_bsCostsConway   :: Bool
prop_bsCostsDijkstra :: Bool
prop_bsCostsConway    = measureBSCosts AsConwayEra    == [42..65] ++ [67..107]
prop_bsCostsDijkstra  = measureBSCosts AsDijkstraEra  == [42..65] ++ [67..107]

stepFunction :: [(Int, Int)] -> [Int]
stepFunction f = scanl1 (+) steps
 where steps = concatMap (\(count,step) -> replicate count step) f

-- Measure the cost of metadata map entries.
-- This is the cost of the index with an empty BS as payload.
measureMapCosts :: forall era . IsEra era => AsType era -> [Int]
measureMapCosts era = map (metadataSize era . Just . replicateEmptyBS) [0..maxMapSize]
 where
  replicateEmptyBS :: Int -> TxMetadata
  replicateEmptyBS n = listMetadata $ replicate n $ TxMetaBytes BS.empty

listMetadata :: [TxMetadataValue] -> TxMetadata
listMetadata l = makeTransactionMetadata $ Map.fromList $ zip [0..] l

-- Cost of metadata with a single BS of size [0..maxBSSize].
measureBSCosts :: forall era . IsEra era => AsType era -> [Int]
measureBSCosts era = map (metadataSize era . Just . bsMetadata) [0..maxBSSize]
 where bsMetadata s = listMetadata [TxMetaBytes $ BS.replicate s 0]

metadataSize :: forall era . IsEra era => AsType era -> Maybe TxMetadata -> Int
metadataSize p m = dummyTxSize p m - dummyTxSize p Nothing

dummyTxSizeInEra :: forall era . IsEra era => TxMetadataInEra era -> Int
dummyTxSizeInEra metadata = obtainCommonConstraints era $ BS.length $ serialiseToRawBytes dummyTx
 where
  era = useEra @era
  expMetadata = case metadata of
    TxMetadataNone -> mempty
    TxMetadataInEra _ m -> m
  txBodyContent = Exp.defaultTxBodyContent
    & Exp.setTxIns [(mkTxIn "dbaff4e270cfb55612d9e2ac4658a27c79da4a5271c6f90853042d1403733810#0", AnyKeyWitnessPlaceholder)]
    & Exp.setTxMetadata expMetadata
  dummyTx :: SignedTx era
  dummyTx = signTx era [] [] unsignedTx
   where
    unsignedTx = either (\err -> error $ "dummyTxSizeInEra: " ++ show err) id $ makeUnsignedTx era txBodyContent

dummyTxSize :: forall era . IsEra era => AsType era -> Maybe TxMetadata -> Int
dummyTxSize _p m = obtainCommonConstraints (useEra @era) $ dummyTxSizeInEra @era (metadataInEra m)

metadataInEra :: forall era . IsShelleyBasedEra era => Maybe TxMetadata -> TxMetadataInEra era
metadataInEra Nothing = TxMetadataNone
metadataInEra (Just m) = case forEraMaybeEon (cardanoEra @era) of
  Nothing -> error "unreachable"
  Just e -> TxMetadataInEra e m

mkMetadata :: forall era . IsShelleyBasedEra era => Int -> Either String (TxMetadataInEra era)
mkMetadata 0 = Right $ metadataInEra Nothing
mkMetadata size
  = if size < minSize
      then Left $ "Error : metadata must be 0 or at least " ++ show minSize ++ " bytes in this era."
      else Right $ metadataInEra $ Just metadata
 where
  minSize = case shelleyBasedEra @era of
    ShelleyBasedEraShelley  -> 37
    ShelleyBasedEraAllegra  -> 39
    ShelleyBasedEraMary     -> 39
    ShelleyBasedEraAlonzo   -> 39
    ShelleyBasedEraBabbage  -> 39
    ShelleyBasedEraConway   -> 39
    ShelleyBasedEraDijkstra -> 39
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
