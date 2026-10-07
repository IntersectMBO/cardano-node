{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.GeneratorTx.SizedMetadata
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L

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
prop_mapCostsConway    = measureMapCosts Exp.ConwayEra   == assumeMapCosts Exp.ConwayEra
prop_mapCostsDijkstra  = measureMapCosts Exp.DijkstraEra == assumeMapCosts Exp.DijkstraEra

assumeMapCosts :: Exp.Era era -> [Int]
assumeMapCosts era = stepFunction [
      (   1 , 0)          -- An empty map of metadata has the same cost as no metadata.
    , (   1 , firstEntry) -- Using Metadata costs 42 bytes (first map entry).
    , (  22 , 2)          -- The next 22 entries cost 2 bytes each.
    , ( 233 , 3)          -- 233 entries at 3 bytes.
    , ( 744 , 4)          -- 744 entries at 4 bytes.
    ]
  where
    firstEntry = case era of
      Exp.ConwayEra   -> 42
      Exp.DijkstraEra -> 42

-- Bytestring costs are not LINEAR !!
-- Costs are piecewise linear for payload sizes [0..23] and [24..64].
prop_bsCostsConway   :: Bool
prop_bsCostsDijkstra :: Bool
prop_bsCostsConway    = measureBSCosts Exp.ConwayEra   == [42..65] ++ [67..107]
prop_bsCostsDijkstra  = measureBSCosts Exp.DijkstraEra == [42..65] ++ [67..107]

stepFunction :: [(Int, Int)] -> [Int]
stepFunction f = scanl1 (+) steps
 where steps = concatMap (\(count,step) -> replicate count step) f

-- Measure the cost of metadata map entries.
-- This is the cost of the index with an empty BS as payload.
measureMapCosts :: Exp.Era era -> [Int]
measureMapCosts era = map (metadataSize era . replicateEmptyBS) [0..maxMapSize]
 where
  replicateEmptyBS :: Int -> TxMetadata
  replicateEmptyBS n = listMetadata $ replicate n $ TxMetaBytes BS.empty

listMetadata :: [TxMetadataValue] -> TxMetadata
listMetadata l = makeTransactionMetadata $ Map.fromList $ zip [0..] l

-- Cost of metadata with a single BS of size [0..maxBSSize].
measureBSCosts :: Exp.Era era -> [Int]
measureBSCosts era = map (metadataSize era . bsMetadata) [0..maxBSSize]
 where bsMetadata s = listMetadata [TxMetaBytes $ BS.replicate s 0]

metadataSize :: Exp.Era era -> TxMetadata -> Int
metadataSize era m = dummyTxSize era m - dummyTxSize era mempty

-- | Size of an unsigned single-input transaction carrying the given metadata.
-- Empty metadata results in a transaction without auxiliary data.
dummyTxSize :: forall era. Exp.Era era -> TxMetadata -> Int
dummyTxSize era metadata = Exp.obtainCommonConstraints era $
  case Exp.makeUnsignedTx era dummyTx of
    Right (Exp.UnsignedTx tx) -> BS.length $ L.serialize' (Exp.eraProtVerHigh era) tx
    Left err -> error $ "dummyTxSize: " ++ docToString (prettyError err)
 where
  dummyTx :: Exp.TxBodyContent (Exp.LedgerEra era)
  dummyTx = Exp.defaultTxBodyContent
    & Exp.setTxIns
      [ ( mkTxIn "dbaff4e270cfb55612d9e2ac4658a27c79da4a5271c6f90853042d1403733810#0"
        , Exp.AnyKeyWitnessPlaceholder
        )
      ]
    & Exp.setTxFee 0
    & Exp.setTxValidityUpperBound 0
    & Exp.setTxMetadata metadata

-- | Metadata adding @size@ bytes to a transaction; 0 means no metadata.
mkMetadata :: Int -> Either String TxMetadata
mkMetadata 0 = Right mempty
mkMetadata size
  = if size < minSize
      then Left $ "Error : metadata must be 0 or at least " ++ show minSize ++ " bytes."
      else Right metadata
 where
  -- the same in Conway and Dijkstra
  minSize = 39
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
