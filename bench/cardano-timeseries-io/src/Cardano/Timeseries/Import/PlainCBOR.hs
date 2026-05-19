{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans #-}

module Cardano.Timeseries.Import.PlainCBOR where

import           Cardano.Timeseries.Domain.Instant (Instant (..))
import           Cardano.Timeseries.Store.Flat (Flat, Point (..))

import           Codec.Serialise
import           Data.Map.Strict as Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (Text, unpack)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics (Generic)


data NumericValue =
    NVInt       Int
  | NVDouble    Double
  deriving (Generic, Show, Serialise)


data Snapshot = Snapshot
  { singletonLabel  :: Text
  , timeStamp       :: POSIXTime
  , scrape          :: Map Text NumericValue
  }
  deriving (Generic, Serialise)

instance Show Snapshot where
  show (Snapshot l t s) = "Snapshot{" ++ unpack l ++ "} @ " ++ show t ++ ", entries: " ++ show s


instance Serialise POSIXTime where
  encode = encode . toInteger . floor
  decode = fromInteger <$> decode


readFileSnapshots :: FilePath -> IO [Snapshot]
readFileSnapshots = readFileDeserialise

numericValueToDouble :: NumericValue -> Double
numericValueToDouble (NVInt x) = fromIntegral x
numericValueToDouble (NVDouble x) = x

scrapeDatapointToPoint :: Text -> POSIXTime -> Text -> NumericValue -> Point Double
scrapeDatapointToPoint node t metric v =
  Point metric (Instant (Set.fromList [("node", node)]) (floor (t * 1000)) (numericValueToDouble v))

snapshotToFlatStore :: Snapshot -> Flat Double
snapshotToFlatStore (Snapshot l t s) = Map.foldlWithKey' (\acc k v -> scrapeDatapointToPoint l t k v : acc) [] s

snapshotsToFlatStore :: [Snapshot] -> Flat Double
snapshotsToFlatStore = (>>= snapshotToFlatStore)
