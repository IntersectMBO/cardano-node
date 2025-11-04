{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans #-}

module Cardano.Timeseries.Import.PlainCBOR where

import           Codec.Serialise
import           Control.Applicative
import           Data.Map.Strict as Map (Map, size)
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
  show (Snapshot l t s) = "Snapshot{" ++ unpack l ++ "} @ " ++ show t ++ ", entries: " ++ show (Map.size s)


instance Serialise POSIXTime where
  encode = encode . toInteger . floor
  decode = fromInteger <$> decode


readFileSnapshots :: FilePath -> IO [Snapshot]
readFileSnapshots = readFileDeserialise

-- can be used with Data.List.sortBy
snapshotOrd :: Snapshot -> Snapshot -> Ordering
snapshotOrd a b =
     singletonLabel a `compare` singletonLabel b
  <> timeStamp a `compare` timeStamp b
