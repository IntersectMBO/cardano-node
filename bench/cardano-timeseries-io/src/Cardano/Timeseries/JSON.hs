{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | JSON serialisation instances for timeseries query results.
--   The format closely follows the Prometheus HTTP API: result types are named
--   \"scalar\", \"vector\", \"matrix\"; timestamps are Unix seconds (Double);
--   durations are seconds (Double).
--   Import this module to bring the instances into scope; it exports nothing itself.
module Cardano.Timeseries.JSON () where

import qualified Data.Aeson         as A
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import qualified Data.Vector        as V

import           Cardano.Timeseries.AsText (showT)

import           Data.Text          (Text)

import           Cardano.Timeseries.Domain.Types        (SeriesIdentifier)
import           Cardano.Timeseries.Domain.Instant      (Instant (..))
import           Cardano.Timeseries.Domain.Timeseries   (Timeseries (..))
import           Cardano.Timeseries.Interp.Value        (Value (..))

-- SeriesIdentifier = Set (Label, Text); serialise as a JSON object {"key": "val", ...}
-- We do not define ToJSON SeriesIdentifier directly (it would overlap with aeson's Set instance),
-- so we convert via Map first using the helper below.
labelsJson :: SeriesIdentifier -> A.Value
labelsJson si = A.toJSON (Map.fromList (Set.toList si))

instance (A.ToJSON a, Show a) => A.ToJSON (Instant a) where
  toJSON (Instant ls ts val) = A.object
    [ "metric" A..= labelsJson ls
    , "value"  A..= A.Array (V.fromList [A.toJSON (fromIntegral ts / 1000.0 :: Double), A.String (showT val)])
    ]

instance (A.ToJSON a, Show a) => A.ToJSON (Timeseries a) where
  toJSON (Timeseries ls ps) = A.object
    [ "metric" A..= labelsJson ls
    , "values" A..= map (\(t, v) -> A.Array (V.fromList [A.toJSON (fromIntegral t / 1000.0 :: Double), A.String (showT v)])) ps
    ]

instance A.ToJSON Value where
  toJSON (Scalar x)         = A.object ["resultType" A..= ("scalar"    :: Text), "result" A..= x]
  toJSON (RangeVector xs)   = A.object ["resultType" A..= ("matrix"    :: Text), "result" A..= xs]
  toJSON (InstantVector xs) = A.object ["resultType" A..= ("vector"    :: Text), "result" A..= xs]
  toJSON (Pair a b)         = A.object ["resultType" A..= ("pair"      :: Text), "fst"    A..= a, "snd" A..= b]
  toJSON Unit               = A.object ["resultType" A..= ("unit"      :: Text)]
  toJSON Truth              = A.object ["resultType" A..= ("truth"     :: Text)]
  toJSON Falsity            = A.object ["resultType" A..= ("falsity"   :: Text)]
  toJSON (Duration d)       = A.object ["resultType" A..= ("duration"  :: Text), "result" A..= (fromIntegral d / 1000.0 :: Double)]
  toJSON (Timestamp t)      = A.object ["resultType" A..= ("timestamp" :: Text), "result" A..= (fromIntegral t / 1000.0 :: Double)]
  toJSON (Text t)           = A.object ["resultType" A..= ("text"      :: Text), "result" A..= t]
  toJSON (Function _)       = A.object ["resultType" A..= ("function"  :: Text)]
  toJSON Nil                = A.object ["resultType" A..= ("nil"       :: Text)]
  toJSON (Cons h t)         = A.object ["resultType" A..= ("cons"      :: Text), "head" A..= h, "tail" A..= t]
