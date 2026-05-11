{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | JSON serialisation instances for timeseries query results.
--   Import this module to bring the instances into scope; it exports nothing itself.
module Cardano.Timeseries.JSON () where

import qualified Data.Aeson         as A
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import qualified Data.Vector        as V

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

instance A.ToJSON a => A.ToJSON (Instant a) where
  toJSON (Instant ls ts val) = A.object
    [ "labels"    A..= labelsJson ls
    , "timestamp" A..= ts
    , "value"     A..= val
    ]

instance A.ToJSON a => A.ToJSON (Timeseries a) where
  toJSON (Timeseries ls ps) = A.object
    [ "labels" A..= labelsJson ls
    , "data"   A..= map (\(t, v) -> A.Array (V.fromList [A.toJSON t, A.toJSON v])) ps
    ]

instance A.ToJSON Value where
  toJSON (Scalar x)         = A.object ["tag" A..= ("Scalar"        :: Text), "value" A..= x]
  toJSON (RangeVector xs)   = A.object ["tag" A..= ("RangeVector"   :: Text), "value" A..= xs]
  toJSON (InstantVector xs) = A.object ["tag" A..= ("InstantVector" :: Text), "value" A..= xs]
  toJSON (Pair a b)         = A.object ["tag" A..= ("Pair"          :: Text), "fst"   A..= a,  "snd" A..= b]
  toJSON Unit               = A.object ["tag" A..= ("Unit"          :: Text)]
  toJSON Truth              = A.object ["tag" A..= ("Truth"         :: Text)]
  toJSON Falsity            = A.object ["tag" A..= ("Falsity"       :: Text)]
  toJSON (Duration d)       = A.object ["tag" A..= ("Duration"      :: Text), "value" A..= d]
  toJSON (Timestamp t)      = A.object ["tag" A..= ("Timestamp"     :: Text), "value" A..= t]
  toJSON (Text t)           = A.object ["tag" A..= ("Text"          :: Text), "value" A..= t]
  toJSON (Function _)       = A.object ["tag" A..= ("Function"      :: Text)]
  toJSON Nil                = A.object ["tag" A..= ("Nil"           :: Text)]
  toJSON (Cons h t)         = A.object ["tag" A..= ("Cons"          :: Text), "head" A..= h, "tail" A..= t]
