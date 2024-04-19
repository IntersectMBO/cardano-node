{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- All traces start with the a JSON object having an "at" and an "ns" property,
-- use this assumption to build a provably correct "fast" decoder.
-- {"at":"2024-03-30T00:30:27.015631111Z","ns":"Reflection.TracerInfo",...}
--------------------------------------------------------------------------------

module Cardano.Tracer.Trace
  ( Trace (Trace, at, ns, remainder)
  , fromJson

  , Remainder (remainderData, sev, thread, host)
  , DataWithSlot (slot), DataWithUtxoSize (utxoSize)
  , DataResources (..)
  ) where

--------------------------------------------------------------------------------

-- base.
import           GHC.Generics

-- package: time.
import           Data.Time.Clock (UTCTime)
-- package: text.
import qualified Data.Text as Text
-- package: text-iso8601-0.1
import qualified Data.Time.FromText as ParseTime
-- package: aeson.
import qualified Data.Aeson as Aeson

-- A Cardano tracing system message.
--------------------------------------------------------------------------------

-- Keep it simple!
-- Keep the same order commonly used for traces!
-- Use `ns` and `at` to filter and only decode the `remainder` if needed.
data Trace = Trace
  -- Keep it non-strict, avoids executing the parser until requested.
  { at :: Either String UTCTime
  -- The namespace is the fastest way of filtering messages, use it first
  , ns :: Text.Text
    -- Only do `fromJSON` if needed, `Aeson` decoding is costly!
  , remainder :: Text.Text
  }
  deriving (Eq, Show)

-- Fast & Ugly, Ugly & Fast.
-- Too many assumptions (assumption is the parent of all thing that did not go
-- quite as expected) but here we pretend that we don't care.
fromJson :: Text.Text -> Either Text.Text Trace
fromJson text =
  -- Look for '{"at":"''
  case Text.splitAt 7 text of -- No cost center for the unavoidable simple part!
    -- Property 'at' found, please assume a valid trace message!
    ("{\"at\":\"", text') ->
          -- Assume a date like '2024-04-11T12:01:33.2135764Z' is next.
          -- The milliseconds part is variable so we can't read a fixed amount.
          -- TODO: Can we make it of a fixed number of decimals ???
      let (atText, text'' ) = {-# SCC "fromJson_break_at" #-}
                              Text.break (== '"') text' -- Until next '"'.
          -- If this fails it's provably not a valid trace message.
          parseTimeOutput = {-# SCC "fromJson_parseUTCTime" #-}
                            ParseTime.parseUTCTime atText
              -- Drop the date's last '"' and assume ',"ns":"' is there.
          text''' = {-# SCC "fromJson_drop_ns" #-}
                    Text.drop 8 text'' -- Drops '","ns":"'.
          -- Consume all the text until the next '"'.
          (nsText, text'''') = {-# SCC "fromJson_break_ns" #-}
                               Text.break (== '"') text''' -- Until next '"'.
          -- Drop closing '",' of 'ns' and leave the unconsumed Text, the
          -- `Remainder`, as a new JSON object.
          remainderText = {-# SCC "fromJson_remainder" #-}
                          "{" <> Text.drop 2 text''''
      in Right $ Trace parseTimeOutput nsText remainderText
    -- Assumption failed.
    _ -> Left text

--------------------------------------------------------------------------------

-- Keep the same order commonly used for trace messages!
data Remainder a = Remainder
  { remainderData :: a
  , sev           :: Text.Text
  , thread        :: Text.Text
  , host          :: Text.Text
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (Remainder a) where
  -- Only using a non-automatic instance because of "data" and "remainderData".
  toJSON p = {-# SCC "Remainder_toJSON" #-}
    Aeson.object
      [ "data"   Aeson..= remainderData p
      , "sev"    Aeson..= sev           p
      , "thread" Aeson..= thread        p
      , "host"   Aeson..= host          p
      ]

instance Aeson.FromJSON a => Aeson.FromJSON (Remainder a) where
  -- Only using a non-automatic instance because of "data" and "remainderData".
  parseJSON = {-# SCC "Remainder_parseJSON" #-}
    Aeson.withObject "Remainder" $ \o -> do
      Remainder
        <$> o Aeson..: "data"
        <*> o Aeson..: "sev"
        <*> o Aeson..: "thread"
        <*> o Aeson..: "host"

--------------------------------------------------------------------------------

-- Helper for when you only need the slot number.
-- { at:..., ns:"", data:{ ..., slot:0, ... } }
newtype DataWithSlot = DataWithSlot
  { slot :: Integer }
  deriving Generic

instance Aeson.ToJSON DataWithSlot where

instance Aeson.FromJSON DataWithSlot where

-- Helper for when you only need the utxoSize number.
-- { at:..., ns:"", data:{ ..., utxoSize:0, ... } }
newtype DataWithUtxoSize = DataWithUtxoSize
  { utxoSize :: Integer }
  deriving Generic

instance Aeson.ToJSON DataWithUtxoSize where

instance Aeson.FromJSON DataWithUtxoSize where

--------------------------------------------------------------------------------

{--
  "data": {
    "Alloc": 98169147912,
    "CentiBlkIO": 0,
    "CentiCpu": 10831,
    "CentiGC": 4726,
    "CentiMut": 6104,
    "FsRd": 0,
    "FsWr": 8192,
    "GcsMajor": 7,
    "GcsMinor": 3590,
    "Heap": 8629780480,
    "Live": 2529171488,
    "NetRd": 0,
    "NetWr": 0,
    "RSS": 8683200512,
    "Threads": 9,
    "kind": "ResourceStats"
  },
--}
data DataResources = DataResources
  { resourcesAlloc :: Integer
  , resourcesCentiBlkIO :: Integer
  , resourcesCentiCpu :: Integer
  , resourcesCentiGC :: Integer
  , resourcesCentiMut :: Integer
  , resourcesFsRd :: Integer
  , resourcesGcsMajor :: Integer
  , resourcesGcsMinor :: Integer
  , resourcesHeap :: Integer
  , resourcesLive :: Integer
  , resourcesNetRd :: Integer
  , resourcesNetWr :: Integer
  , resourcesRSS :: Integer
  , resourcesThreads :: Integer
  }
  deriving (Eq, Show, Generic)

msgCustomOptions :: Aeson.Options
msgCustomOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = drop 9
  , Aeson.constructorTagModifier = id
  }

instance Aeson.ToJSON DataResources where
  toJSON = Aeson.genericToJSON msgCustomOptions
  toEncoding = Aeson.genericToEncoding msgCustomOptions

instance Aeson.FromJSON DataResources where
  parseJSON = Aeson.genericParseJSON msgCustomOptions
