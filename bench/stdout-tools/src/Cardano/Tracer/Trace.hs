{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- All traces start with the a JSON object having an "at" and an "ns" property,
-- use this assumption to build a "fast" decoder.
-- {"at":"2024-03-30T00:30:27.015631111Z","ns":"Reflection.TracerInfo",...}
--------------------------------------------------------------------------------

module Cardano.Tracer.Trace
  ( Trace (Trace, at, ns, remainder)
  , fromJson

  , Remainder (remainderData, sev, thread, host)
  , DataWithSlot (slot)
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
-- Use this two properties to filter and if you need some more data, decode the
-- the remainder.
-- Keep the same order commonly used for traces!
data Trace = Trace
  -- Strict or keep thunks of `Data.Time.FromText.parseUTCTime` ???
  { at :: UTCTime
  , ns :: Text.Text
    -- Only do `fromJSON` if needed!
  , remainder :: Text.Text
  }
  deriving (Eq, Show)

-- Fast & Ugly, Ugly & Fast.
-- Too many assumptions (assumption is the parent of all thing that did not go
-- quite as expected) but here we pretend that we don't care.
fromJson :: Text.Text -> Either Text.Text Trace
fromJson text =
  -- Assume '{"at":"'' is there
  case Text.splitAt 7 text of
    -- Property 'at' assumed as expected.
    ("{\"at\":\"", text') ->
           -- Assume a date like '2024-04-11T12:01:33.2135764Z' is there.
           -- The milliseconds part is variable, can't read a fix amount.
      let (atText, text'' ) = Text.break (== '"') text'
          -- Consume all the text until the next '"'.
          -- First drop the date's last '"' ans assume ',"ns":"' is there.
          (nsText, text''') = Text.break (== '"') (Text.drop 8 text'')
      --in Left $ show (atText, text'', nsText, text''')
      in  case ParseTime.parseUTCTime atText of
            (Left err) -> Left $ Text.pack $ "parseUTCTime: " ++ err
            (Right utcTime) -> Right $
              -- Drop closing '",' of 'ns' and leave unconsumed as a new object.
              Trace utcTime nsText ("{" <> (Text.drop 2 text'''))
    _ -> Left "No {\"at\":\""

--------------------------------------------------------------------------------

-- Keep the same order commonly used for traces!
data Remainder a = Remainder
  { remainderData :: a
  , sev           :: Text.Text
  , thread        :: Text.Text
  , host          :: Text.Text
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (Remainder a) where
  -- Only using a non-automatic instance because of "data" and "msgData".
  toJSON p@(Remainder _ _ _ _) =
    Aeson.object
      [ "data"   Aeson..= remainderData p
      , "sev"    Aeson..= sev           p
      , "thread" Aeson..= thread        p
      , "host"   Aeson..= host          p
      ]

instance Aeson.FromJSON a => Aeson.FromJSON (Remainder a) where
  -- Only using a non-automatic instance because of "data" and "msgData".
  parseJSON =
    Aeson.withObject "Remainder" $ \o -> do
      Remainder
        <$> o Aeson..: "data"
        <*> o Aeson..: "sev"
        <*> o Aeson..: "thread"
        <*> o Aeson..: "host"

--------------------------------------------------------------------------------

data DataWithSlot = DataWithSlot
  { slot :: Integer }
  deriving Generic

instance Aeson.ToJSON DataWithSlot where

instance Aeson.FromJSON DataWithSlot where

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
