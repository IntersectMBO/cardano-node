{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Logging.Resources.Types
    ( Resources(..)
    , ResourceStats
    , docResourceStats
    ) where


import           Cardano.Logging.Types
import           Data.Aeson
import           Data.Text (pack)
import           Data.Word
import           GHC.Generics (Generic)

-- | Struct for resources used by the process
type ResourceStats = Resources Word64

-- * HKD for resources used by the process.
--
data Resources a
  = Resources
      { rCentiCpu   :: !a
      , rCentiGC    :: !a
      , rCentiMut   :: !a
      , rGcsMajor   :: !a
      , rGcsMinor   :: !a
      , rAlloc      :: !a
      , rLive       :: !a
      , rHeap       :: !a
      , rRSS        :: !a
      , rCentiBlkIO :: !a
      , rThreads    :: !a
      }
  deriving (Functor, Generic, Show)

instance Applicative Resources where
  pure a = Resources a a a a a a a a a a a
  f <*> x =
    Resources
    { rCentiCpu   = rCentiCpu   f (rCentiCpu   x)
    , rCentiGC    = rCentiGC    f (rCentiGC    x)
    , rCentiMut   = rCentiMut   f (rCentiMut   x)
    , rGcsMajor   = rGcsMajor   f (rGcsMajor   x)
    , rGcsMinor   = rGcsMinor   f (rGcsMinor   x)
    , rAlloc      = rAlloc      f (rAlloc      x)
    , rLive       = rLive       f (rLive       x)
    , rHeap       = rHeap       f (rHeap       x)
    , rRSS        = rRSS        f (rRSS        x)
    , rCentiBlkIO = rCentiBlkIO f (rCentiBlkIO x)
    , rThreads    = rThreads    f (rThreads    x)
    }

instance FromJSON a => FromJSON (Resources a) where
  parseJSON = genericParseJSON jsonEncodingOptions

instance ToJSON a => ToJSON (Resources a) where
  toJSON = genericToJSON jsonEncodingOptions
  toEncoding = genericToEncoding jsonEncodingOptions

jsonEncodingOptions :: Options
jsonEncodingOptions = defaultOptions
  { fieldLabelModifier     = drop 1
  , tagSingleConstructors  = True
  , sumEncoding =
    TaggedObject
    { tagFieldName = "kind"
    , contentsFieldName = "contents"
    }
  }

docResourceStats :: Documented ResourceStats
docResourceStats = Documented [
      DocMsg
        (pure 0)
        [("Stat.Cputicks", "Reports the CPU ticks, sice the process was started")
        ,("Mem.Resident", "TODO JNF")
        ,("RTS.GcLiveBytes", "TODO JNF")
        ,("RTS.GcMajorNum", "TODO JNF")
        ,("RTS.GcMinorNum", "TODO JNF")
        ,("RTS.Gcticks", "TODO JNF")
        ,("RTS.Mutticks", "TODO JNF")
        ,("RTS.Threads","TODO JNF")
        ]
        "TODO JNF"
    ]

instance LogFormatting ResourceStats where
    forHuman rs = "Resources: CpuTicks " <> (pack . show) (rCentiCpu rs)
                  <> ", Resident " <> (pack . show) (rRSS rs)
                  <> ", GcLiveBytes " <> (pack . show) (rLive rs)
                  <> ", GcMajorNum " <> (pack . show) (rGcsMajor rs)
                  <> ", GcMinorNum " <> (pack . show) (rGcsMinor rs)
                  <> ", Gcticks " <> (pack . show) (rCentiGC rs)
                  <> ", Mutticks " <> (pack . show) (rCentiMut rs)
                  <> ", Threads " <> (pack . show) (rThreads rs)
                  <> "."

    forMachine _dtal rs = mkObject
      [ "kind"          .= String "ResourceStats"
      , "Cputicks"      .= Number (fromIntegral $ rCentiCpu rs)
      , "Resident"      .= Number (fromIntegral $ rRSS rs)
      , "GcLiveBytes"   .= Number (fromIntegral $ rLive rs)
      , "GcMajorNum"    .= Number (fromIntegral $ rGcsMajor rs)
      , "GcMinorNum"    .= Number (fromIntegral $ rGcsMinor rs)
      , "Gcticks"       .= Number (fromIntegral $ rCentiGC rs)
      , "Mutticks"      .= Number (fromIntegral $ rCentiMut rs)
      , "Threads"       .= Number (fromIntegral $ rThreads rs)
      ]

    asMetrics rs =
      [ IntM "Stat.Cputicks" (fromIntegral $ rCentiCpu rs)
      , IntM "Mem.Resident" (fromIntegral $ rRSS rs)
      , IntM "RTS.GcLiveBytes" (fromIntegral $ rLive rs)
      , IntM "RTS.GcMajorNum" (fromIntegral $ rGcsMajor rs)
      , IntM "RTS.GcMinorNum" (fromIntegral $ rGcsMinor rs)
      , IntM "RTS.Gcticks" (fromIntegral $ rCentiGC rs)
      , IntM "RTS.Mutticks" (fromIntegral $ rCentiMut rs)
      , IntM "Stat.Threads" (fromIntegral $ rThreads rs)
      ]
