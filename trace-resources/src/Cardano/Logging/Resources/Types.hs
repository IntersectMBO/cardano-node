{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Logging.Resources.Types
    ( Resources(..)
    , ResourceStats
    , docResourceStats
    ) where


import           Cardano.Logging
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
        []
        [("Resources.Stat.Cputicks", "Reports the CPU ticks, sice the process was started")
        ,("Resources.Mem.Resident", "")
        ,("Resources.RTS.GcLiveBytes", "")
        ,("Resources.RTS.GcMajorNum", "")
        ,("Resources.RTS.GcMinorNum", "")
        ,("Resources.RTS.Gcticks", "")
        ,("Resources.RTS.Mutticks", "")
        ,("Resources.RTS.Threads","")
        ]
        ""
    ]

instance LogFormatting ResourceStats where
    forHuman rs = "Resources:"
                  <>  " Cpu Ticks "            <> (pack . show) (rCentiCpu rs)
                  <> ", GC centiseconds "      <> (pack . show) (rCentiGC rs)
                  <> ", Mutator centiseconds " <> (pack . show) (rCentiMut rs)
                  <> ", GCs major "            <> (pack . show) (rGcsMajor rs)
                  <> ", GCs minor "            <> (pack . show) (rGcsMinor rs)
                  <> ", Allocated bytes "      <> (pack . show) (rAlloc rs)
                  <>" , GC live bytes "        <> (pack . show) (rLive rs)
                  <> ", RTS heap "             <> (pack . show) (rHeap rs)
                  <> ", RSS "                  <> (pack . show) (rRSS rs)
                  <> ", Threads "              <> (pack . show) (rThreads rs)
                  <> "."

    forMachine _dtal rs = mconcat
      [ "kind"          .= String "ResourceStats"
      , "CentiCpu"      .= Number (fromIntegral $ rCentiCpu rs)
      , "CentiGC"       .= Number (fromIntegral $ rCentiGC rs)
      , "CentiMut"      .= Number (fromIntegral $ rCentiMut rs)
      , "GcsMajor"      .= Number (fromIntegral $ rGcsMajor rs)
      , "GcsMinor"      .= Number (fromIntegral $ rGcsMinor rs)
      , "Alloc"         .= Number (fromIntegral $ rAlloc rs)
      , "Live"          .= Number (fromIntegral $ rLive rs)
      , "Heap"          .= Number (fromIntegral $ rHeap rs)
      , "RSS"           .= Number (fromIntegral $ rRSS rs)
      , "CentiBlkIO"    .= Number (fromIntegral $ rCentiBlkIO rs)
      , "Threads"       .= Number (fromIntegral $ rThreads rs)
      ]

    asMetrics rs =
      [ IntM "Resources.Stat.Cputicks" (fromIntegral $ rCentiCpu rs)
      , IntM "Resources.mem.Resident" (fromIntegral $ rRSS rs)
      , IntM "Resources.RTS.GcLiveBytes" (fromIntegral $ rLive rs)
      , IntM "Resources.RTS.GcMajorNum" (fromIntegral $ rGcsMajor rs)
      , IntM "Resources.RTS.GcMinorNum" (fromIntegral $ rGcsMinor rs)
      , IntM "Resources.RTS.Gcticks" (fromIntegral $ rCentiGC rs)
      , IntM "Resources.RTS.Mutticks" (fromIntegral $ rCentiMut rs)
      , IntM "Resources.RTS.Stat.Threads" (fromIntegral $ rThreads rs)
      ]
