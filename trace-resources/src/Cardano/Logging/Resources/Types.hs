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
        anyProto
        [("stat.cputicks", "Reports the CPU ticks, sice the process was started")
        ,("mem.resident", "TODO JNF")
        ,("rts.gcLiveBytes", "TODO JNF")
        ,("rts.gcMajorNum", "TODO JNF")
        ,("rts.gcMinorNum", "TODO JNF")
        ,("rts.gcticks", "TODO JNF")
        ,("rts.mutticks", "TODO JNF")
        ,("rts.threads","TODO JNF")
        ]
        "TODO JNF"
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
      [ IntM "stat.cputicks" (fromIntegral $ rCentiCpu rs)
      , IntM "mem.resident" (fromIntegral $ rRSS rs)
      , IntM "rts.gcLiveBytes" (fromIntegral $ rLive rs)
      , IntM "rts.gcMajorNum" (fromIntegral $ rGcsMajor rs)
      , IntM "rts.gcMinorNum" (fromIntegral $ rGcsMinor rs)
      , IntM "rts.gcticks" (fromIntegral $ rCentiGC rs)
      , IntM "rts.mutticks" (fromIntegral $ rCentiMut rs)
      , IntM "rts.stat.threads" (fromIntegral $ rThreads rs)
      ]
