{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Logging.Resources.Types
    ( Resources(..)
    , ResourceStats
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
      , rNetRd :: !a
      , rNetWr :: !a
      , rFsRd  :: !a
      , rFsWr  :: !a
      , rThreads    :: !a
      }
  deriving (Functor, Generic, Show)

instance Applicative Resources where
  pure a = Resources a a a a a a a a a a a a a a a
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
    , rNetRd = rNetRd f (rNetRd x)
    , rNetWr = rNetWr f (rNetWr x)
    , rFsRd  = rFsRd  f (rFsRd  x)
    , rFsWr  = rFsWr  f (rFsWr  x)
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

instance LogFormatting ResourceStats where
    forHuman Resources{..} = "Resources:"
                  <>  " Cpu Ticks "            <> (pack . show) rCentiCpu
                  <> ", GC centiseconds "      <> (pack . show) rCentiGC
                  <> ", Mutator centiseconds " <> (pack . show) rCentiMut
                  <> ", GCs major "            <> (pack . show) rGcsMajor
                  <> ", GCs minor "            <> (pack . show) rGcsMinor
                  <> ", Allocated bytes "      <> (pack . show) rAlloc
                  <>" , GC live bytes "        <> (pack . show) rLive
                  <> ", RTS heap "             <> (pack . show) rHeap
                  <> ", RSS "                  <> (pack . show) rRSS
                  <> ", Net bytes read "       <> (pack . show) rNetRd
                  <> " written "               <> (pack . show) rNetWr
                  <> ", FS bytes read "        <> (pack . show) rFsRd
                  <> " written "               <> (pack . show) rFsWr
                  <> ", Threads "              <> (pack . show) rThreads
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
      , "NetRd"    .= Number (fromIntegral $ rNetRd rs)
      , "NetWr"    .= Number (fromIntegral $ rNetWr rs)
      , "FsRd"     .= Number (fromIntegral $ rFsRd rs)
      , "FsWr"     .= Number (fromIntegral $ rFsWr rs)
      , "Threads"       .= Number (fromIntegral $ rThreads rs)
      ]

    asMetrics rs =
      [ IntM "Resources.Stat.Cputicks"    (fromIntegral $ rCentiCpu rs)
      , IntM "Resources.RTS.Gcticks"      (fromIntegral $ rCentiGC rs)
      , IntM "Resources.RTS.Mutticks"     (fromIntegral $ rCentiMut rs)
      , IntM "Resources.RTS.GcMajorNum"   (fromIntegral $ rGcsMajor rs)
      , IntM "Resources.RTS.GcMinorNum"   (fromIntegral $ rGcsMinor rs)
      , IntM "Resources.RTS.Alloc"        (fromIntegral $ rAlloc rs)
      , IntM "Resources.RTS.GcLiveBytes"  (fromIntegral $ rLive rs)
      , IntM "Resources.RTS.Heap"         (fromIntegral $ rHeap rs)
      , IntM "Resources.Mem.Resident"     (fromIntegral $ rRSS rs)
      , IntM "Resources.Stat.BlkIOticks"  (fromIntegral $ rCentiBlkIO rs)
      , IntM "Resources.State.NetRd"      (fromIntegral $ rNetRd rs)
      , IntM "Resources.State.NetWr"      (fromIntegral $ rNetWr rs)
      , IntM "Resources.State.FsRd"       (fromIntegral $ rFsRd rs)
      , IntM "Resources.State.FsWr"       (fromIntegral $ rFsWr rs)
      , IntM "Resources.RTS.Stat.Threads" (fromIntegral $ rThreads rs)
      ]

instance MetaTrace ResourceStats where
  namespaceFor Resources {} =
    Namespace [] ["Resources"]
  severityFor  (Namespace _ ["Resources"]) _ = Just Info
  severityFor _ns _ = Nothing
  documentFor  (Namespace _ ["Resources"]) = Just ""
  documentFor _ns = Nothing
  metricsDocFor  (Namespace _ ["Resources"]) =
    [("Resources.Stat.Cputicks", "Kernel-reported CPU ticks (1/100th of a second), since process start")
    ,("Resources.Mem.Resident", "Kernel-reported RSS (resident set size)")
    ,("Resources.RTS.GcLiveBytes", "RTS-reported live bytes")
    ,("Resources.RTS.GcMajorNum", "Major GCs")
    ,("Resources.RTS.GcMinorNum", "Minor GCs")
    ,("Resources.RTS.Gcticks", "RTS-reported CPU ticks spent on GC")
    ,("Resources.RTS.Mutticks", "RTS-reported CPU ticks spent on mutator")
    ,("Resources.State.NetRd", "IP packet bytes read")
    ,("Resources.State.NetWr", "IP packet bytes written")
    ,("Resources.State.FsRd", "FS bytes read")
    ,("Resources.State.FsWr", "FS bytes written")
    ,("Resources.RTS.Threads","RTS green thread count")
    ]
  metricsDocFor _ns = []
  allNamespaces = [ Namespace [] ["Resources"]]

