{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Every hermod trace the node constructs is declared exactly once, in
-- 'Cardano.Node.Tracing.Tracers.buildNodeTracers', as a call to one of the
-- constructors below. A constructor builds the trace and records it, so one
-- declaration serves the runtime (configuration), the trace documentation and
-- the configuration consistency check.
module Cardano.Node.Tracing.Registry
  ( Registry
  , Backends (..)
  , Purpose (..)
  , newRegistry
  , registered
  , regBackends
    -- * Declaring traces
  , Listing (..)
  , standard
  , withoutMetrics
  , undocumented
  , runtimeOnly
  , newTrace
  , newTraceWith
  , newDataPoint
  , newDataPointWith
  , docOnly
    -- * Using the declarations
  , Entry (..)
  , SomeEntry (..)
  , configureAll
  , documentAll
  , namespacesOf
  , checkAll
    -- * Applying a configuration
  , ApplyTraceConfig (..)
  , applier
  ) where

import           Control.DeepSeq (NFData)
import           Control.Monad (forM_, unless, void, when)
import           Data.Aeson (ToJSON)
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import           Data.List (foldl')
import           Data.Text (Text)

import           Hermod.Tracing
import           Hermod.Tracing.DocuGenerator (DocTracer (..), documentTracer)

-- | The backends every trace is built on.
data Backends = Backends
  { bkStdout    :: Trace IO FormattedMessage
  , bkForward   :: Trace IO FormattedMessage
  , bkEKG       :: Maybe (Trace IO FormattedMessage)
  , bkDataPoint :: Trace IO DataPoint
  }

-- | Entries that exist only for the documentation are not built at runtime.
data Purpose = ForRuntime | ForDocumentation
  deriving Eq

-- | How an entry takes part in the three consumers.
data Listing = Listing
  { withMetrics :: Bool  -- ^ attach the EKG backend
  , documented  :: Bool  -- ^ part of the trace documentation
  , checked     :: Bool  -- ^ part of the configuration consistency check
  }

-- | 'undocumented' is for traces whose 'MetaTrace' instance would make the
-- documentation warn; 'runtimeOnly' for traces the check cannot take either.
standard, withoutMetrics, undocumented, runtimeOnly :: Listing
standard = Listing { withMetrics = True, documented = True, checked = True }
withoutMetrics = standard { withMetrics = False }
undocumented = standard { documented = False }
runtimeOnly = standard { documented = False, checked = False }

data Entry a = Entry
  { entryPrefix  :: [Text]
  , entryTrace   :: Trace IO a
  , entryListing :: Listing
  }

data SomeEntry where
  SomeEntry :: MetaTrace a => Entry a -> SomeEntry

data Registry = Registry
  { regPurpose  :: Purpose
  , regBackends :: Backends
  , regEntries  :: IORef [SomeEntry]
  }

newRegistry :: Purpose -> Backends -> IO Registry
newRegistry purpose bk = Registry purpose bk <$> newIORef []

-- | The entries, in the order they were declared.
registered :: Registry -> IO [SomeEntry]
registered reg = reverse <$> readIORef (regEntries reg)

record :: MetaTrace a => Registry -> Entry a -> IO ()
record reg e = modifyIORef' (regEntries reg) (SomeEntry e :)

newTrace :: (LogFormatting a, MetaTrace a) => Registry -> [Text] -> IO (Trace IO a)
newTrace = newTraceWith standard

newTraceWith :: (LogFormatting a, MetaTrace a)
  => Listing -> Registry -> [Text] -> IO (Trace IO a)
newTraceWith listing reg prefix = do
  let bk = regBackends reg
      ekg = if withMetrics listing then bkEKG bk else Nothing
  !tr <- mkHermodTracer (bkStdout bk) (bkForward bk) ekg prefix
  record reg (Entry prefix tr listing)
  pure tr

newDataPoint :: (ToJSON a, MetaTrace a, NFData a) => Registry -> IO (Trace IO a)
newDataPoint = newDataPointWith standard

-- | Data points carry no prefix: 'mkDataPointTracer' names them by their inner
-- namespaces.
newDataPointWith :: (ToJSON a, MetaTrace a, NFData a)
  => Listing -> Registry -> IO (Trace IO a)
newDataPointWith listing reg = do
  !tr <- mkDataPointTracer (bkDataPoint (regBackends reg))
  record reg (Entry [] tr listing)
  pure tr

-- | A namespace that is documented and checked but has no runtime trace.
docOnly :: forall a. (LogFormatting a, MetaTrace a) => Registry -> [Text] -> IO ()
docOnly reg prefix =
  when (regPurpose reg == ForDocumentation) $
    void (newTrace reg prefix :: IO (Trace IO a))

configureAll :: ConfigReflection -> TraceConfig -> [SomeEntry] -> IO ()
configureAll configReflection trConfig es =
  forM_ es $ \(SomeEntry e) -> configureTracers configReflection trConfig [entryTrace e]

-- | Applies a trace configuration to every trace the node declared. The same
-- action serves the configuration applied at start-up and the one re-read on
-- SIGHUP, so the two cannot drift apart.
data ApplyTraceConfig = ApplyTraceConfig
  { -- | Configure every declared trace, then report the reflection, the
    -- consistency warnings and the effective configuration. Returns the
    -- warnings so the caller can mention them.
    applyTraceConfig   :: TraceConfig -> IO NSWarnings
    -- | Like 'applyTraceConfig', but reports only the effective
    -- configuration. For putting the previous configuration back after a
    -- failed apply, where the reflection and the warnings are noise.
  , restoreTraceConfig :: TraceConfig -> IO ()
  }

-- | 'registered' is a plain read, so the entries can be taken once and
-- configured any number of times afterwards.
applier :: Registry -> IO ApplyTraceConfig
applier reg = do
    entries <- registered reg
    let Backends{bkStdout, bkForward} = regBackends reg

        -- A fresh reflection per pass: 'traceTracerInfo' empties the one it
        -- reports, so a reflection cannot be used twice.
        configure trConfig = do
          configReflection <- emptyConfigReflection
          configureAll configReflection trConfig entries
          pure configReflection

    pure ApplyTraceConfig
      { applyTraceConfig = \trConfig -> do
          configReflection <- configure trConfig
          traceTracerInfo bkStdout bkForward configReflection
          let warnings = checkAll trConfig entries
          unless (null warnings) $
            traceConfigWarnings bkStdout bkForward warnings
          traceEffectiveConfiguration bkStdout bkForward trConfig
          pure warnings
      , restoreTraceConfig = \trConfig -> do
          _ <- configure trConfig
          traceEffectiveConfiguration bkStdout bkForward trConfig
      }

-- | Call after 'configureAll': an unconfigured trace cannot be documented.
documentAll :: [SomeEntry] -> IO DocTracer
documentAll es =
  foldl' (<>) noDocs <$> sequence
    [documentTracer (entryTrace e) | SomeEntry e <- es, documented (entryListing e)]
  where
    noDocs = DocTracer
      { dtTracerNames = []
      , dtSilent      = []
      , dtNoMetrics   = []
      , dtBuilderList = []
      , dtWarnings    = []
      }

-- | Every namespace of the checked entries, as (prefix, inner) pairs.
namespacesOf :: [SomeEntry] -> [([Text], [Text])]
namespacesOf es =
  concat [entryNamespaces e | SomeEntry e <- es, checked (entryListing e)]
  where
    entryNamespaces :: forall a. MetaTrace a => Entry a -> [([Text], [Text])]
    entryNamespaces e =
      map (nsGetTuple . nsReplacePrefix (entryPrefix e)) (allNamespaces :: [Namespace a])

checkAll :: TraceConfig -> [SomeEntry] -> NSWarnings
checkAll trConfig = checkTraceConfiguration' trConfig . namespacesOf
