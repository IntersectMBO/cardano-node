{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
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
  ) where

import           Control.DeepSeq (NFData)
import           Control.Monad (forM_, void, when)
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
