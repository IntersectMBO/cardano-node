{-# LANGUAGE RankNTypes #-}

module Cardano.Node.TUI.Run
    ( LiveViewBackend (..)
    , realize
    , effectuate
    , captureCounters
    , liveViewPostSetup
    , setNodeThread
    , storePeersInLiveView
    ) where

import           Cardano.Prelude hiding (modifyMVar_, newMVar, on, readMVar, show)
import           Prelude (show)

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar.Strict (modifyMVar_)

import qualified Data.Text as Text

import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem (LOContent (..), PrivacyAnnotation (Confidential),
                     mkLOMeta)
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Trace
import           Cardano.Node.TUI.Drawing (LiveViewState (..), LiveViewThread (..))
import           Cardano.Node.TUI.EventHandler (LiveViewBackend (..))
import           Cardano.Tracing.Peer (Peer (..))

import           Cardano.Node.Configuration.POM (NodeConfiguration (..), ncProtocol)
import           Cardano.Node.Types

-- | Change a few fields in the LiveViewState after it has been initialized above.
liveViewPostSetup :: NFData a => LiveViewBackend blk a -> NodeConfiguration-> IO ()
liveViewPostSetup lvbe nc = do
    modifyMVar_ (getbe lvbe) $ \lvs ->
      pure lvs
            { lvsNodeId = nodeId
            , lvsProtocol = ncProtocol nc
            , lvsRelease = protocolName (ncProtocol nc)
            }
 where
    --TODO: this is meaningless. Nodes do not have ids. The port number is not
    -- an ID. We don't even have a port number that we know if we're given our
    -- listening socket via systemd socket activation.
    nodeId :: Text
    nodeId = Text.pack $ "Port: " <> maybe "-" show (naPort <$> ncNodeAddr nc)

setNodeThread :: NFData a => LiveViewBackend blk a -> Async.Async () -> IO ()
setNodeThread lvbe nodeThr =
  modifyMVar_ (getbe lvbe) $ \lvs ->
      return $ lvs { lvsNodeThread = LiveViewThread $ Just nodeThr }

storePeersInLiveView :: NFData a => [Peer blk] -> LiveViewBackend blk a -> IO ()
storePeersInLiveView peers lvbe = modifyMVar_ (getbe lvbe) $ \lvs -> pure $ lvs { lvsPeers = peers }

captureCounters :: NFData a => LiveViewBackend blk a -> Trace IO Text -> IO ()
captureCounters lvbe trace0 = do
    let trace' = appendName "metrics" trace0
        counters = [MemoryStats, ProcessStats, NetStats, IOStats, SysStats]
    -- start capturing counters on this process
    thr <- Async.async $ forever $ do
                threadDelay 1000000   -- 1 second
                cts <- readCounters (ObservableTraceSelf counters)
                traceCounters trace' cts

    modifyMVar_ (getbe lvbe) $ \lvs -> return $ lvs { lvsMetricsThread = LiveViewThread $ Just thr }
    where
    traceCounters :: forall m a. MonadIO m => Trace m a -> [Counter] -> m ()
    traceCounters _tr [] = return ()
    traceCounters tr (c@(Counter _ct cn cv) : cs) = do
        mle <- mkLOMeta Critical Confidential
        traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
        traceCounters tr cs
