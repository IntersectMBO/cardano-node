{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module LiveView (
      LiveViewBackend (..)
    , realize
    , effectuate
    , captureCounters
    , setTopology
    ) where

import qualified Brick.Main as M
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import           Control.Monad (forever)
import           Data.Aeson (FromJSON)
import           Data.Text (Text, pack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Version (showVersion)

import           GitRev (gitRev)

import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Trace
import           Ouroboros.Consensus.NodeId
import           Paths_cardano_node (version)
import           Topology


type LiveViewMVar a = MVar (LiveViewState a)
newtype LiveViewBackend a = LiveViewBackend { getbe :: LiveViewMVar a }

instance (FromJSON a) => IsBackend LiveViewBackend a where
    typeof _ = UserDefinedBK "LiveViewBackend"
    realize _ = do
        initState <- initLiveViewState
        mv <- newMVar initState
        let sharedState = LiveViewBackend mv
        -- start UI
        thr <- Async.async $
            -- brick process
            return ()
        modifyMVar_ mv $ \lvs -> return $ lvs { lvsUIThread = Just thr }
        return $ sharedState

    unrealize be = putStrLn $ "unrealize " <> show (typeof be)

instance IsEffectuator LiveViewBackend a where
    effectuate lvbe _item = do
        modifyMVar_ (getbe lvbe) $ \lvs ->
            return $ lvs { lvsBlockHeight = lvsBlockHeight lvs + 1 }

    handleOverflow _ = return ()


data LiveViewState a = LiveViewState
    { lvsQuit            :: Bool
    , lvsRelease         :: Text
    , lvsNodeId          :: Text
    , lvsVersion         :: Text
    , lvsCommit          :: Text
    , lvsUpTime          :: Text
    , lvsBlockHeight     :: Int
    , lvsBlocksMinted    :: Int
    , lvsTransactions    :: Int
    , lvsPeersConnected  :: Int
    , lvsMaxNetDelay     :: Int
    , lvsMempool         :: Int
    , lvsMempoolPerc     :: Int
    , lvsCPUUsagePerc    :: Int
    , lvsMemoryUsageCurr :: Double
    , lvsMemoryUsageMax  :: Double
    -- internal state
    , lvsStartTime       :: UTCTime
    , lvsMessage         :: Maybe a
    , lvsUIThread        :: Maybe (Async.Async ())
    , lvsMetricsThread   :: Maybe (Async.Async ())
    } deriving (Eq)

initLiveViewState
    :: IO (LiveViewState a)
initLiveViewState = do
    tstamp <- getCurrentTime
    return $ LiveViewState
                { lvsQuit            = False
                , lvsRelease         = "Shelley"
                , lvsNodeId          = "N/A"
                , lvsVersion         = pack $ showVersion version
                , lvsCommit          = gitRev
                , lvsUpTime          = "00:00:00"
                , lvsBlockHeight     = 1891
                , lvsBlocksMinted    = 543
                , lvsTransactions    = 1732
                , lvsPeersConnected  = 3
                , lvsMaxNetDelay     = 17
                , lvsMempool         = 95
                , lvsMempoolPerc     = 79
                , lvsCPUUsagePerc    = 58
                , lvsMemoryUsageCurr = 2.1
                , lvsMemoryUsageMax  = 4.7
                , lvsStartTime       = tstamp
                , lvsMessage         = Nothing
                , lvsUIThread        = Nothing
                , lvsMetricsThread   = Nothing
                }

setTopology :: LiveViewBackend a -> TopologyInfo -> IO ()
setTopology lvbe (TopologyInfo nodeId _) =
    modifyMVar_ (getbe lvbe) $ \lvs ->
        return $ lvs { lvsNodeId = namenum }
  where
    namenum = case nodeId of
          CoreId num  -> "C" <> pack (show num)
          RelayId num -> "R" <> pack (show num)

captureCounters :: LiveViewBackend a -> Trace IO Text -> IO ()
captureCounters lvbe trace0 = do
    let trace = appendName "metrics" trace0
        counters = [MemoryStats, ProcessStats, NetStats, IOStats]
    -- start capturing counters on this process
    thr <- Async.async $ forever $ do
               threadDelay 1000000   -- 1 second
               cts <- readCounters (ObservableTraceSelf counters)
               traceCounters trace cts

    modifyMVar_ (getbe lvbe) $ \lvs -> return $ lvs { lvsMetricsThread = Just thr }
    return ()
  where
    traceCounters _tr [] = return ()
    traceCounters tr (c@(Counter _ct cn cv) : cs) = do
        mle <- mkLOMeta Info Confidential
        traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
        traceCounters tr cs
