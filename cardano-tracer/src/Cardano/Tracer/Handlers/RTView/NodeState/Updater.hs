{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

#if defined(mingw32_HOST_OS)
#define WINDOWS
#endif
#if defined(linux_HOST_OS)
#define LINUX
#endif
#if defined(darwin_HOST_OS)
#define DARWIN
#endif

module Cardano.RTView.NodeState.Updater
    ( launchNodeStateUpdater
    ) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import           Control.Monad (forever, forM_)
import           Control.Monad.STM (atomically)
import qualified Data.Aeson as A
import           Data.HashMap.Strict ((!), (!?))
import qualified Data.HashMap.Strict as HM
import           Data.List (partition)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)
import           System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as G
import           System.Time.Extra (sleep)
import           Text.Read (readMaybe)

import           Cardano.BM.Backend.Switchboard (Switchboard, readLogBuffer)
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Counter (Platform (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..), LogObject (..),
                                          MonitorAction (..), utc2ns)
import           Cardano.BM.Trace (Trace, logDebug)

import           Cardano.RTView.EKG (EKGStores, isItEKGMetric, storeEKGMetrics)
import           Cardano.RTView.ErrorBuffer (ErrorBuffer, readErrorBuffer)
import           Cardano.RTView.NodeState.Parsers (extractPeersInfo)
import           Cardano.RTView.NodeState.Types

-- | This function is running in a separate thread.
--   It takes |LogObject|s with nodes' metrics from |LogBuffer|,
--   extracts these metrics and updates corresponding values
--   in the |NodesState|.
launchNodeStateUpdater
  :: Trace IO Text
  -> Switchboard Text
  -> ErrorBuffer Text
  -> TVar NodesState
  -> TVar EKGStores
  -> IO ()
launchNodeStateUpdater tr switchBoard errBuff nsTVar ekgStoresTVar = forever $ do
  -- Take current |LogObject|s from the |ErrorBuffer|.
  updateNodesStateByTracedErrors tr errBuff nsTVar
  -- Take current |LogObject|s from the |LogBuffer| and split them in two groups:
  -- 1. EKG-metrics,
  -- 2. Traced values.
  -- Later all traced values and EKG-metrics will be received via two independent channels.
  (ekgMetrics, tracedValues) <- partition isItEKGMetric <$> readLogBuffer switchBoard
  storeEKGMetrics ekgMetrics ekgStoresTVar

  updateNodesStateByTracedValues tr nsTVar tracedValues
  updateNodesStateByEKGMetrics tr nsTVar ekgStoresTVar
  -- Check for updates in the |LogBuffer| every second.
  sleep 1.0

updateNodesStateByTracedErrors
  :: Trace IO Text
  -> ErrorBuffer Text
  -> TVar NodesState
  -> IO ()
updateNodesStateByTracedErrors tr errBuff nsTVar = do
  currentErrLogObjects <- readErrorBuffer errBuff
  forM_ currentErrLogObjects $ \(loggerName, errLogObject) ->
    updateNodesStateErrors tr nsTVar loggerName errLogObject

-- | Update NodeState for particular node based on loggerName.
--   Please note that this function updates only Error-messages (if errors occurred).
updateNodesStateErrors
  :: Trace IO Text
  -> TVar NodesState
  -> Text
  -> LogObject Text
  -> IO ()
updateNodesStateErrors tr nsTVar loggerName (LogObject aName aMeta aContent) = do
  logDebug tr $ "New error, name: " <> aName
              <> ", meta: " <> T.pack (show aMeta)
              <> ", content: " <> T.pack (show aContent)
  -- Check the name of the node this logObject came from.
  -- It is assumed that configuration contains correct names of remote nodes and
  -- loggers for them, for example:
  --   1. "a" - name of remote node in getAcceptAt.
  --   2. "cardano-rt-view.acceptor.a" - name of the logger which receives
  --        |LogObject|s from that node.
  -- So currently logger name for metrics has the following format:
  -- #buffered.cardano-rt-view.acceptor.a.NAME_OF_METRICS_GROUP.NAME_OF_METRIC,
  -- where "a" is the node name (from TraceAcceptor).
  let loggerNameParts = filter (not . T.null) $ T.splitOn "." loggerName
      nameOfNode = loggerNameParts !! 3

  atomically $ modifyTVar' nsTVar $ \currentNodesState ->
    let nsWith :: NodeState -> NodesState
        nsWith newState = HM.adjust (const newState) nameOfNode currentNodesState
    in
    case currentNodesState !? nameOfNode of
      Just ns -> nsWith $ updateNodeErrors ns aMeta aContent
      Nothing -> currentNodesState

updateNodeErrors :: Show a => NodeState -> LOMeta -> LOContent a -> NodeState
updateNodeErrors ns (LOMeta timeStamp _ _ sev _) aContent = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = newErrors
    , errorsChanged = True -- Every error message changes current list of errors by definition.
    , errorsRebuild = False -- By default we shouldn't rebuild all the list.
    }
  newErrors = currentErrors ++ [NodeError timeStamp sev errorMessage visible]
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics
  errorMessage =
    case aContent of
      LogMessage msg -> T.pack (show msg)
      LogError eMsg -> eMsg
      LogStructured obj -> T.pack (show obj)
      LogStructuredText _ txt -> txt
      MonitoringEffect (MonitorAlert msg) -> "Monitor alert: " <> msg
      MonitoringEffect _ -> ""
      _ -> "UNPARSED_ERROR_MESSAGE"
  visible = True

updateNodesStateByTracedValues
  :: Trace IO Text
  -> TVar NodesState
  -> [(Text, LogObject Text)]
  -> IO ()
updateNodesStateByTracedValues tr nsTVar tracedValues =
  forM_ tracedValues $ \(loggerName, logObject) ->
    updateNodesStateByTracedValue tr nsTVar loggerName logObject

-- | Update NodeState for particular node based on loggerName.
updateNodesStateByTracedValue
  :: Trace IO Text
  -> TVar NodesState
  -> Text
  -> LogObject Text
  -> IO ()
updateNodesStateByTracedValue tr nsTVar loggerName (LogObject aName aMeta aContent) = do
  logDebug tr $ "New logObject, name: " <> aName
                <> ", meta: " <> T.pack (show aMeta)
                <> ", content: " <> T.pack (show aContent)
  -- Check the name of the node this logObject came from.
  -- It is assumed that configuration contains correct names of remote nodes and
  -- loggers for them, for example:
  --   1. "a" - name of remote node in getAcceptAt.
  --   2. "cardano-rt-view.acceptor.a" - name of the logger which receives
  --        |LogObject|s from that node.
  -- So currently logger name for metrics has the following format:
  -- #buffered.cardano-rt-view.acceptor.a.NAME_OF_METRICS_GROUP.NAME_OF_METRIC,
  -- where "a" is the node name (from TraceAcceptor).
  let loggerNameParts = filter (not . T.null) $ T.splitOn "." loggerName
      nameOfNode = loggerNameParts !! 3

  now <- getMonotonicTimeNSec

  atomically $ modifyTVar' nsTVar $ \currentNodesState ->
    let nsWith :: NodeState -> NodesState
        nsWith newState = HM.adjust (const newState) nameOfNode currentNodesState
        itIs name' = name' `T.isInfixOf` aName
        textValue updater =
          case aContent of
            LogMessage txtValue -> nsWith $ updater txtValue
            _ -> currentNodesState
    in
    case currentNodesState !? nameOfNode of
      Just ns ->
        if | itIs "basicInfo.protocol" ->
             textValue $ updateNodeProtocol ns now
           | itIs "basicInfo.version" ->
             textValue $ updateNodeVersion ns now
           | itIs "basicInfo.commit" ->
             textValue $ updateNodeCommit ns now
           | itIs "basicInfo.nodeStartTime" ->
             textValue $ updateNodeStartTime ns now
           | itIs "basicInfo.systemStartTime" ->
             textValue $ updateSystemStartTime ns now
           | itIs "basicInfo.slotLengthShelley" ->
             textValue $ updateSlotLength Shelley ns now
           | itIs "basicInfo.slotsPerKESPeriodShelley" ->
             textValue $ updateSlotsPerKESPeriod Shelley ns now
           | itIs "basicInfo.slotLengthAllegra" ->
             textValue $ updateSlotLength Allegra ns now
           | itIs "basicInfo.slotsPerKESPeriodAllegra" ->
             textValue $ updateSlotsPerKESPeriod Allegra ns now
           | itIs "basicInfo.slotLengthMary" ->
             textValue $ updateSlotLength Mary ns now
           | itIs "basicInfo.slotsPerKESPeriodMary" ->
             textValue $ updateSlotsPerKESPeriod Mary ns now
           | otherwise ->
               case aContent of
                 LogStructured newPeersInfo ->
                   nsWith $ updatePeersInfo ns newPeersInfo now
                 LogValue "density" (PureD density) ->
                   nsWith $ updateChainDensity ns density now
                 LogValue "blockNum" (PureI blockNum) ->
                   nsWith $ updateBlocksNumber ns blockNum now
                 LogValue "slotInEpoch" (PureI slotNum) ->
                   nsWith $ updateSlotInEpoch ns slotNum now
                 LogValue "epoch" (PureI epoch') ->
                   nsWith $ updateEpoch ns epoch' now
                 LogValue "txsInMempool" (PureI txsInMempool) ->
                   nsWith $ updateMempoolTxs ns txsInMempool now
                 LogValue "mempoolBytes" (PureI mempoolBytes') ->
                   nsWith $ updateMempoolBytes ns mempoolBytes' now
                 LogValue "txsProcessedNum" (PureI processedTxsNum) ->
                   nsWith $ updateTxsProcessed ns processedTxsNum now
                 LogValue "blocksForgedNum" (PureI forgedBlocksNum) ->
                   nsWith $ updateBlocksForged ns forgedBlocksNum now
                 LogValue "nodeCannotForge" (PureI cannotForge) ->
                   nsWith $ updateNodeCannotForge ns cannotForge now
                 LogValue "nodeIsLeaderNum" (PureI leaderNum) ->
                   nsWith $ updateNodeIsLeader ns leaderNum now
                 LogValue "slotsMissedNum" (PureI missedSlotsNum) ->
                   nsWith $ updateSlotsMissed ns missedSlotsNum now
                 LogValue "operationalCertificateStartKESPeriod" (PureI oCertStartKesPeriod) ->
                   nsWith $ updateCertStartKESPeriod ns oCertStartKesPeriod now
                 LogValue "operationalCertificateExpiryKESPeriod" (PureI oCertExpiryKesPeriod) ->
                   nsWith $ updateCertExpiryKESPeriod ns oCertExpiryKesPeriod now
                 LogValue "currentKESPeriod" (PureI currentKesPeriod) ->
                   nsWith $ updateCurrentKESPeriod ns currentKesPeriod now
                 LogValue "remainingKESPeriods" (PureI kesPeriodsUntilExpiry) ->
                   nsWith $ updateRemainingKESPeriods ns kesPeriodsUntilExpiry now
                 _ -> currentNodesState
      Nothing ->
        -- This is a problem, because it means that configuration is unexpected one:
        -- name of node in getAcceptAt doesn't correspond to the name of loggerName.
        currentNodesState

updateNodesStateByEKGMetrics
  :: Trace IO Text
  -> TVar NodesState
  -> TVar EKGStores
  -> IO ()
updateNodesStateByEKGMetrics tr nsTVar ekgStoresTVar = do
  ekgStores <- readTVarIO ekgStoresTVar
  let namesOfNodes = HM.keys ekgStores
  forM_ namesOfNodes $ \nameOfNode -> do
    let (_, gaugesForThisNode) = ekgStores ! nameOfNode
    forM_ (HM.toList gaugesForThisNode) $ \(metricName, (gauge, timeStamp)) ->
      updateNodesStateByEKGMetric tr nsTVar nameOfNode metricName gauge timeStamp

updateNodesStateByEKGMetric
  :: Trace IO Text
  -> TVar NodesState
  -> Text
  -> Text
  -> Gauge
  -> UTCTime
  -> IO ()
updateNodesStateByEKGMetric _tr nsTVar nameOfNode mName gauge ts = do
  mValue <- G.read gauge
  now <- getMonotonicTimeNSec

  atomically $ modifyTVar' nsTVar $ \currentNodesState ->
    let nsWith :: NodeState -> NodesState
        nsWith newState = HM.adjust (const newState) nameOfNode currentNodesState
    in
    case currentNodesState !? nameOfNode of
      Just ns ->
        if | mName == "Sys.Platform" ->    nsWith $ updateNodePlatform ns (fromIntegral mValue) now
           | mName == "RTS.gcLiveBytes" -> nsWith $ updateRTSBytesUsed ns (fromIntegral mValue) now
           | mName == "RTS.gcMajorNum" ->  nsWith $ updateGcMajorNum   ns (fromIntegral mValue) now
           | mName == "RTS.gcMinorNum" ->  nsWith $ updateGcMinorNum   ns (fromIntegral mValue) now
           | mName == "RTS.gcticks" ->     nsWith $ updateGCTicks      ns (fromIntegral mValue) ts now
           | mName == "RTS.mutticks" ->    nsWith $ updateMutTicks     ns (fromIntegral mValue) ts now
#ifdef WINDOWS
           | mName == "Stat.CPUTime" ->
             let microsecs = fromIntegral rawValue in
             nsWith $ updateCPUSecs ns (microsecs * 1000) ts now
#endif
#ifdef DARWIN
           | mName == "Mem.resident_size" -> nsWith $ updateMemoryBytes ns (fromIntegral mValue) now
           | mName == "Sys.CPUTime" ->       nsWith $ updateCPUSecs     ns (fromIntegral mValue) ts now
           | mName == "Net.ifd_0-ibytes" ->  nsWith $ updateNetworkIn   ns (fromIntegral mValue) ts now
           | mName == "Net.ifd_0-obytes" ->  nsWith $ updateNetworkOut  ns (fromIntegral mValue) ts now
#endif
#ifdef LINUX
           | mName == "Mem.resident" ->        nsWith $ updateMemoryBytes ns (fromIntegral mValue) now
           | mName == "IO.rchar" ->            nsWith $ updateDiskRead    ns (fromIntegral mValue) ts now
           | mName == "IO.wchar" ->            nsWith $ updateDiskWrite   ns (fromIntegral mValue) ts now
           | mName == "Stat.cputicks" ->       nsWith $ updateCPUTicks    ns (fromIntegral mValue) ts now
           | mName == "Net.IpExt:InOctets" ->  nsWith $ updateNetworkIn   ns (fromIntegral mValue) ts now
           | mName == "Net.IpExt:OutOctets" -> nsWith $ updateNetworkOut  ns (fromIntegral mValue) ts now
#endif
           | otherwise -> currentNodesState
      Nothing ->
        -- This is a problem, because it means that configuration is unexpected one:
        -- name of node in getAcceptAt doesn't correspond to the name of loggerName.
        currentNodesState

-- Updaters for particular node state's fields.

updatePeersInfo :: NodeState -> A.Object -> Word64 -> NodeState
updatePeersInfo ns newPeersInfo now = ns { peersMetrics = newPeersMetrics, metricsLastUpdate = now }
 where
  newPeersMetrics = currentMetrics
    { peersInfo = newPeersInfo'
    , peersInfoChanged = peersInfo currentMetrics /= newPeersInfo'
    }
  currentMetrics = peersMetrics ns
  newPeersInfo' = extractPeersInfo newPeersInfo

updateNodeProtocol :: NodeState -> Word64 -> Text -> NodeState
updateNodeProtocol ns now protocol = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodeProtocol = protocol
    , nodeProtocolChanged = nodeProtocol currentMetrics /= protocol
    }
  currentMetrics = nodeMetrics ns

updateNodeVersion :: NodeState -> Word64 -> Text -> NodeState
updateNodeVersion ns now version = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodeVersion = version
    , nodeVersionChanged = nodeVersion currentMetrics /= version
    }
  currentMetrics = nodeMetrics ns

updateNodeCommit :: NodeState -> Word64 -> Text -> NodeState
updateNodeCommit ns now commit = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodeCommit = commit
    , nodeCommitChanged = nodeCommit currentMetrics /= commit
    , nodeShortCommit = T.take 7 commit
    }
  currentMetrics = nodeMetrics ns

updateNodeStartTime :: NodeState -> Word64 -> Text -> NodeState
updateNodeStartTime ns now startTime = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodeStartTime = startTimeUTC
    , nodeStartTimeChanged = nodeStartTime currentMetrics /= startTimeUTC
    }
  currentMetrics = nodeMetrics ns
  startTimeUTC =
    case readMaybe (T.unpack startTime) of
      Just (st :: UTCTime) -> st
      Nothing -> nullTime

updateSystemStartTime :: NodeState -> Word64 -> Text -> NodeState
updateSystemStartTime ns now systemStart = ns { blockchainMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { systemStartTime = systemStartTimeUTC
    , systemStartTimeChanged = systemStartTime currentMetrics /= systemStartTimeUTC
    }
  currentMetrics = blockchainMetrics ns
  systemStartTimeUTC =
    case readMaybe (T.unpack systemStart) of
      Just (st :: UTCTime) -> st
      Nothing -> nullTime

updateSlotLength :: Era -> NodeState -> Word64 -> Text -> NodeState
updateSlotLength era ns now sl = ns { blockchainMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics =
    case era of
      Shelley -> currentMetrics { slotLengthShelley = slotLength }
      Allegra -> currentMetrics { slotLengthAllegra = slotLength }
      Mary    -> currentMetrics { slotLengthMary    = slotLength }
  currentMetrics = blockchainMetrics ns
  slotLength =
    case readMaybe (T.unpack sl) of
      Just (sl' :: Integer) -> sl'
      Nothing -> slotLengthShelley currentMetrics

updateSlotsPerKESPeriod :: Era -> NodeState -> Word64 -> Text -> NodeState
updateSlotsPerKESPeriod era ns now spk = ns { blockchainMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics =
    case era of
      Shelley -> currentMetrics { slotsPerKESPeriodShelley = spk' }
      Allegra -> currentMetrics { slotsPerKESPeriodAllegra = spk' }
      Mary    -> currentMetrics { slotsPerKESPeriodMary    = spk' }
  currentMetrics = blockchainMetrics ns
  spk' =
    case readMaybe (T.unpack spk) of
      Just (kes :: Integer) -> kes
      Nothing -> slotsPerKESPeriodShelley currentMetrics

updateNodePlatform :: NodeState -> Int -> Word64 -> NodeState
updateNodePlatform ns platfId now = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodePlatform = platformName
    , nodePlatformChanged = True -- nodePlatform currentMetrics /= platformName
    }
  currentMetrics = nodeMetrics ns
  platformName = T.pack . show $ (toEnum platfId :: Platform)

#if defined(DARWIN)
updateMemoryBytes :: NodeState -> Word64 -> Word64 -> NodeState
updateMemoryBytes ns bytes now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics     = currentMetrics { memory = if bytes == 0
                                               then currentMemory
                                               else mBytes
                                  }
  currentMetrics = resourcesMetrics ns
  currentMemory  = memory currentMetrics
  mBytes         = fromIntegral bytes / 1024 / 1024 :: Double
#endif

#if defined(LINUX)
updateMemoryBytes :: NodeState -> Integer -> Word64 -> NodeState
updateMemoryBytes ns bytes now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics     = currentMetrics { memory = if bytes == 0
                                               then currentMemory
                                               else mBytes
                                  }
  currentMetrics = resourcesMetrics ns
  currentMemory  = memory currentMetrics
  mBytes         = fromIntegral bytes / 1024 / 1024 :: Double
#endif

#ifdef LINUX
updateDiskRead :: NodeState -> Word64 -> UTCTime -> Word64 -> NodeState
updateDiskRead ns bytesWereRead ts now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { diskUsageR     = currentDiskRate
      , diskUsageRLast = bytesWereRead
      , diskUsageRNs   = currentTimeInNs
      }
  currentMetrics  = resourcesMetrics ns
  currentDiskRate = bytesDiffInKB / timeDiffInSecs
  bytesDiffInKB   = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                      then 1.0
                      else bytesDiffInKB'
  bytesDiffInKB'  = bytesDiff / 1024
  bytesDiff       = fromIntegral (bytesWereRead - diskUsageRLast currentMetrics) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  timeDiff        = fromIntegral (currentTimeInNs - diskUsageRNs currentMetrics) :: Double
  currentTimeInNs = utc2ns ts

updateDiskWrite :: NodeState -> Word64 -> UTCTime -> Word64 -> NodeState
updateDiskWrite ns bytesWereWritten ts now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { diskUsageW     = currentDiskRate
      , diskUsageWLast = bytesWereWritten
      , diskUsageWNs   = currentTimeInNs
      }
  currentMetrics  = resourcesMetrics ns
  currentDiskRate = bytesDiffInKB / timeDiffInSecs
  bytesDiffInKB   = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                      then 1.0
                      else bytesDiffInKB'
  bytesDiffInKB'  = bytesDiff / 1024
  bytesDiff       = fromIntegral (bytesWereWritten - diskUsageWLast currentMetrics) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  timeDiff        = fromIntegral (currentTimeInNs - diskUsageWNs currentMetrics) :: Double
  currentTimeInNs = utc2ns ts

updateCPUTicks :: NodeState -> Integer -> UTCTime -> Word64 -> NodeState
updateCPUTicks ns ticks ts now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { cpuPercent = newCPUPercent
      , cpuLast    = ticks
      , cpuNs      = tns
      }
  currentMetrics = resourcesMetrics ns
  newCPUPercent  = if cpuperc < 0
                     then 0.0
                     else cpuperc * 100.0
  cpuperc        = fromIntegral (ticks - cpuLast currentMetrics) / fromIntegral clktck / tdiff
  clktck         = 100 :: Integer
  tdiff          = max 0.1 $ fromIntegral (tns - cpuNs currentMetrics) / 1000000000 :: Double
  tns            = utc2ns ts
#endif

#if defined(DARWIN) || defined(LINUX)
updateNetworkIn :: NodeState -> Word64 -> UTCTime -> Word64 -> NodeState
updateNetworkIn ns inBytes ts now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { networkUsageIn     = currentNetRate
      , networkUsageInLast = inBytes
      , networkUsageInNs   = currentTimeInNs
      }
  currentMetrics  = resourcesMetrics ns
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  bytesDiffInKB   = bytesDiff / 1024
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (inBytes - networkUsageInLast currentMetrics) :: Double
  timeDiff        = fromIntegral (currentTimeInNs - networkUsageInNs currentMetrics) :: Double
  currentTimeInNs = utc2ns ts

updateNetworkOut :: NodeState -> Word64 -> UTCTime -> Word64 -> NodeState
updateNetworkOut ns outBytes ts now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { networkUsageOut     = currentNetRate
      , networkUsageOutLast = outBytes
      , networkUsageOutNs   = currentTimeInNs
      }
  currentMetrics  = resourcesMetrics ns
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  bytesDiffInKB   = bytesDiff / 1024
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (outBytes - networkUsageOutLast currentMetrics) :: Double
  timeDiff        = fromIntegral (currentTimeInNs - networkUsageOutNs currentMetrics) :: Double
  currentTimeInNs = utc2ns ts
#endif

#if defined(DARWIN) || defined(WINDOWS)
updateCPUSecs :: NodeState -> Word64 -> UTCTime -> Word64 -> NodeState
updateCPUSecs ns nanosecs ts now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { cpuPercent = newCPUPercent
      , cpuLast    = fromIntegral nanosecs
      , cpuNs      = tns
      }
  currentMetrics = resourcesMetrics ns
  newCPUPercent  = if cpuperc < 0
                    then 0.0
                    else if cpuperc > 20.0
                           then cpuPercent currentMetrics
                           else cpuperc * 100.0
  cpuperc  = fromIntegral deltacpu / 100000000 / tdiff
  deltacpu = fromIntegral nanosecs - cpuLast currentMetrics
  tdiff    = max 0.1 $ fromIntegral (tns - cpuNs currentMetrics) / 1000000000 :: Double
  tns      = utc2ns ts
#endif

updateMempoolTxs :: NodeState -> Integer -> Word64 -> NodeState
updateMempoolTxs ns txsInMempool now = ns { mempoolMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { mempoolTxsNumber = txsInMempool
    , mempoolTxsNumberChanged = changed
    , mempoolTxsPercent =   fromIntegral txsInMempool
                          / fromIntegral maxTxs
                          * 100.0
    , mempoolTxsPercentChanged = changed
    , mempoolMaxTxs = maxTxs
    , mempoolMaxTxsChanged = mempoolMaxTxs currentMetrics /= maxTxs
    }
  currentMetrics = mempoolMetrics ns
  currentMempoolTxsNumber = mempoolTxsNumber currentMetrics
  maxTxs = max txsInMempool (mempoolMaxTxs currentMetrics)
  changed = currentMempoolTxsNumber /= fromIntegral txsInMempool

updateMempoolBytes :: NodeState -> Integer -> Word64 -> NodeState
updateMempoolBytes ns newMempoolBytes now = ns { mempoolMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { mempoolBytes = fromIntegral newMempoolBytes
    , mempoolBytesChanged = currentMempoolBytes /= fromIntegral newMempoolBytes
    , mempoolBytesPercent =   fromIntegral newMempoolBytes
                            / fromIntegral maxBytes
                            * 100.0
    , mempoolMaxBytes = maxBytes
    , mempoolMaxBytesChanged = mempoolMaxBytes currentMetrics /= maxBytes
    }
  currentMetrics = mempoolMetrics ns
  currentMempoolBytes = mempoolBytes currentMetrics
  maxBytes = max newMempoolBytes (mempoolMaxBytes currentMetrics)

updateTxsProcessed :: NodeState -> Integer -> Word64 -> NodeState
updateTxsProcessed ns txsProcNum now = ns { mempoolMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { txsProcessed = txsProcNum
    , txsProcessedChanged = txsProcessed currentMetrics /= txsProcNum
    }
  currentMetrics = mempoolMetrics ns

updateBlocksForged :: NodeState -> Integer -> Word64 -> NodeState
updateBlocksForged ns blocksForged now = ns { forgeMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { blocksForgedNumber = blocksForged
    , blocksForgedNumberChanged = blocksForgedNumber currentMetrics /= blocksForged
    }
  currentMetrics = forgeMetrics ns

updateNodeCannotForge :: NodeState -> Integer -> Word64 -> NodeState
updateNodeCannotForge ns cannotForge now = ns { forgeMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { nodeCannotForge = cannotForge
    , nodeCannotForgeChanged = nodeCannotForge currentMetrics /= cannotForge
    }
  currentMetrics = forgeMetrics ns

updateNodeIsLeader :: NodeState -> Integer -> Word64 -> NodeState
updateNodeIsLeader ns nodeIsLeader now = ns { forgeMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { nodeIsLeaderNum = nodeIsLeader
    , nodeIsLeaderNumChanged = nodeIsLeaderNum currentMetrics /= nodeIsLeader
    }
  currentMetrics = forgeMetrics ns

updateSlotsMissed :: NodeState -> Integer -> Word64 -> NodeState
updateSlotsMissed ns slotsMissed now = ns { forgeMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { slotsMissedNumber = slotsMissed
    , slotsMissedNumberChanged = slotsMissedNumber currentMetrics /= slotsMissed
    }
  currentMetrics = forgeMetrics ns

updateRTSBytesUsed :: NodeState -> Integer -> Word64 -> NodeState
updateRTSBytesUsed ns usedMemBytes now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics     = currentMetrics { rtsMemoryUsed = if usedMemBytes == 0
                                                      then currentRTSMem
                                                      else mBytes
                                  }
  currentMetrics = rtsMetrics ns
  currentRTSMem  = rtsMemoryUsed currentMetrics
  mBytes         = fromIntegral usedMemBytes / 1024 / 1024 :: Double

updateGcMajorNum :: NodeState -> Integer -> Word64 -> NodeState
updateGcMajorNum ns gcMajorNum now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { rtsGcMajorNum = gcMajorNum
    , rtsGcMajorNumChanged = rtsGcMajorNum currentMetrics /= gcMajorNum
    }
  currentMetrics = rtsMetrics ns

updateGcMinorNum :: NodeState -> Integer -> Word64 -> NodeState
updateGcMinorNum ns gcMinorNum now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { rtsGcMinorNum = gcMinorNum
    , rtsGcMinorNumChanged = rtsGcMinorNum currentMetrics /= gcMinorNum
    }
  currentMetrics = rtsMetrics ns

updateGCTicks :: NodeState -> Integer -> UTCTime -> Word64 -> NodeState
updateGCTicks ns ticks ts now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { rtsGCPercent = newGCPercent
      , rtsGCLast    = ticks
      , rtsGCNs      = tns
      }
  currentMetrics = rtsMetrics ns
  newGCPercent   = if gcPerc < 0
                     then 0.0
                     else gcPerc * 100.0
  gcPerc         = fromIntegral (ticks - rtsGCLast currentMetrics) / fromIntegral clktck / tdiff
  clktck         = 100 :: Integer
  tdiff          = max 0.1 $ fromIntegral (tns - rtsGCNs currentMetrics) / 1000000000 :: Double
  tns            = utc2ns ts

updateMutTicks :: NodeState -> Integer -> UTCTime -> Word64 -> NodeState
updateMutTicks ns ticks ts now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { rtsMutPercent = newMutPercent
      , rtsMutLast    = ticks
      , rtsMutNs      = tns
      }
  currentMetrics = rtsMetrics ns
  newMutPercent  = if mutPerc < 0
                     then 0.0
                     else mutPerc * 100.0
  mutPerc        = fromIntegral (ticks - rtsMutLast currentMetrics) / fromIntegral clktck / tdiff
  clktck         = 100 :: Integer
  tdiff          = max 0.1 $ fromIntegral (tns - rtsMutNs currentMetrics) / 1000000000 :: Double
  tns            = utc2ns ts

updateCertStartKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCertStartKESPeriod ns oCertStartKesPeriod now = ns { kesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { opCertStartKESPeriod = oCertStartKesPeriod
    , opCertStartKESPeriodChanged = opCertStartKESPeriod currentMetrics /= oCertStartKesPeriod
    }
  currentMetrics = kesMetrics ns

updateCertExpiryKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCertExpiryKESPeriod ns oCertExpiryKesPeriod now = ns { kesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { opCertExpiryKESPeriod = oCertExpiryKesPeriod
    , opCertExpiryKESPeriodChanged = opCertExpiryKESPeriod currentMetrics /= oCertExpiryKesPeriod
    }
  currentMetrics = kesMetrics ns

updateCurrentKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCurrentKESPeriod ns currentKesPeriod now = ns { kesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { currentKESPeriod = currentKesPeriod
    , currentKESPeriodChanged = currentKESPeriod currentMetrics /= currentKesPeriod
    }
  currentMetrics = kesMetrics ns

updateRemainingKESPeriods :: NodeState -> Integer -> Word64 -> NodeState
updateRemainingKESPeriods ns kesPeriodsUntilExpiry now = ns { kesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { remKESPeriods = kesPeriodsUntilExpiry
    , remKESPeriodsChanged = changed
    , remKESPeriodsInDays = floor remainingInDays
    , remKESPeriodsInDaysChanged = changed
    }
  currentMetrics = kesMetrics ns
  changed = remKESPeriods currentMetrics /= kesPeriodsUntilExpiry
  (slotLength', slotsPerKESPeriod') = getSlotsValues ns
  remainingInSeconds = fromIntegral kesPeriodsUntilExpiry * slotLength' * slotsPerKESPeriod'
  remainingInDays :: Double
  remainingInDays = fromIntegral remainingInSeconds / 3600 / 24

updateChainDensity :: NodeState -> Double -> Word64 -> NodeState
updateChainDensity ns density now = ns { blockchainMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { chainDensity = newDencity
    , chainDensityChanged = chainDensity currentMetrics /= newDencity
    }
  currentMetrics = blockchainMetrics ns
  newDencity = 0.05 + density * 100.0

updateBlocksNumber :: NodeState -> Integer -> Word64 -> NodeState
updateBlocksNumber ns blockNum now = ns { blockchainMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { blocksNumber = blockNum
    , blocksNumberChanged = blocksNumber currentMetrics /= blockNum
    }
  currentMetrics = blockchainMetrics ns

updateSlotInEpoch :: NodeState -> Integer -> Word64 -> NodeState
updateSlotInEpoch ns slotNum now = ns { blockchainMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { slot = slotNum
    , slotChanged = slot currentMetrics /= slotNum
    }
  currentMetrics = blockchainMetrics ns

updateEpoch :: NodeState -> Integer -> Word64 -> NodeState
updateEpoch ns newEpoch now = ns { blockchainMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { epoch = newEpoch
    , epochChanged = epoch currentMetrics /= newEpoch
    }
  currentMetrics = blockchainMetrics ns

---

getSlotsValues :: NodeState -> (Integer, Integer)
getSlotsValues ns =
  case protocol of
    "Shelley" ->
      (slotLengthShelley bm, slotsPerKESPeriodShelley bm)
    "Byron; Shelley" ->
      -- It's a Cardano protocol, which supports all eras.
      -- Take Allegra-values for now.
      -- TODO: check current era explicitly and use corresponding values.
      (slotLengthAllegra bm, slotsPerKESPeriodAllegra bm)
    _ ->
      -- Ok, there are two possible ways:
      --   1. If it's "Byron" protocol, it's impossible,
      --      because Byron protocol doesn't support KES.
      --   2. If it's some unknown/new protocol, corresponding KES-values
      --      should be explicitly extracted from basicInfo.
      -- Use Shelley-values for now.
      (slotLengthShelley bm, slotsPerKESPeriodShelley bm)
 where
  protocol = nodeProtocol $ nodeMetrics ns
  bm = blockchainMetrics ns
