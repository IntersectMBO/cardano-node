{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Node.TUI.LiveView (
      LiveViewBackend (..)
    , realize
    , effectuate
    , captureCounters
    , setTopology
    , setNodeThread
    ) where

import           Cardano.Prelude hiding (isPrefixOf, on, show)
import           Prelude (String, show)

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text, pack, unpack)
import           Data.Time.Calendar (Day (..))
import           Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime,
                                  diffUTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Version (showVersion)
import           Data.Word (Word64)

import qualified Brick.AttrMap as A
import qualified Brick.BChan
import qualified Brick.Main as M
import           Brick.Themes (Theme, newTheme, themeToAttrMap)
import           Brick.Types (BrickEvent (..), EventM, Next, Widget)
import qualified Brick.Types as T
import           Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core (hBox, hLimitPercent, padBottom, padLeft,
                                     padRight, padTop, str, txt, updateAttrMap,
                                     vBox, vLimitPercent, withAttr,
                                     withBorderStyle)
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V

import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..),
                                          LogObject (..),
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta, utc2ns)
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Trace

import           Cardano.Node.TUI.GitRev (gitRev)
import           Ouroboros.Consensus.NodeId
import           Paths_cardano_node (version)
import           Cardano.Config.Topology

-- constants, to be evaluated from host system

-- getconf PAGESIZE
pagesize :: Integer
pagesize = 4096

-- getconf CLK_TCK
clktck :: Integer
clktck = 100

type LiveViewMVar a = MVar (LiveViewState a)
newtype LiveViewBackend a = LiveViewBackend { getbe :: LiveViewMVar a }

instance IsBackend LiveViewBackend Text where
    bekind _ = UserDefinedBK "LiveViewBackend"
    realize _ = do
        initState <- initLiveViewState
        mv <- newMVar initState
        let sharedState = LiveViewBackend mv
        thr <- Async.async $ do
            eventChan <- Brick.BChan.newBChan 10
            let buildVty = V.mkVty V.defaultConfig
            initialVty <- buildVty
            ticker <- Async.async $ forever $ do
                        -- could be replaced by retry if we have TVar-like vars
                        threadDelay 800000 -- refresh TUI every 800 ms
                        Brick.BChan.writeBChan eventChan $ LiveViewBackend mv
            Async.link ticker
            void $ M.customMain initialVty buildVty (Just eventChan) app initState
        modifyMVar_ mv $ \lvs -> return $ lvs { lvsUIThread = Just thr }
        return sharedState

    unrealize be = putStrLn $ "unrealize " <> show (bekind be)

instance IsEffectuator LiveViewBackend Text where
    effectuate lvbe item =
        case item of
            LogObject ["cardano","node","metrics"] meta content ->
                case content of
                    LogValue "Mem.resident" (PureI pages) ->
                        let mbytes = fromIntegral (pages * pagesize) / 1024 / 1024 :: Float
                        in
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                            return $ lvs { lvsMemoryUsageCurr = mbytes
                                         , lvsMemoryUsageMax  = max (lvsMemoryUsageMax lvs) mbytes
                                         , lvsUpTime          = diffUTCTime (tstamp meta) (lvsStartTime lvs)
                                         }
                    LogValue "IO.rchar" (Bytes bytesWereRead) ->
                        let currentTimeInNs = utc2ns (tstamp meta)
                        in
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                            let timeDiff        = fromIntegral (currentTimeInNs - lvsDiskUsageRNs lvs) :: Float
                                timeDiffInSecs  = timeDiff / 1000000000
                                bytesDiff       = fromIntegral (bytesWereRead - lvsDiskUsageRLast lvs) :: Float
                                bytesDiffInKB   = bytesDiff / 1024
                                currentDiskRate = bytesDiffInKB / timeDiffInSecs
                                maxDiskRate     = max currentDiskRate $ lvsDiskUsageRMax lvs
                            in
                            return $ lvs { lvsDiskUsageRCurr = currentDiskRate
                                         , lvsDiskUsageRPerc = (currentDiskRate / (maxDiskRate / 100.0)) / 100.0
                                         , lvsDiskUsageRLast = bytesWereRead
                                         , lvsDiskUsageRNs   = currentTimeInNs
                                         , lvsDiskUsageRMax  = maxDiskRate
                                         , lvsUpTime         = diffUTCTime (tstamp meta) (lvsStartTime lvs)
                                         }
                    LogValue "IO.wchar" (Bytes bytesWereWritten) ->
                        let currentTimeInNs = utc2ns (tstamp meta)
                        in
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                            let timeDiff        = fromIntegral (currentTimeInNs - lvsDiskUsageWNs lvs) :: Float
                                timeDiffInSecs  = timeDiff / 1000000000
                                bytesDiff       = fromIntegral (bytesWereWritten - lvsDiskUsageWLast lvs) :: Float
                                bytesDiffInKB   = bytesDiff / 1024
                                currentDiskRate = bytesDiffInKB / timeDiffInSecs
                                maxDiskRate     = max currentDiskRate $ lvsDiskUsageWMax lvs
                            in
                            return $ lvs { lvsDiskUsageWCurr = currentDiskRate
                                         , lvsDiskUsageWPerc = (currentDiskRate / (maxDiskRate / 100.0)) / 100.0
                                         , lvsDiskUsageWLast = bytesWereWritten
                                         , lvsDiskUsageWNs   = currentTimeInNs
                                         , lvsDiskUsageWMax  = maxDiskRate
                                         , lvsUpTime         = diffUTCTime (tstamp meta) (lvsStartTime lvs)
                                         }
                    LogValue "Stat.utime" (PureI ticks) ->
                        let tns = utc2ns (tstamp meta)
                        in
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                            let tdiff = min 1 $ (fromIntegral (tns - lvsCPUUsageNs lvs)) / 1000000000 :: Float
                                cpuperc = (fromIntegral (ticks - lvsCPUUsageLast lvs)) / (fromIntegral clktck) / tdiff
                            in
                            return $ lvs { lvsCPUUsagePerc = cpuperc
                                         , lvsCPUUsageLast = ticks
                                         , lvsCPUUsageNs   = tns
                                         , lvsUpTime       = diffUTCTime (tstamp meta) (lvsStartTime lvs)
                                         }
                    LogValue "Net.IpExt:InOctets" (Bytes inBytes) ->
                        let currentTimeInNs = utc2ns (tstamp meta)
                        in
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                            let timeDiff        = fromIntegral (currentTimeInNs - lvsNetworkUsageInNs lvs) :: Float
                                timeDiffInSecs  = timeDiff / 1000000000
                                bytesDiff       = fromIntegral (inBytes - lvsNetworkUsageInLast lvs) :: Float
                                bytesDiffInKB   = bytesDiff / 1024
                                currentNetRate  = bytesDiffInKB / timeDiffInSecs
                                maxNetRate      = max currentNetRate $ lvsNetworkUsageInMax lvs
                            in
                            return $ lvs { lvsNetworkUsageInCurr = currentNetRate
                                         , lvsNetworkUsageInPerc = (currentNetRate / (maxNetRate / 100.0)) / 100.0
                                         , lvsNetworkUsageInLast = inBytes
                                         , lvsNetworkUsageInNs   = currentTimeInNs
                                         , lvsNetworkUsageInMax  = maxNetRate
                                         , lvsUpTime             = diffUTCTime (tstamp meta) (lvsStartTime lvs)
                                         }
                    LogValue "Net.IpExt:OutOctets" (Bytes outBytes) ->
                        let currentTimeInNs = utc2ns (tstamp meta)
                        in
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                            let timeDiff        = fromIntegral (currentTimeInNs - lvsNetworkUsageOutNs lvs) :: Float
                                timeDiffInSecs  = timeDiff / 1000000000
                                bytesDiff       = fromIntegral (outBytes - lvsNetworkUsageOutLast lvs) :: Float
                                bytesDiffInKB   = bytesDiff / 1024
                                currentNetRate  = bytesDiffInKB / timeDiffInSecs
                                maxNetRate      = max currentNetRate $ lvsNetworkUsageOutMax lvs
                            in
                            return $ lvs { lvsNetworkUsageOutCurr = currentNetRate
                                         , lvsNetworkUsageOutPerc = (currentNetRate / (maxNetRate / 100.0)) / 100.0
                                         , lvsNetworkUsageOutLast = outBytes
                                         , lvsNetworkUsageOutNs   = currentTimeInNs
                                         , lvsNetworkUsageOutMax  = maxNetRate
                                         , lvsUpTime              = diffUTCTime (tstamp meta) (lvsStartTime lvs)
                                         }
                    _ -> pure ()
            LogObject _ _ (LogValue "txsInMempool" (PureI txsInMempool)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do
                        let lvsMempool' = fromIntegral txsInMempool :: Word64
                            percentage = fromIntegral lvsMempool' / fromIntegral (lvsMempoolCapacity lvs) :: Float
                        return $ lvs { lvsMempool = lvsMempool'
                                     , lvsMempoolPerc = percentage
                                     }
            LogObject _ _ (LogValue "density" (PureD density)) ->
                modifyMVar_ (getbe lvbe) $ \lvs ->
                        return $ lvs { lvsChainDensity = 0.05 + density * 100.0 }
            LogObject _ _ (LogValue "connectedPeers" (PureI npeers)) ->
                modifyMVar_ (getbe lvbe) $ \lvs ->
                        return $ lvs { lvsPeersConnected = fromIntegral npeers }
            LogObject _ _ (LogValue "txsProcessed" (PureI txsProcessed)) ->
                modifyMVar_ (getbe lvbe) $ \lvs ->
                        return $ lvs { lvsTransactions = lvsTransactions lvs + fromIntegral txsProcessed }
            LogObject _ _ (LogValue "blockNum" (PureI slotnum)) ->
                modifyMVar_ (getbe lvbe) $ \lvs ->
                        return $ lvs { lvsBlockNum = fromIntegral slotnum }
            LogObject _ _ (LogValue "slotInEpoch" (PureI slotnum)) ->
                modifyMVar_ (getbe lvbe) $ \lvs ->
                        return $ lvs { lvsSlotNum = fromIntegral slotnum }
            LogObject _ _ (LogValue "epoch" (PureI epoch)) ->
                modifyMVar_ (getbe lvbe) $ \lvs ->
                        return $ lvs { lvsEpoch = fromIntegral epoch }
            _ -> pure ()

    handleOverflow _ = pure ()

data ColorTheme
    = DarkTheme
    | LightTheme
    deriving (Eq)

data LiveViewState a = LiveViewState
    { lvsQuit                :: Bool
    , lvsRelease             :: String
    , lvsNodeId              :: Text
    , lvsVersion             :: String
    , lvsCommit              :: String
    , lvsUpTime              :: NominalDiffTime
    , lvsEpoch               :: Word64
    , lvsSlotNum             :: Word64
    , lvsBlockNum            :: Word64
    , lvsChainDensity        :: Double
    , lvsBlocksMinted        :: Word64
    , lvsTransactions        :: Word64
    , lvsPeersConnected      :: Word64
    , lvsMempool             :: Word64
    , lvsMempoolPerc         :: Float
    , lvsCPUUsagePerc        :: Float
    , lvsMemoryUsageCurr     :: Float
    , lvsMemoryUsageMax      :: Float
    , lvsDiskUsageRPerc      :: Float
    , lvsDiskUsageRCurr      :: Float
    , lvsDiskUsageRMax       :: Float
    , lvsDiskUsageWPerc      :: Float
    , lvsDiskUsageWCurr      :: Float
    , lvsDiskUsageWMax       :: Float
    , lvsNetworkUsageInPerc  :: Float
    , lvsNetworkUsageInCurr  :: Float
    , lvsNetworkUsageInMax   :: Float
    , lvsNetworkUsageOutPerc :: Float
    , lvsNetworkUsageOutCurr :: Float
    , lvsNetworkUsageOutMax  :: Float
    -- internal state
    , lvsStartTime           :: UTCTime
    , lvsCPUUsageLast        :: Integer
    , lvsCPUUsageNs          :: Word64
    , lvsDiskUsageRLast      :: Word64
    , lvsDiskUsageRNs        :: Word64
    , lvsDiskUsageWLast      :: Word64
    , lvsDiskUsageWNs        :: Word64
    , lvsNetworkUsageInLast  :: Word64
    , lvsNetworkUsageInNs    :: Word64
    , lvsNetworkUsageOutLast :: Word64
    , lvsNetworkUsageOutNs   :: Word64
    , lvsMempoolCapacity     :: Word64
    , lvsMessage             :: Maybe a
    , lvsUIThread            :: Maybe (Async.Async ())
    , lvsMetricsThread       :: Maybe (Async.Async ())
    , lvsNodeThread          :: Maybe (Async.Async ())
    , lvsColorTheme          :: ColorTheme
    } deriving (Eq)

initLiveViewState :: IO (LiveViewState a)
initLiveViewState = do
    now <- getCurrentTime
    return $ LiveViewState
                { lvsQuit                = False
                , lvsRelease             = "Shelley"
                , lvsNodeId              = ""
                , lvsVersion             = showVersion version
                , lvsCommit              = unpack gitRev
                , lvsUpTime              = diffUTCTime now now
                , lvsEpoch               = 0
                , lvsSlotNum             = 0
                , lvsBlockNum            = 0
                , lvsChainDensity        = 0.0
                , lvsBlocksMinted        = 0
                , lvsTransactions        = 0
                , lvsPeersConnected      = 0
                , lvsMempool             = 0
                , lvsMempoolPerc         = 0.0
                , lvsCPUUsagePerc        = 0.58
                , lvsMemoryUsageCurr     = 0.0
                , lvsMemoryUsageMax      = 0.2
                , lvsDiskUsageRPerc      = 0.0
                , lvsDiskUsageRCurr      = 0.0
                , lvsDiskUsageRMax       = 0.0
                , lvsDiskUsageWPerc      = 0.0
                , lvsDiskUsageWCurr      = 0.0
                , lvsDiskUsageWMax       = 0.0
                , lvsNetworkUsageInPerc  = 0.0
                , lvsNetworkUsageInCurr  = 0.0
                , lvsNetworkUsageInMax   = 0.0
                , lvsNetworkUsageOutPerc = 0.0
                , lvsNetworkUsageOutCurr = 0.0
                , lvsNetworkUsageOutMax  = 0.0
                , lvsStartTime           = now
                , lvsCPUUsageLast        = 0
                , lvsCPUUsageNs          = 10000
                , lvsDiskUsageRLast      = 0
                , lvsDiskUsageRNs        = 10000
                , lvsDiskUsageWLast      = 0
                , lvsDiskUsageWNs        = 10000
                , lvsNetworkUsageInLast  = 0
                , lvsNetworkUsageInNs    = 10000
                , lvsNetworkUsageOutLast = 0
                , lvsNetworkUsageOutNs   = 10000
                , lvsMempoolCapacity     = 200
                , lvsMessage             = Nothing
                , lvsUIThread            = Nothing
                , lvsMetricsThread       = Nothing
                , lvsNodeThread          = Nothing
                , lvsColorTheme          = DarkTheme
                }

setTopology :: LiveViewBackend a -> TopologyInfo -> IO ()
setTopology lvbe (TopologyInfo nodeid _) =
    modifyMVar_ (getbe lvbe) $ \lvs ->
        return $ lvs { lvsNodeId = namenum }
  where
    namenum = case nodeid of
        CoreId num  -> "C" <> pack (show num)
        RelayId num -> "R" <> pack (show num)

setNodeThread :: LiveViewBackend a -> Async.Async () -> IO ()
setNodeThread lvbe nodeThr =
    modifyMVar_ (getbe lvbe) $ \lvs ->
        return $ lvs { lvsNodeThread = Just nodeThr }

captureCounters :: LiveViewBackend a -> Trace IO Text -> IO ()
captureCounters lvbe trace0 = do
    let trace' = appendName "metrics" trace0
        counters = [MemoryStats, ProcessStats, NetStats, IOStats]
    -- start capturing counters on this process
    thr <- Async.async $ forever $ do
                threadDelay 1000000   -- 1 second
                cts <- readCounters (ObservableTraceSelf counters)
                traceCounters trace' cts

    modifyMVar_ (getbe lvbe) $ \lvs -> return $ lvs { lvsMetricsThread = Just thr }
    where
    traceCounters :: forall m a. MonadIO m => Trace m a -> [Counter] -> m ()
    traceCounters _tr [] = return ()
    traceCounters tr (c@(Counter _ct cn cv) : cs) = do
        mle <- mkLOMeta Critical Confidential
        traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
        traceCounters tr cs

-------------------------------------------------------------------------------
-- UI color themes
-------------------------------------------------------------------------------

cardanoAttr :: A.AttrName
cardanoAttr = "cardano"

releaseAttr :: A.AttrName
releaseAttr = "release"

nodeIdAttr :: A.AttrName
nodeIdAttr = "nodeId"

valueAttr :: A.AttrName
valueAttr = "value"

keyAttr :: A.AttrName
keyAttr = "quit"

barValueAttr :: A.AttrName
barValueAttr = "barValue"

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

mempoolDoneAttr, mempoolToDoAttr :: A.AttrName
mempoolDoneAttr = theBaseAttr <> A.attrName "mempool:done"
mempoolToDoAttr = theBaseAttr <> A.attrName "mempool:remaining"

memDoneAttr, memToDoAttr :: A.AttrName
memDoneAttr = theBaseAttr <> A.attrName "mem:done"
memToDoAttr = theBaseAttr <> A.attrName "mem:remaining"

cpuDoneAttr, cpuToDoAttr :: A.AttrName
cpuDoneAttr = theBaseAttr <> A.attrName "cpu:done"
cpuToDoAttr = theBaseAttr <> A.attrName "cpu:remaining"

diskIODoneAttr, diskIOToDoAttr :: A.AttrName
diskIODoneAttr = theBaseAttr <> A.attrName "diskIO:done"
diskIOToDoAttr = theBaseAttr <> A.attrName "diskIO:remaining"

networkIODoneAttr, networkIOToDoAttr :: A.AttrName
networkIODoneAttr = theBaseAttr <> A.attrName "networkIO:done"
networkIOToDoAttr = theBaseAttr <> A.attrName "networkIO:remaining"

-- Please note that there's no full support of RGB, it's just a terminal. :-)
progressToDoColorLFG
  , progressToDoColorLBG
  , progressDoneColorLFG
  , progressDoneColorLBG
  , progressToDoColorDFG
  , progressToDoColorDBG
  , progressDoneColorDFG
  , progressDoneColorDBG
  , darkMainBG
  :: V.Color
progressToDoColorLFG = V.white
progressToDoColorLBG = V.Color240 19
progressDoneColorLFG = V.white
progressDoneColorLBG = V.Color240 6
progressToDoColorDFG = V.black
progressToDoColorDBG = V.white
progressDoneColorDFG = V.black
progressDoneColorDBG = V.Color240 19
darkMainBG           = V.Color240 0

bold :: V.Attr -> V.Attr
bold a = V.withStyle a V.bold

lightThemeAttributes :: [(A.AttrName, V.Attr)]
lightThemeAttributes =
    [ (cardanoAttr,       bold $ fg V.black)
    , (releaseAttr,       bold $ fg V.blue)
    , (nodeIdAttr,        bold $ fg V.blue)
    , (valueAttr,         bold $ fg V.black)
    , (keyAttr,           bold $ fg V.magenta)
    , (barValueAttr,      bold $ fg V.black)
    , (mempoolDoneAttr,   bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (mempoolToDoAttr,   bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    , (memDoneAttr,       bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (memToDoAttr,       bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    , (cpuDoneAttr,       bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (cpuToDoAttr,       bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    , (diskIODoneAttr,    bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (diskIOToDoAttr,    bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    , (networkIODoneAttr, bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (networkIOToDoAttr, bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    ]

darkThemeAttributes :: [(A.AttrName, V.Attr)]
darkThemeAttributes =
    [ (cardanoAttr,       bold $ fg V.white)
    , (releaseAttr,       bold $ fg V.cyan)
    , (nodeIdAttr,        bold $ fg V.cyan)
    , (valueAttr,         bold $ fg V.white)
    , (keyAttr,           bold $ fg V.white)
    , (barValueAttr,      bold $ fg V.white)
    , (mempoolDoneAttr,   bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (mempoolToDoAttr,   bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    , (memDoneAttr,       bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (memToDoAttr,       bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    , (cpuDoneAttr,       bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (cpuToDoAttr,       bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    , (diskIODoneAttr,    bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (diskIOToDoAttr,    bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    , (networkIODoneAttr, bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (networkIOToDoAttr, bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    ]

lightTheme :: Theme
lightTheme = newTheme (V.black `on` V.white)
                      lightThemeAttributes

darkTheme :: Theme
darkTheme = newTheme (V.white `on` darkMainBG)
                     darkThemeAttributes

-------------------------------------------------------------------------------
-- UI drawing
-------------------------------------------------------------------------------

drawUI :: LiveViewState a -> [Widget ()]
drawUI p = [mainWidget p]

mainWidget :: LiveViewState a -> Widget ()
mainWidget p =
      C.hCenter
    . C.vCenter
    . hLimitPercent 96
    . vLimitPercent 96
    $ mainContentW p

mainContentW :: LiveViewState a -> Widget ()
mainContentW p =
      withBorderStyle BS.unicode
    . B.border $ vBox
        [ headerW p
        , hBox [systemStatsW p, nodeInfoW p]
        , keysMessageW
        ]

keysMessageW :: Widget ()
keysMessageW =
      padBottom (T.Pad 1)
    . padLeft   (T.Pad 2)
    $ hBox [ txt "Press "
           , withAttr keyAttr $ txt "Q"
           , txt " to quit, "
           , withAttr keyAttr $ txt "L"
           , txt "/"
           , withAttr keyAttr $ txt "D"
           , txt " to change color theme"
           ]

headerW :: LiveViewState a -> Widget ()
headerW p =
      C.hCenter
    . padTop   (T.Pad 1)
    . padLeft  (T.Pad 2)
    . padRight (T.Pad 2)
    $ hBox [   withAttr cardanoAttr
             . padRight (T.Pad 10)
             $ txt "CARDANO SL"
           , txt "release: "
           ,   withAttr releaseAttr
             $ str (lvsRelease p)
           , padLeft T.Max $ txt "Node: "
           ,   withAttr nodeIdAttr
             $ txt (lvsNodeId p)
           ]

systemStatsW :: LiveViewState a -> Widget ()
systemStatsW p =
      padTop   (T.Pad 2)
    . padLeft  (T.Pad 2)
    . padRight (T.Pad 2)
    $ vBox [ vBox [ hBox [ padBottom (T.Pad 1) $ txt "Mempool:"
                         , withAttr barValueAttr . padLeft T.Max . str . show $ lvsMempoolCapacity p
                         ]
                  , padBottom (T.Pad 1) memPoolBar
                  ]
           , vBox [ hBox [ txt "Memory usage:"
                         , withAttr barValueAttr . padLeft T.Max $ str $ (take 5 $ show $ max (lvsMemoryUsageMax p) 200.0) <> " MB"
                         ]
                  , padBottom (T.Pad 1) memUsageBar
                  ]
           , vBox [ hBox [ txt "CPU usage:"
                         , withAttr barValueAttr . padLeft T.Max $ str "100%"
                         ]
                  , padBottom (T.Pad 1) cpuUsageBar
                  ]
           , hBox [ vBox [ hBox [ txt "Disk R:"
                                , withAttr barValueAttr . padLeft T.Max $ str $ (take 5 $ show $ max (lvsDiskUsageRMax p) 1.0) <> " KB/s"
                                ]
                         , padBottom (T.Pad 1) diskUsageRBar
                         ]
                  , padLeft (T.Pad 3) $
                    vBox [ hBox [ txt "Disk W:"
                                , withAttr barValueAttr . padLeft T.Max $ str $ (take 5 $ show $ max (lvsDiskUsageWMax p) 1.0) <> " KB/s"
                                ]
                         , padBottom (T.Pad 1) diskUsageWBar
                         ]
                  ]
           , hBox [ vBox [ hBox [ txt "Network In:"
                                , withAttr barValueAttr . padLeft T.Max $ str $ (take 5 $ show $ max (lvsNetworkUsageInMax p) 1.0) <> " KB/s"
                                ]
                         , padBottom (T.Pad 1) networkUsageInBar
                         ]
                  , padLeft (T.Pad 3) $
                    vBox [ hBox [ txt "Network Out:"
                                , withAttr barValueAttr . padLeft T.Max $ str $ (take 5 $ show $ max (lvsNetworkUsageOutMax p) 1.0) <> " KB/s"
                                ]
                         , padBottom (T.Pad 1) networkUsageOutBar
                         ]
                  ]
           ]
  where
    -- use mapAttrNames
    memPoolBar :: forall n. Widget n
    memPoolBar = updateAttrMap
                 (A.mapAttrNames [ (mempoolDoneAttr, P.progressCompleteAttr)
                                 , (mempoolToDoAttr, P.progressIncompleteAttr)
                                 ]
                 ) $ bar mempoolLabel (lvsMempoolPerc p)
    mempoolLabel = Just $ (show . lvsMempool $ p)
                        ++ " / "
                        ++ take 5 (show $ lvsMempoolPerc p * 100) ++ "%"
    memUsageBar :: forall n. Widget n
    memUsageBar = updateAttrMap
                  (A.mapAttrNames [ (memDoneAttr, P.progressCompleteAttr)
                                  , (memToDoAttr, P.progressIncompleteAttr)
                                  ]
                  ) $ bar memLabel lvsMemUsagePerc
    memLabel = Just $ take 5 (show $ lvsMemoryUsageCurr p) ++ " MB / max " ++ take 5 (show $ lvsMemoryUsageMax p) ++ " MB"
    cpuUsageBar :: forall n. Widget n
    cpuUsageBar = updateAttrMap
                  (A.mapAttrNames [ (cpuDoneAttr, P.progressCompleteAttr)
                                  , (cpuToDoAttr, P.progressIncompleteAttr)
                                  ]
                  ) $ bar cpuLabel (lvsCPUUsagePerc p)
    cpuLabel = Just $ take 5 (show $ lvsCPUUsagePerc p * 100) ++ "%"

    diskUsageRBar :: forall n. Widget n
    diskUsageRBar = updateAttrMap
                    (A.mapAttrNames [ (diskIODoneAttr, P.progressCompleteAttr)
                                    , (diskIOToDoAttr, P.progressIncompleteAttr)
                                    ]
                    ) $ bar diskUsageRLabel (lvsDiskUsageRPerc p)
    diskUsageRLabel = Just $ take 5 (show $ lvsDiskUsageRCurr p) ++ " KB/s"

    diskUsageWBar :: forall n. Widget n
    diskUsageWBar = updateAttrMap
                    (A.mapAttrNames [ (diskIODoneAttr, P.progressCompleteAttr)
                                    , (diskIOToDoAttr, P.progressIncompleteAttr)
                                    ]
                    ) $ bar diskUsageWLabel (lvsDiskUsageWPerc p)
    diskUsageWLabel = Just $ take 5 (show $ lvsDiskUsageWCurr p) ++ " KB/s"

    networkUsageInBar :: forall n. Widget n
    networkUsageInBar = updateAttrMap
                        (A.mapAttrNames [ (networkIODoneAttr, P.progressCompleteAttr)
                                        , (networkIOToDoAttr, P.progressIncompleteAttr)
                                        ]
                        ) $ bar networkUsageInLabel (lvsNetworkUsageInPerc p)
    networkUsageInLabel = Just $ take 5 (show $ lvsNetworkUsageInCurr p) ++ " KB/s"

    networkUsageOutBar :: forall n. Widget n
    networkUsageOutBar = updateAttrMap
                         (A.mapAttrNames [ (networkIODoneAttr, P.progressCompleteAttr)
                                         , (networkIOToDoAttr, P.progressIncompleteAttr)
                                         ]
                         ) $ bar networkUsageOutLabel (lvsNetworkUsageOutPerc p)
    networkUsageOutLabel = Just $ take 5 (show $ lvsNetworkUsageOutCurr p) ++ " KB/s"

    bar :: forall n. Maybe String -> Float -> Widget n
    bar lbl pcntg = P.progressBar lbl pcntg
    lvsMemUsagePerc = lvsMemoryUsageCurr p / max 200 (lvsMemoryUsageMax p)

nodeInfoW :: LiveViewState a -> Widget ()
nodeInfoW p =
      padTop    (T.Pad 2)
    . padLeft   (T.Pad 3)
    . padRight  (T.Pad 3)
    . padBottom (T.Pad 2)
    $ hBox [nodeInfoLabels, nodeInfoValues p]

nodeInfoLabels :: Widget ()
nodeInfoLabels =
      padRight (T.Pad 3)
    $ vBox [                    txt "version:"
           ,                    txt "commit:"
           , padTop (T.Pad 1) $ txt "uptime:"
           , padTop (T.Pad 1) $ txt "epoch / slot:"
           ,                    txt "block number:"
           ,                    txt "chain density:"
           , padTop (T.Pad 1) $ txt "transactions processed:"
           , padTop (T.Pad 1) $ txt "peers connected:"
           ]

nodeInfoValues :: LiveViewState a -> Widget ()
nodeInfoValues lvs =
      withAttr valueAttr
    $ vBox [                    str (lvsVersion lvs)
           ,                    str (take 7 $ lvsCommit lvs)
           , padTop (T.Pad 1) $ str (formatTime defaultTimeLocale "%X" $
                                        -- NominalDiffTime is not an instance of FormatTime before time-1.9.1
                                        addUTCTime (lvsUpTime lvs) (UTCTime (ModifiedJulianDay 0) 0))
           , padTop (T.Pad 1) $ str $ show (lvsEpoch lvs) ++ " / " ++ show (lvsSlotNum lvs)
           ,                    str (show . lvsBlockNum $ lvs)
           ,                    str $ (take 5 . show . lvsChainDensity $ lvs) ++ " %"
           , padTop (T.Pad 1) $ str (show . lvsTransactions $ lvs)
           , padTop (T.Pad 1) $ str (show . lvsPeersConnected $ lvs)
           ]

eventHandler :: LiveViewState a -> BrickEvent n (LiveViewBackend a) -> EventM n (Next (LiveViewState a))
eventHandler prev (AppEvent lvBackend) = do
    next <- liftIO . readMVar . getbe $ lvBackend
    M.continue $ next { lvsColorTheme = lvsColorTheme prev }
eventHandler lvs  (VtyEvent e)         =
    case e of
        V.EvKey  (V.KChar 'q') []        -> stopNodeThread >> M.halt lvs
        V.EvKey  (V.KChar 'Q') []        -> stopNodeThread >> M.halt lvs
        V.EvKey  (V.KChar 'c') [V.MCtrl] -> stopNodeThread >> M.halt lvs
        V.EvKey  (V.KChar 'd') []        -> M.continue $ lvs { lvsColorTheme = DarkTheme }
        V.EvKey  (V.KChar 'D') []        -> M.continue $ lvs { lvsColorTheme = DarkTheme }
        V.EvKey  (V.KChar 'l') []        -> M.continue $ lvs { lvsColorTheme = LightTheme }
        V.EvKey  (V.KChar 'L') []        -> M.continue $ lvs { lvsColorTheme = LightTheme }
        _                                -> M.continue lvs
  where
    stopNodeThread :: MonadIO m => m ()
    stopNodeThread = case lvsNodeThread lvs of
        Nothing -> return ()
        Just t  -> liftIO $ Async.cancel t
eventHandler lvs  _                    = M.halt lvs

app :: M.App (LiveViewState a) (LiveViewBackend a) ()
app =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = eventHandler
          , M.appStartEvent = return
          , M.appAttrMap = \lvs ->
                if lvsColorTheme lvs == DarkTheme
                then themeToAttrMap darkTheme
                else themeToAttrMap lightTheme
          }
