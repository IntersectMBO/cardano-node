{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Node.TUI.EventHandler
  ( ColorTheme(..)
  , LiveViewBackend(..)
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Brick.BChan (newBChan, writeBChan)
import           Brick.Main (App (..), continue, customMain, halt, showFirstCursor)
import           Brick.Themes (themeToAttrMap)
import           Brick.Types (BrickEvent (..), EventM, Next)
import qualified Control.Concurrent.Async as Async
import qualified Data.Text as Text
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Version (showVersion)
import qualified Graphics.Vty as Vty

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Backend (BackendKind (..), IsBackend (..), IsEffectuator (..))
import           Cardano.BM.Data.Counter (Platform (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..), LogObject (..), utc2ns)
import           Cardano.Config.Git.Rev (gitRev)
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.TUI.Drawing (ColorTheme (..), LiveViewState (..), LiveViewThread (..),
                     Screen (..), darkTheme, drawUI, lightTheme)

import           Paths_cardano_node (version)


type LiveViewMVar blk a = MVar (LiveViewState blk a)

newtype LiveViewBackend blk a = LiveViewBackend { getbe :: LiveViewMVar blk a }

instance IsEffectuator (LiveViewBackend blk) Text where
    effectuate lvbe item = do
        checkForUnexpectedThunks ["IsEffectuator LiveViewBackend"] lvbe

        case item of
            LogObject "cardano.node.metrics" meta content -> do
                let currentTimeInNs = utc2ns (tstamp meta)
                case content of
                    LogValue "Mem.resident" (PureI pages) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                            let !mbytes     = fromIntegral (pages * pagesize) / 1024 / 1024 :: Float
                            let !maxMemory  = max (lvsMemoryUsageMax lvs) mbytes
                            let !uptime     = diffUTCTime (tstamp meta) (lvsStartTime lvs)

                            checkForUnexpectedThunks ["Meresident LiveViewBackend"] lvs

                            return $ lvs { lvsMemoryUsageCurr = mbytes
                                         , lvsMemoryUsageMax  = maxMemory
                                         , lvsUpTime          = uptime
                                         }

                    LogValue "Mem.resident_size" (Bytes bytes) ->   -- Darwin
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                            let !mbytes     = fromIntegral bytes / 1024 / 1024 :: Float
                            let !maxMemory  = max (lvsMemoryUsageMax lvs) mbytes
                            let !uptime     = diffUTCTime (tstamp meta) (lvsStartTime lvs)

                            checkForUnexpectedThunks ["Meresident_size LiveViewBackend"] lvs

                            return $ lvs { lvsMemoryUsageCurr = mbytes
                                         , lvsMemoryUsageMax  = maxMemory
                                         , lvsUpTime          = uptime
                                         }

                    LogValue "IO.rchar" (Bytes bytesWereRead) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                            let !timeDiff           = fromIntegral (currentTimeInNs - lvsDiskUsageRNs lvs) :: Float
                                !timeDiffInSecs     = timeDiff / 1000000000
                                !bytesDiff          = fromIntegral (bytesWereRead - lvsDiskUsageRLast lvs) :: Float
                                !bytesDiffInKB      = bytesDiff / 1024
                                !currentDiskRate    = bytesDiffInKB / timeDiffInSecs
                                !maxDiskRate        = max currentDiskRate $ lvsDiskUsageRMax lvs
                                !upTime             = diffUTCTime (tstamp meta) (lvsStartTime lvs)
                                !diskUsageRPerc     = (currentDiskRate / (maxDiskRate / 100.0)) / 100.0

                            checkForUnexpectedThunks ["IO.rchar LiveViewBackend"] lvs

                            return $ lvs { lvsDiskUsageRCurr = currentDiskRate
                                         , lvsDiskUsageRPerc = diskUsageRPerc
                                         , lvsDiskUsageRLast = bytesWereRead
                                         , lvsDiskUsageRNs   = currentTimeInNs
                                         , lvsDiskUsageRMax  = maxDiskRate
                                         , lvsUpTime         = upTime
                                         }

                    LogValue "IO.wchar" (Bytes bytesWereWritten) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                            let !timeDiff        = fromIntegral (currentTimeInNs - lvsDiskUsageWNs lvs) :: Float
                                !timeDiffInSecs  = timeDiff / 1000000000
                                !bytesDiff       = fromIntegral (bytesWereWritten - lvsDiskUsageWLast lvs) :: Float
                                !bytesDiffInKB   = bytesDiff / 1024
                                !currentDiskRate = bytesDiffInKB / timeDiffInSecs
                                !maxDiskRate     = max currentDiskRate $ lvsDiskUsageWMax lvs

                            checkForUnexpectedThunks ["IO.wchar LiveViewBackend"] lvs

                            return $! lvs { lvsDiskUsageWCurr = currentDiskRate
                                         , lvsDiskUsageWPerc = (currentDiskRate / (maxDiskRate / 100.0)) / 100.0
                                         , lvsDiskUsageWLast = bytesWereWritten
                                         , lvsDiskUsageWNs   = currentTimeInNs
                                         , lvsDiskUsageWMax  = maxDiskRate
                                         , lvsUpTime         = diffUTCTime (tstamp meta) (lvsStartTime lvs)
                                         }

                    LogValue "Stat.utime" (PureI ticks) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                            let !tdiff      = max 1 $ (fromIntegral (currentTimeInNs - lvsCPUUsageNs lvs)) / 1000000000 :: Float
                                !cpuperc    = (fromIntegral (ticks - lvsCPUUsageLast lvs)) / (fromIntegral clktck) / tdiff
                                !uptime     = diffUTCTime (tstamp meta) (lvsStartTime lvs)

                            checkForUnexpectedThunks ["Stat.utime LiveViewBackend"] lvs

                            return $ lvs { lvsCPUUsagePerc = cpuperc
                                         , lvsCPUUsageLast = ticks
                                         , lvsCPUUsageNs   = currentTimeInNs
                                         , lvsUpTime       = uptime
                                         }

                    LogValue "Sys.SysUserTime" (Nanoseconds nsecs) ->   -- Darwin, Windows
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                            let !tdiff      = max 1 $ (fromIntegral (currentTimeInNs - lvsCPUUsageNs lvs)) :: Float
                                !deltacpu   = fromIntegral nsecs - fromIntegral (lvsCPUUsageLast lvs) :: Float
                                !cpuperc    = deltacpu * 10 / tdiff
                                !uptime     = diffUTCTime (tstamp meta) (lvsStartTime lvs)

                            checkForUnexpectedThunks ["Sys.SysUserTime LiveViewBackend"] lvs

                            return $ lvs { lvsCPUUsagePerc = cpuperc
                                         , lvsCPUUsageLast = fromIntegral nsecs
                                         , lvsCPUUsageNs   = currentTimeInNs
                                         , lvsUpTime       = uptime
                                         }

                    LogValue "Net.ifd_0-ibytes" (Bytes inBytes) ->  -- Darwin
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                          updateNetInbound lvs currentTimeInNs inBytes  -- Linux
                    LogValue "Net.IpExt:InOctets" (Bytes inBytes) ->
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                          updateNetInbound lvs currentTimeInNs inBytes

                    LogValue "Net.ifd_0-obytes" (Bytes outBytes) ->   -- Darwin
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                          updateNetOutbound lvs currentTimeInNs outBytes
                    LogValue "Net.IpExt:OutOctets" (Bytes outBytes) ->
                        modifyMVar_ (getbe lvbe) $ \lvs ->
                          updateNetOutbound lvs currentTimeInNs outBytes

                    LogValue "Sys.Platform" (PureI pfid) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                            let pltfrm = toEnum $ fromIntegral pfid :: Platform

                            checkForUnexpectedThunks ["Sys.Platform LiveViewBackend"] lvs

                            return $ lvs { lvsPlatform = show pltfrm }

                    LogValue "txsInMempool" (PureI txsInMempool) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                                let lvsMempool' = fromIntegral txsInMempool :: Word64
                                    maxTxs = max lvsMempool' (lvsMempoolMaxTxs lvs)
                                    percentage = fromIntegral lvsMempool' / fromIntegral maxTxs :: Float

                                checkForUnexpectedThunks ["txsInMempool LiveViewBackend"] lvs

                                return $ lvs { lvsMempool = lvsMempool'
                                             , lvsMempoolMaxTxs = maxTxs
                                             , lvsMempoolPerc = percentage
                                             }
                    LogValue "mempoolBytes" (PureI mempoolBytes) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do
                                let lvsMempoolBytes' = fromIntegral mempoolBytes :: Word64
                                    maxBytes = max lvsMempoolBytes' (lvsMempoolMaxBytes lvs)
                                    percentage = fromIntegral lvsMempoolBytes' / fromIntegral maxBytes :: Float

                                checkForUnexpectedThunks ["mempoolBytes LiveViewBackend"] lvs

                                return $ lvs
                                    { lvsMempoolBytes = lvsMempoolBytes'
                                    , lvsMempoolMaxBytes = maxBytes
                                    , lvsMempoolBytesPerc = percentage
                                    }
                    LogValue "txsProcessedNum" (PureI txsProcessedNum) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do

                                checkForUnexpectedThunks ["txsProcessedNum LiveViewBackend"] lvs

                                return $ lvs { lvsTransactions = fromIntegral txsProcessedNum }

                    LogValue "blocksForgedNum" (PureI forgedBlocksNum) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do

                                checkForUnexpectedThunks ["blocksForgedNum LiveViewBackend"] lvs

                                return $ lvs { lvsBlocksMinted = fromIntegral forgedBlocksNum }
                    LogValue "nodeCannotLead" (PureI nodeCannotLead) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do

                                checkForUnexpectedThunks ["nodeCannotLead LiveViewBackend"] lvs

                                return $ lvs { lvsNodeCannotLead = fromIntegral nodeCannotLead }
                    LogValue "nodeIsLeaderNum" (PureI leaderNum) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do

                                checkForUnexpectedThunks ["nodeIsLeaderNum LiveViewBackend"] lvs

                                return $ lvs { lvsLeaderNum = fromIntegral leaderNum }

                    LogValue "slotsMissedNum" (PureI missedSlotsNum) ->
                        modifyMVar_ (getbe lvbe) $ \lvs -> do

                                checkForUnexpectedThunks ["slotsMissedNum LiveViewBackend"] lvs

                                return $ lvs { lvsSlotsMissedNum = fromIntegral missedSlotsNum }

                    _ -> pure ()

                checkForUnexpectedThunks ["Cardano node metrics dispatch LiveViewBackend"] lvbe

            LogObject _ _ (LogValue "density" (PureD density)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do

                        let !chainDensity = 0.05 + density * 100.0

                        checkForUnexpectedThunks ["density LiveViewBackend"] lvs

                        return $ lvs { lvsChainDensity = chainDensity }
            LogObject _ _ (LogValue "connectedPeers" (PureI npeers)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do

                        checkForUnexpectedThunks ["connectedPeers LiveViewBackend"] lvs

                        return $ lvs { lvsPeersConnected = fromIntegral npeers }
            LogObject _ _ (LogValue "blockNum" (PureI slotnum)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do

                        checkForUnexpectedThunks ["blockNum LiveViewBackend"] lvs

                        return $ lvs { lvsBlockNum = fromIntegral slotnum }
            LogObject _ _ (LogValue "slotInEpoch" (PureI slotnum)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do

                        checkForUnexpectedThunks ["slotInEpoch LiveViewBackend"] lvs

                        return $ lvs { lvsSlotNum = fromIntegral slotnum }
            LogObject _ _ (LogValue "epoch" (PureI epoch)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do

                        checkForUnexpectedThunks ["epoch LiveViewBackend"] lvs

                        return $ lvs { lvsEpoch = fromIntegral epoch }

            LogObject _ _ (LogValue "operationalCertificateStartKESPeriod" (PureI oCertStartKesPeriod)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do

                        checkForUnexpectedThunks ["operationalCertificateStartKESPeriod LiveViewBackend"] lvs

                        return $ lvs { lvsOpCertStartKESPeriod = fromIntegral oCertStartKesPeriod }

            LogObject _ _ (LogValue "currentKESPeriod" (PureI currentKesPeriod)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do

                        checkForUnexpectedThunks ["currentKESPeriod LiveViewBackend"] lvs

                        return $ lvs { lvsCurrentKESPeriod = fromIntegral currentKesPeriod }

            LogObject _ _ (LogValue "remainingKESPeriods" (PureI kesPeriodsUntilExpiry)) ->
                modifyMVar_ (getbe lvbe) $ \lvs -> do

                        checkForUnexpectedThunks ["remainingKESPeriods LiveViewBackend"] lvs

                        return $ lvs { lvsRemainingKESPeriods = fromIntegral kesPeriodsUntilExpiry }

            _ -> pure ()

    handleOverflow _ = pure ()

instance IsBackend (LiveViewBackend blk) Text where
    bekind _ = UserDefinedBK "LiveViewBackend"
    realize _ = do
        !initState <- initLiveViewState
        !mv <- newMVar initState
        let !sharedState = LiveViewBackend mv
        thr <- Async.async $ do
            eventChan <- newBChan 10
            let buildVty = Vty.mkVty Vty.defaultConfig
            initialVty <- buildVty
            ticker <- Async.async $ forever $ do
                        -- could be replaced by retry if we have TVar-like vars
                        threadDelay 800000 -- refresh TUI every 800 ms
                        writeBChan eventChan $ LiveViewBackend mv
            Async.link ticker
            void $ customMain initialVty buildVty (Just eventChan) app initState

        modifyMVar_ mv $ \lvs -> return $ lvs { lvsUIThread = LiveViewThread $ Just thr }

        checkForUnexpectedThunks ["IsBackend LiveViewBackend"] sharedState

        return sharedState

    unrealize be = putStrLn $ "unrealize " ++ show (bekind be)


instance NoUnexpectedThunks a => NoUnexpectedThunks (LiveViewBackend blk a) where
  showTypeOf _ = "LiveViewBackend"
  whnfNoUnexpectedThunks ctxt liveViewBackend = do
    let liveViewMVar = getbe liveViewBackend
    a <- takeMVar liveViewMVar
    result <- noUnexpectedThunks ctxt a
    putMVar liveViewMVar a
    return result

eventHandler :: NFData a => LiveViewState blk a -> BrickEvent n (LiveViewBackend blk a) -> EventM n (Next (LiveViewState blk a))
eventHandler prev (AppEvent lvBackend) = do
    next <- liftIO . readMVar . getbe $ lvBackend
    continue $ next
      { lvsColorTheme = lvsColorTheme prev
      , lvsScreen = lvsScreen prev
      }
eventHandler lvs  (VtyEvent e)         =
    case e of
        Vty.EvKey  (Vty.KChar 'q') []        -> stopNodeThread >> halt lvs
        Vty.EvKey  (Vty.KChar 'Q') []        -> stopNodeThread >> halt lvs
        Vty.EvKey  (Vty.KChar 'c') [Vty.MCtrl] -> stopNodeThread >> halt lvs
        Vty.EvKey  (Vty.KChar 'd') []        -> continue $ lvs { lvsColorTheme = DarkTheme }
        Vty.EvKey  (Vty.KChar 'D') []        -> continue $ lvs { lvsColorTheme = DarkTheme }
        Vty.EvKey  (Vty.KChar 'l') []        -> continue $ lvs { lvsColorTheme = LightTheme }
        Vty.EvKey  (Vty.KChar 'L') []        -> continue $ lvs { lvsColorTheme = LightTheme }
        Vty.EvKey  (Vty.KChar 'p') []        -> continue $ lvs { lvsScreen = Peers }
        Vty.EvKey  (Vty.KChar 'P') []        -> continue $ lvs { lvsScreen = Peers }
        Vty.EvKey  Vty.KEsc        []        -> continue $ lvs { lvsScreen = MainView }
        _                                -> continue lvs
  where
    stopNodeThread :: MonadIO m => m (Async ())
    stopNodeThread = liftIO $ Async.async $ do
      -- Because of this 100ms delay, Vty will be halted _before_ stopping node's thread.
      -- It keeps the terminal in the normal state after quitting.
      threadDelay 100000
      case getLVThread (lvsNodeThread lvs) of
        Nothing -> return ()
        Just t  -> liftIO $ Async.cancel t
eventHandler lvs  _                    = halt lvs

initLiveViewState :: IO (LiveViewState blk a)
initLiveViewState = do
    now <- getCurrentTime
    return $ LiveViewState
                { lvsScreen                 = MainView
                , lvsRelease                = "Release not set yet"
                , lvsProtocol               = CardanoProtocol -- Needs a real value. Will be overwritten later.
                , lvsNodeId                 = "NodeId not set yet"
                , lvsVersion                = showVersion version
                , lvsCommit                 = Text.unpack gitRev
                , lvsPlatform               = "Platform not set yet"
                , lvsUpTime                 = diffUTCTime now now
                , lvsEpoch                  = 0
                , lvsSlotNum                = 0
                , lvsBlockNum               = 0
                , lvsChainDensity           = 0.0
                , lvsBlocksMinted           = 0
                , lvsNodeCannotLead         = 0
                , lvsLeaderNum              = 0
                , lvsSlotsMissedNum         = 0
                , lvsTransactions           = 0
                , lvsPeersConnected         = 0
                , lvsMempool                = 0
                , lvsMempoolPerc            = 0.0
                , lvsMempoolBytes           = 0
                , lvsMempoolBytesPerc       = 0.0
                , lvsCPUUsagePerc           = 0.0
                , lvsMemoryUsageCurr        = 0.0
                , lvsMemoryUsageMax         = 0.2
                , lvsDiskUsageRPerc         = 0.0
                , lvsDiskUsageRCurr         = 0.0
                , lvsDiskUsageRMax          = 0.0
                , lvsDiskUsageWPerc         = 0.0
                , lvsDiskUsageWCurr         = 0.0
                , lvsDiskUsageWMax          = 0.0
                , lvsNetworkUsageInPerc     = 0.0
                , lvsNetworkUsageInCurr     = 0.0
                , lvsNetworkUsageInMax      = 0.0
                , lvsNetworkUsageOutPerc    = 0.0
                , lvsNetworkUsageOutCurr    = 0.0
                , lvsNetworkUsageOutMax     = 0.0
                , lvsStartTime              = now
                , lvsCPUUsageLast           = 0
                , lvsCPUUsageNs             = 10000
                , lvsDiskUsageRLast         = 0
                , lvsDiskUsageRNs           = 10000
                , lvsDiskUsageWLast         = 0
                , lvsDiskUsageWNs           = 10000
                , lvsNetworkUsageInLast     = 0
                , lvsNetworkUsageInNs       = 10000
                , lvsNetworkUsageOutLast    = 0
                , lvsNetworkUsageOutNs      = 10000
                , lvsMempoolMaxTxs          = 0
                , lvsMempoolMaxBytes        = 0
                , lvsOpCertStartKESPeriod   = 9999999999
                , lvsCurrentKESPeriod       = 9999999999
                , lvsRemainingKESPeriods    = 9999999999
                , lvsMessage                = Nothing
                , lvsUIThread               = LiveViewThread Nothing
                , lvsMetricsThread          = LiveViewThread Nothing
                , lvsNodeThread             = LiveViewThread Nothing
                , lvsPeers                  = mempty
                , lvsColorTheme             = DarkTheme
                }

updateNetInbound :: Monad m => LiveViewState blk a -> Word64 -> Word64 -> m (LiveViewState blk a)
updateNetInbound lvs currentTimeInNs inBytes =
    let (_timeDiff, currentNetRate) =
            calcNetRate currentTimeInNs (lvsNetworkUsageInNs lvs) (lvsNetworkUsageInLast lvs) inBytes
        maxNetRate = max currentNetRate (lvsNetworkUsageInMax lvs)
    in return $ lvs { lvsNetworkUsageInCurr = currentNetRate
                    , lvsNetworkUsageInPerc = (currentNetRate / (maxNetRate / 100.0)) / 100.0
                    , lvsNetworkUsageInLast = inBytes
                    , lvsNetworkUsageInNs   = currentTimeInNs
                    , lvsNetworkUsageInMax  = maxNetRate
                    }

updateNetOutbound :: Monad m => LiveViewState blk a -> Word64 -> Word64 -> m (LiveViewState blk a)
updateNetOutbound lvs currentTimeInNs outBytes =
    let (_timeDiff, currentNetRate) =
            calcNetRate currentTimeInNs (lvsNetworkUsageOutNs lvs) (lvsNetworkUsageOutLast lvs) outBytes
        maxNetRate = max currentNetRate (lvsNetworkUsageOutMax lvs)
    in return lvs { lvsNetworkUsageOutCurr = currentNetRate
                  , lvsNetworkUsageOutPerc = (currentNetRate / (maxNetRate / 100.0)) / 100.0
                  , lvsNetworkUsageOutLast = outBytes
                  , lvsNetworkUsageOutNs   = currentTimeInNs
                  , lvsNetworkUsageOutMax  = maxNetRate
                  }

-------------------------------------------------------------------------------
-- Application
-------------------------------------------------------------------------------

app :: NFData a => App (LiveViewState blk a) (LiveViewBackend blk a) ()
app =
    App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = eventHandler
          , appStartEvent = return
          , appAttrMap = \lvs ->
                if lvsColorTheme lvs == DarkTheme
                then themeToAttrMap darkTheme
                else themeToAttrMap lightTheme
          }

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- constants, to be evaluated from host system

-- getconf PAGESIZE
pagesize :: Integer
pagesize = 4096

-- getconf CLK_TCK
clktck :: Integer
clktck = 100

calcNetRate :: (Integral a1, Integral a2) => a2 -> a2 -> a1 -> a1 -> (Float, Float)
calcNetRate currentTimeInNs lastTimeInNs lastUsageBytes bytes =
    let timeDiff        = fromIntegral (currentTimeInNs - lastTimeInNs) :: Float
        timeDiffInSecs  = timeDiff / 1000000000
        bytesDiff       = fromIntegral (bytes - lastUsageBytes) :: Float
        bytesDiffInKB   = bytesDiff / 1024
        currentNetRate  = bytesDiffInKB / timeDiffInSecs
    in (timeDiff, currentNetRate)


-- | Check for unexpected thunks.
-- Should NOT be used in PRODUCTION!
checkForUnexpectedThunks :: NoUnexpectedThunks a => [String] -> a -> IO ()
#ifdef UNEXPECTED_THUNKS
checkForUnexpectedThunks context unexpectedThunks = do

    noUnexpectedThunks' <- noUnexpectedThunks context unexpectedThunks

    unless (thunkInfoToIsNF noUnexpectedThunks') $ do
        let showedUnexpectedThunks = show noUnexpectedThunks'
        panic $ "Unexpected thunks! " <> toS showedUnexpectedThunks
#else
checkForUnexpectedThunks _context _unexpectedThunks = pure ()
#endif
