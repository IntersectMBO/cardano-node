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
      runNodeLiveView
    , LiveViewState (..)
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import           Control.Monad (void)
import           Data.Aeson (FromJSON)
import           Data.Text (unpack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Version (showVersion)
import           Terminal.Game

import           GitRev (gitRev)

import           Cardano.BM.Data.Backend
import           Cardano.Shell.Features.Logging (LoggingLayer (..))
import           Ouroboros.Consensus.NodeId
import           Paths_cardano_node (version)
import           Topology


runNodeLiveView :: LoggingLayer -> TopologyInfo -> IO ()
runNodeLiveView ll topology = void $ do
    mv <- newMVar =<< initLiveViewState topology
    let sharedState = LiveViewBackend mv 
    runBackend ll sharedState
    playGame game

  where
      game = Game
          { gScreenWidth   = mw
          , gScreenHeight  = mh
          , gFPS           = 5
          , gInitState     = initLiveViewState topology
          , gLogicFunction = liveViewLogic
          , gDrawFunction  = liveViewDraw
          , gQuitFunction  = lvsQuit
          }

mh :: Height
mh = 25
            
mw :: Width
mw = 95

type LiveViewMVar a = MVar (LiveViewState a)
newtype LiveViewBackend a = LiveViewBackend { getbe :: LiveViewMVar a }

instance (FromJSON a) => IsBackend LiveViewBackend a where
    typeof _ = UserDefinedBK "LiveViewBackend"
    realize _ = return ()
    unrealize be = putStrLn $ "unrealize " <> show (typeof be)

instance IsEffectuator LiveViewBackend a where
    effectuate mlvs _item = do
        modifyMVar_ (mlvs) $ \lvs ->
            return $ lvs { lvsBlockHeight = lvsBlockHeight lvs + 1 }

    handleOverflow _ = return ()


runBackend :: LoggingLayer -> LiveViewBackend a -> IO ()
runBackend ll lvs = do
    return ()
            
data LiveViewState a = LiveViewState
    { lvsQuit            :: Bool
    , lvsRelease         :: String
    , lvsNodeId          :: Int
    , lvsVersion         :: String
    , lvsCommit          :: String
    , lvsUpTime          :: String
    , lvsBlockHeight     :: Int
    , lvsBlocksMinted    :: Int
    , lvsTransactions    :: Int
    , lvsPeersConnected  :: Int
    , lvsMaxNetDelay     :: Int
    , lvsMempool         :: Int
    , lvsMempoolPerc     :: Int
    , lvsCPUUsagePerc    :: Int
    , lvsMemoryUsageCurr :: Int
    , lvsMemoryUsageMax  :: Int
    -- internal state
    , lvsStartTime       :: UTCTime
    } deriving (Show, Eq)

initLiveViewState
    :: forall a .
       TopologyInfo
    -> IO (LiveViewBackend a)
initLiveViewState (TopologyInfo nodeId _) = return $ LiveViewState
    { lvsQuit            = False
    , lvsRelease         = "Shelley"   -- Should be taken from ..?
    , lvsNodeId          = nodeIdNum
    , lvsVersion         = showVersion version
    , lvsCommit          = unpack gitRev
    , lvsUpTime          = "00:00:00"
    , lvsBlockHeight     = 0
    , lvsBlocksMinted    = 0
    , lvsTransactions    = 0
    , lvsPeersConnected  = 0
    , lvsMaxNetDelay     = 0
    , lvsMempool         = 95
    , lvsMempoolPerc     = 79
    , lvsCPUUsagePerc    = 58
    , lvsMemoryUsageCurr = 3
    , lvsMemoryUsageMax  = 0
    , lvsStartTime       = getCurrentTime
    }
  where
    nodeIdNum = case nodeId of
        CoreId num  -> num
        RelayId num -> num

liveViewLogic
    :: LiveViewBackend a
    -> Event
    -> LiveViewBackend a
liveViewLogic lvbe (KeyPress 'q') = modifyMVar_ (getbe lvbe) $ \lvs -> lvs { lvsQuit = True }
liveViewLogic lvbe (KeyPress 'Q') = lvs { lvsQuit = True }
liveViewLogic lvbe (KeyPress _)   = lvs
liveViewLogic lvbe Tick           = lvs

makeProgressBar :: Int -> Plane
makeProgressBar percentage =
    if percentage < 0 || percentage > 100
    then error "Impossible: percentage is out of range!"
    else blankPlane fullLength (1 :: Height)
       & (1 :: Row, 1 :: Column) % stringPlane "["
                                 # bold
       & (1 :: Row, 2 :: Column) % stringPlane progress
                                 # color progressColor Vivid
       & (1 :: Row, fullLength)  % stringPlane "]"
                                 # bold
  where
    progress       = replicate progressChars '|' ++ replicate emptyChars ' '
    progressColor  = if | percentage <= 50 -> Green
                        | percentage <= 75 -> Yellow
                        | otherwise        -> Red
    progressChars' = round $ (fromIntegral (percentage `div` percPerChar) :: Double)
    progressChars  = if progressChars' == 0 then 1 else progressChars'
    emptyChars     = progressLength - progressChars
    percPerChar    = 100 `div` progressLength
    progressLength = 20
    fullLength :: Integer
    fullLength = fromIntegral progressLength + 2

header :: LiveViewBackend a -> Plane
header lvbe = blankPlane (85 :: Width) (1 :: Height)
    & (1 :: Row,  1 :: Column) % stringPlane "CARDANO SL"
                               # bold
    & (1 :: Row, 17 :: Column) % stringPlane ("Release: ")
    & (1 :: Row, 26 :: Column) % (stringPlane $ lvsRelease lvs)
                               # color Cyan Vivid
    & (1 :: Row, 76 :: Column) % stringPlane ("Node: ")
    & (1 :: Row, 82 :: Column) % (stringPlane . show $ lvsNodeId lvs)
                               # color Cyan Vivid
                               # bold

mempoolStats :: LiveViewBackend a -> Plane
mempoolStats lvbe = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % stringPlane "Memory pool"
                               # bold
    & (1 :: Row, 19 :: Column) % stringPlane (
                                            (show . lvsMempool $ lvs)
                                         <> " / "
                                         <> (show . lvsMempoolPerc $ lvs) <> "%"
                                         )
                               # color White Vivid
    & (3 :: Row,  3 :: Column) % (makeProgressBar $ lvsMempoolPerc lvs)

cpuStats :: LiveViewBackend a -> Plane
cpuStats lvbe = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % stringPlane "CPU usage"
                               # bold
    & (1 :: Row, 19 :: Column) % stringPlane ((show . lvsCPUUsagePerc $ lvs) <> "%")
                               # color White Vivid
    & (3 :: Row,  3 :: Column) % (makeProgressBar $ lvsCPUUsagePerc lvs)

memoryStats :: LiveViewBackend a -> Plane
memoryStats lvbe = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % stringPlane "Memory usage"
                               # bold
    & (1 :: Row, 19 :: Column) % stringPlane ((show . lvsMemoryUsageCurr $ lvs) <> " GB")
                               # color White Vivid
    & (3 :: Row,  3 :: Column) % (makeProgressBar $ lvsMemoryUsageCurr lvs)

systemStats :: LiveViewBackend a -> Plane
systemStats lvbe = blankPlane (30 :: Width) (17 :: Height)
    & ( 1 :: Row, 1 :: Column) % mempoolStats lvs
    & ( 7 :: Row, 1 :: Column) % cpuStats lvs
    & (13 :: Row, 1 :: Column) % memoryStats lvs

nodeInfoLabels :: Plane
nodeInfoLabels = blankPlane (20 :: Width) (18 :: Height)
    & ( 1 :: Row, 1 :: Column) % stringPlane "version:"
    & ( 2 :: Row, 1 :: Column) % stringPlane "commit:"
    & ( 4 :: Row, 1 :: Column) % stringPlane "uptime:"
    & ( 6 :: Row, 1 :: Column) % stringPlane "block height:"
    & ( 7 :: Row, 1 :: Column) % stringPlane "minted:"
    & ( 9 :: Row, 1 :: Column) % stringPlane "transactions:"
    & (11 :: Row, 1 :: Column) % stringPlane "peers connected:"
    & (13 :: Row, 1 :: Column) % stringPlane "max network delay:"

nodeInfoValues :: LiveViewBackend a -> Plane
nodeInfoValues lvs = blankPlane (15 :: Width) (18 :: Height)
    & ( 1 :: Row,  1 :: Column) % stringPlane (lvsVersion lvs)
                                # color White Vivid # bold
    & ( 2 :: Row,  1 :: Column) % stringPlane (lvsCommit lvs)
                                # color White Vivid # bold
    & ( 4 :: Row,  1 :: Column) % stringPlane (lvsUpTime lvs)
                                # color White Vivid # bold
    & ( 6 :: Row,  1 :: Column) % stringPlane (show . lvsBlockHeight $ lvs)
                                # color White Vivid # bold
    & ( 7 :: Row,  1 :: Column) % stringPlane (show . lvsBlocksMinted $ lvs)
                                # color White Vivid # bold
    & ( 9 :: Row,  1 :: Column) % stringPlane (show . lvsTransactions $ lvs)
                                # color White Vivid # bold
    & (11 :: Row,  1 :: Column) % stringPlane (show . lvsPeersConnected $ lvs)
                                # color White Vivid # bold
    & (13 :: Row,  1 :: Column) % stringPlane ((show . lvsMaxNetDelay $ lvs) <> " ms")
                                # color White Vivid # bold

nodeInfo :: LiveViewBackend a -> Plane
nodeInfo lvbe = blankPlane (40 :: Width) (18 :: Height)
    & (1 :: Row,  1 :: Column) % nodeInfoLabels
    & (1 :: Row, 22 :: Column) % nodeInfoValues lvs

liveViewDraw :: LiveViewBackend a -> Plane
liveViewDraw lvbe = blankPlane mw mh
    & (1 :: Row,   1 :: Column) % box '*' mw       mh       -- border
    & (2 :: Row,   2 :: Column) % box ' ' (mw - 2) (mh - 2) -- space inside of border
    & (3 :: Row,   7 :: Column) % header lvs
    & (7 :: Row,   9 :: Column) % systemStats lvs
    & (7 :: Row,  55 :: Column) % nodeInfo lvs
