{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module LiveView (
      runNodeLiveView
    ) where

import           Control.Monad (void)
import           Data.Text (unpack)
import           Data.Version (showVersion)
import           Terminal.Game

import           GitRev (gitRev)
import           Ouroboros.Consensus.NodeId
import           Paths_cardano_node (version)
import           Topology

runNodeLiveView :: TopologyInfo -> IO ()
runNodeLiveView topology = void $
    runGame (initLiveViewState topology)
            liveViewLogic
            liveViewDraw
            lvsQuit
            (13 :: FPS)

data LiveViewState = LiveViewState
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
    } deriving (Show, Eq)

initLiveViewState
    :: TopologyInfo
    -> LiveViewState
initLiveViewState (TopologyInfo nodeId _) = LiveViewState
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
    , lvsMempool         = 0
    , lvsMempoolPerc     = 0
    , lvsCPUUsagePerc    = 0
    , lvsMemoryUsageCurr = 0
    , lvsMemoryUsageMax  = 0
    }
  where
    nodeIdNum = case nodeId of
        CoreId num  -> num
        RelayId num -> num

liveViewLogic
    :: LiveViewState
    -> Event
    -> LiveViewState
liveViewLogic lvs (KeyPress 'q') = lvs { lvsQuit = True }
liveViewLogic lvs (KeyPress 'Q') = lvs { lvsQuit = True }
liveViewLogic lvs (KeyPress _)   = lvs
liveViewLogic lvs Tick           = lvs

-- Creates textBox exactly for one text line.
oneLine :: String -> Plane
oneLine s = textBox s ((fromIntegral $ length s) :: Width) (1 :: Height)

makeProgressBar :: Int -> String
makeProgressBar percentage =
    if percentage < 0 || percentage > 100
    then error "Impossible: percentage is out of range!"
    else "[" <> progress <> "]"
  where
    progress       = replicate progressChars '|' ++ replicate emptyChars ' '
    progressChars  = round $ (fromIntegral (percentage `div` percPerChar) :: Double)
    emptyChars     = progressLength - progressChars
    percPerChar    = 100 `div` progressLength
    progressLength = 20

header :: LiveViewState -> Plane
header lvs = blankPlane (85 :: Width) (1 :: Height)
    & (1 :: Row,  1 :: Column) % oneLine "CARDANO SL"
                               # bold
    & (1 :: Row, 17 :: Column) % oneLine ("Release: " <> lvsRelease lvs)
    & (1 :: Row, 76 :: Column) % oneLine ("Node: " <> (show . lvsNodeId $ lvs))

mempoolStats :: LiveViewState -> Plane
mempoolStats lvs = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % oneLine "Memory pool"
                               # bold
    & (1 :: Row, 19 :: Column) % oneLine (
                                            (show . lvsMempool $ lvs)
                                         <> " / "
                                         <> (show . lvsMempoolPerc $ lvs) <> "%"
                                         )
    & (3 :: Row,  3 :: Column) % oneLine (makeProgressBar $ lvsMempoolPerc lvs)

cpuStats :: LiveViewState -> Plane
cpuStats lvs = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % oneLine "CPU usage"
                               # bold
    & (1 :: Row, 19 :: Column) % oneLine ((show . lvsCPUUsagePerc $ lvs) <> "%")
    & (3 :: Row,  3 :: Column) % oneLine (makeProgressBar $ lvsCPUUsagePerc lvs)

memoryStats :: LiveViewState -> Plane
memoryStats lvs = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % oneLine "Memory usage"
                               # bold
    & (1 :: Row, 19 :: Column) % oneLine ((show . lvsMemoryUsageCurr $ lvs) <> " GB")
    & (3 :: Row,  3 :: Column) % oneLine (makeProgressBar $ lvsMemoryUsageCurr lvs)

systemStats :: LiveViewState -> Plane
systemStats lvs = blankPlane (30 :: Width) (17 :: Height)
    & ( 1 :: Row, 1 :: Column) % mempoolStats lvs
    & ( 7 :: Row, 1 :: Column) % cpuStats lvs
    & (13 :: Row, 1 :: Column) % memoryStats lvs

nodeInfoLabels :: Plane
nodeInfoLabels = blankPlane (20 :: Width) (18 :: Height)
    & ( 1 :: Row, 1 :: Column) % oneLine "version:"
    & ( 2 :: Row, 1 :: Column) % oneLine "commit:"
    & ( 4 :: Row, 1 :: Column) % oneLine "uptime:"
    & ( 6 :: Row, 1 :: Column) % oneLine "block height:"
    & ( 7 :: Row, 1 :: Column) % oneLine "minted:"
    & ( 9 :: Row, 1 :: Column) % oneLine "transactions:"
    & (11 :: Row, 1 :: Column) % oneLine "peers connected:"
    & (13 :: Row, 1 :: Column) % oneLine "max network delay:"

nodeInfoValues :: LiveViewState -> Plane
nodeInfoValues lvs = blankPlane (15 :: Width) (18 :: Height)
    & ( 1 :: Row,  1 :: Column) % oneLine (lvsVersion lvs)
                                # bold
    & ( 2 :: Row,  1 :: Column) % oneLine (lvsCommit lvs)
                                # bold
    & ( 4 :: Row,  1 :: Column) % oneLine (lvsUpTime lvs)
                                # bold
    & ( 6 :: Row,  1 :: Column) % oneLine (show . lvsBlockHeight $ lvs)
                                # bold
    & ( 7 :: Row,  1 :: Column) % oneLine (show . lvsBlocksMinted $ lvs)
                                # bold
    & ( 9 :: Row,  1 :: Column) % oneLine (show . lvsTransactions $ lvs)
                                # bold
    & (11 :: Row,  1 :: Column) % oneLine (show . lvsPeersConnected $ lvs)
                                # bold
    & (13 :: Row,  1 :: Column) % oneLine ((show . lvsMaxNetDelay $ lvs) <> " ms")
                                # bold

nodeInfo :: LiveViewState -> Plane
nodeInfo lvs = blankPlane (40 :: Width) (18 :: Height)
    & (1 :: Row,  1 :: Column) % nodeInfoLabels
    & (1 :: Row, 22 :: Column) % nodeInfoValues lvs

liveViewDraw :: LiveViewState -> Plane
liveViewDraw lvs = blankPlane mw mh
    & (1 :: Row,   1 :: Column) % box '*' mw       mh       -- border
    & (2 :: Row,   2 :: Column) % box ' ' (mw - 2) (mh - 2) -- space inside of border
    & (3 :: Row,   7 :: Column) % header lvs
    & (7 :: Row,   9 :: Column) % systemStats lvs
    & (7 :: Row,  55 :: Column) % nodeInfo lvs
  where
    mh = 25 :: Height
    mw = 95 :: Width
