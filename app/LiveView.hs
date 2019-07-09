{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module LiveView (
      LiveViewBackend (..)
    , realize
    , effectuate
    ) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import           Brick.Types (Widget)
import qualified Brick.Types as T
import           Brick.Util (bg, clamp, fg, on)
import           Brick.Widgets.Core (hBox, hLimitPercent, overrideAttr,
                                     padBottom, padLeft, padRight, padTop, str,
                                     updateAttrMap, vBox, vLimitPercent, (<+>),
                                     (<=>))
import           Control.Applicative ((<$>))

import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import           Brick.Types (Widget)
import           Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core (hBox, hLimit, str, txt, updateAttrMap,
                                     vLimit, withAttr, withBorderStyle, (<+>),
                                     (<=>))
import qualified Brick.Widgets.ProgressBar as P
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import           Control.Monad (void)
import           Data.Aeson (FromJSON)
import           Data.Text (unpack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Version (showVersion)
import qualified Graphics.Vty as V

import           GitRev (gitRev)

import           Cardano.BM.Data.Backend
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
        thr <- Async.async $ do
            void $ M.defaultMain theApp initState
            return ()
        modifyMVar_ mv $ \lvs -> return $ lvs { lvsThread = thr }
        return $ sharedState

    unrealize be = putStrLn $ "unrealize " <> show (typeof be)

instance IsEffectuator LiveViewBackend a where
    effectuate lvbe _item = do
        modifyMVar_ (getbe lvbe) $ \lvs ->
            return $ lvs { lvsBlockHeight = lvsBlockHeight lvs + 1 }

    handleOverflow _ = return ()


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
    , lvsMempoolPerc     :: Float
    , lvsCPUUsagePerc    :: Float
    , lvsMemoryUsageCurr :: Int
    , lvsMemoryUsageMax  :: Int
    -- internal state
    , lvsStartTime       :: UTCTime
    , lvsMessage         :: a
    , lvsThread          :: Async.Async ()
    } deriving (Eq)

initLiveViewState :: IO (LiveViewState a)
initLiveViewState = do
    tstamp <- getCurrentTime
    return $ LiveViewState
                { lvsQuit            = False
                , lvsRelease         = "Shelley"
                , lvsNodeId          = 99
                , lvsVersion         = showVersion version
                , lvsCommit          = unpack gitRev
                , lvsUpTime          = "00:00:00"
                , lvsBlockHeight     = 1891
                , lvsBlocksMinted    = 543
                , lvsTransactions    = 1732
                , lvsPeersConnected  = 3
                , lvsMaxNetDelay     = 17
                , lvsMempool         = 50
                , lvsMempoolPerc     = 0.25
                , lvsCPUUsagePerc    = 0.58
                , lvsMemoryUsageCurr = 3
                , lvsMemoryUsageMax  = 5
                , lvsStartTime       = tstamp
                }

setTopology :: LiveViewBackend a -> TopologyInfo -> IO ()
setTopology lvbe (TopologyInfo nodeId _) =
    modifyMVar_ (getbe lvbe) $ \lvs ->
        return $ lvs { lvsNodeId = nodeIdNum }
  where
      nodeIdNum = case nodeId of
          CoreId num  -> num
          RelayId num -> num

drawUI :: LiveViewState a -> [Widget ()]
drawUI p = [mainWidget p]

mainWidget :: LiveViewState a -> Widget ()
mainWidget p =
      C.hCenter
    . C.vCenter
    . hLimitPercent 50
    . vLimitPercent 80
    $ mainContentW p

mainContentW :: LiveViewState a -> Widget ()
mainContentW p =
    withBorderStyle BS.unicode
    . B.border $ vBox
        [ headerW
        , hBox [systemStatsW p, nodeInfoW p]
        ]

titleAttr :: A.AttrName
titleAttr = "title"

valueAttr :: A.AttrName
valueAttr = "value"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (titleAttr, fg V.cyan)
    , (valueAttr, fg V.white)
    ]

headerW :: Widget ()
headerW =
      C.hCenter
    . padTop   (T.Pad 1)
    . padLeft  (T.Pad 3)
    . padRight (T.Pad 3)
    $ hBox [ padRight (T.Pad 10) $ txt "CARDANO SL"
           , txt "release: "
           , releaseW
           , padLeft T.Max $ txt "Node: "
           , nodeIdW
           ]
  where
    releaseW =   updateAttrMap (A.applyAttrMappings borderMappings)
               $ withAttr titleAttr
               $ txt "Shelley"
    nodeIdW  =   updateAttrMap (A.applyAttrMappings borderMappings)
               $ withAttr titleAttr
               $ txt "0"

systemStatsW :: LiveViewState a -> Widget ()
systemStatsW p =
      padTop   (T.Pad 2)
    . padLeft  (T.Pad 3)
    . padRight (T.Pad 3)
    $ vBox [ vBox [ padBottom (T.Pad 1) $ txt "Memory pool:"
                  , padBottom (T.Pad 2) $ memPoolBar
                  ]
           , vBox [ padBottom (T.Pad 1) $ txt "CPU usage:"
                  , padBottom (T.Pad 2) $ cpuUsageBar
                  ]
           ]
  where
    -- use mapAttrNames
    memPoolBar = updateAttrMap
                 (A.mapAttrNames [ (mempoolDoneAttr, P.progressCompleteAttr)
                                 , (mempoolToDoAttr, P.progressIncompleteAttr)
                                 ]
                 ) $ bar mempoolLabel lvsMempoolPerc
    mempoolLabel = Just $ (show . lvsMempool $ p)
                        ++ " / "
                        ++ (show . lvsMempoolPerc $ p) ++ "%"
    cpuUsageBar = updateAttrMap
                  (A.mapAttrNames [ (cpuDoneAttr, P.progressCompleteAttr)
                                  , (cpuToDoAttr, P.progressIncompleteAttr)
                                  ]
                  ) $ bar cpuLabel lvsCPUUsagePerc
    cpuLabel = Just $ (show . lvsCPUUsagePerc $ p) ++ "%"
    bar lbl pcntg = P.progressBar lbl (pcntg p)

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
           , padTop (T.Pad 1) $ txt "block height:"
           ,                    txt "minted:"
           , padTop (T.Pad 1) $ txt "transactions:"
           , padTop (T.Pad 1) $ txt "peers connected:"
           , padTop (T.Pad 1) $ txt "max network delay:"
           ]

nodeInfoValues :: LiveViewState a -> Widget ()
nodeInfoValues lvs =
      updateAttrMap (A.applyAttrMappings borderMappings)
    . withAttr valueAttr
    $ vBox [                    str (lvsVersion lvs)
           ,                    str (take 7 $ lvsCommit lvs) -- Probably we don't need the full commit
           , padTop (T.Pad 1) $ str (lvsUpTime lvs)
           , padTop (T.Pad 1) $ str (show . lvsBlockHeight $ lvs)
           ,                    str (show . lvsBlocksMinted $ lvs)
           , padTop (T.Pad 1) $ str (show . lvsTransactions $ lvs)
           , padTop (T.Pad 1) $ str (show . lvsPeersConnected $ lvs)
           , padTop (T.Pad 1) $ str ((show . lvsMaxNetDelay $ lvs) <> " ms")
           ]

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

mempoolDoneAttr, mempoolToDoAttr :: A.AttrName
mempoolDoneAttr = theBaseAttr <> A.attrName "mempool:done"
mempoolToDoAttr = theBaseAttr <> A.attrName "mempool:remaining"

cpuDoneAttr, cpuToDoAttr :: A.AttrName
cpuDoneAttr = theBaseAttr <> A.attrName "cpu:done"
cpuToDoAttr = theBaseAttr <> A.attrName "cpu:remaining"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (theBaseAttr,              bg V.brightBlack  )
         , (mempoolDoneAttr,          V.red `on` V.white)
         , (mempoolToDoAttr,          V.red `on` V.black)
         , (cpuDoneAttr,              V.red `on` V.white)
         , (cpuToDoAttr,              V.red `on` V.black)
         , (P.progressIncompleteAttr, fg V.yellow       )
         ]

theApp :: M.App (LiveViewState a) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = M.resizeOrQuit -- appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }
