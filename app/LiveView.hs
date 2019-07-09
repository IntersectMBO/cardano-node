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
import           Brick.Widgets.Core (overrideAttr, str, updateAttrMap, (<+>),
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
drawUI p = [ui]
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
      ui = str "Cardano Shelley"
       <=> str ""
       <=> (str "mempool:   " <+> memPoolBar)
       <=> (str "cpu usage: " <+> cpuUsageBar)


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
