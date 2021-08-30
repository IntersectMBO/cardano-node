{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.GUI.Markup.Pane
    ( mkNodesPanes
    ) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import           Control.Monad (forM, forM_, void)
import           Control.Monad.STM (atomically)
import           Data.HashMap.Strict ((!), (!?))
import qualified Data.HashMap.Strict as HM
import           Data.List (sortBy)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, set, string, (#), (#+))
import           System.Time.Extra (sleep)

import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))
import           Cardano.BM.Data.Severity (Severity (..))

import           Cardano.RTView.GUI.Elements (ElementName (..), HTMLClass (..),
                                              HTMLId (..), NodeStateElements, NodesStateElements,
                                              PeerInfoElements (..), PeerInfoItem (..), TmpElements,
                                              dataAttr, hideIt, showIt, (##), (#.))
import           Cardano.RTView.GUI.JS.Utils (downloadCSVFile, goToTab)
import           Cardano.RTView.GUI.Updater (justUpdateErrorsListAndTab)
import           Cardano.RTView.NodeState.CSV (mkCSVWithErrorsForHref)
import           Cardano.RTView.NodeState.Types

  {-
              UI.div #. "panel" #+
                [ UI.p #. "panel-heading" #+
                    [ UI.div #. "columns" #+
                        [ UI.div #. "column" #+
                            [ string "Node: core-1"
                            ]
                        , UI.div #. "column has-text-right" #+
                            [ image "rt-view-node-panel-down" downSVG
                            ]
                        ]
                    ]
                , UI.p #. "panel-tabs is-size-5" #+
                    [ UI.anchor #. "is-active" #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Overview"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Blockchain"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" mempoolSVG,    string "Mempool"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Errors"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "RTS GC"]
                    ]
                , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                    [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
                        [ UI.div #. "column is-half has-text-right" #+
                            [ UI.p #. "mb-1" #+ [ string "Node protocol" ]
                            , UI.p #. "mb-1" #+ [ string "Node version" ]
                            , UI.p #. "mb-1" #+ [ string "Node commit" ]
                            , UI.p #. "mb-1" #+ [ string "Node platform" ]
                            , UI.p #. "mb-1" #+ [ string "Node start time" ]
                            , UI.p           #+ [ string "Node uptime" ]
                            ]
                        , UI.div #. "column is-half has-text-weight-semibold" #+
                            [ UI.p #. "mb-1" #+ [image "rt-view-overview-icon" protocolSVG, string "Shelley"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" versionSVG,  string "1.0"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" commitSVG,   string "abcdefg"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" linuxSVG,    string "Linux"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" calendarSVG, string "2021-01-01 01:01:01 UTC"]
                            , UI.p           #+ [image "rt-view-overview-icon" clockSVG,    string "00:11:05"]
                            ]
                        ]
                    ]
                ]
-}


mkNodesPanes
  :: UI.Window
  -> TVar NodesState
  -> TVar TmpElements
  -> [RemoteAddrNamed]
  -> UI (Element, NodesStateElements, [(Text, Element)])
mkNodesPanes window nsTVar tmpElsTVar acceptors = do
  nodesState <- liftIO $ readTVarIO nsTVar

  nodePanesWithElems
    <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
         -- Explicitly set flags for peers list and errors list: in this case these lists
         -- will be visible even after reload/reopen of the web-page.
         setChangedFlag nameOfNode (\ns -> ns { nodeErrors   = (nodeErrors ns)   { errorsChanged    = True } })
         setChangedFlag nameOfNode (\ns -> ns { peersMetrics = (peersMetrics ns) { peersInfoChanged = True } })

         (pane, nodeStateElems, peerInfoItems) <-
           mkNodePane window nsTVar tmpElsTVar (nodesState ! nameOfNode) nameOfNode acceptors
         return (nameOfNode, pane, nodeStateElems, peerInfoItems)

  panesAreas
    <- forM nodePanesWithElems $ \(_, pane, _, _) ->
         return $ UI.div #. [W3Col, W3L6, W3M12, W3S12, NodePaneArea] #+ [element pane]

  let nodesEls       = [(name, elems, piItems) | (name, _,    elems, piItems) <- nodePanesWithElems]
      panesWithNames = [(name, pane)           | (name, pane, _,     _)       <- nodePanesWithElems]

  panes <- UI.div #. [W3Row] #+ panesAreas

  return (panes, nodesEls, panesWithNames)
 where
  setChangedFlag :: Text -> (NodeState -> NodeState) -> UI ()
  setChangedFlag nameOfNode mkNewNS =
    liftIO . atomically $ modifyTVar' nsTVar $ \currentNS ->
      case currentNS !? nameOfNode of
        Just ns -> HM.adjust (const $ mkNewNS ns) nameOfNode currentNS
        Nothing -> currentNS

mkNodePane
  :: UI.Window
  -> TVar NodesState
  -> TVar TmpElements
  -> NodeState
  -> Text
  -> [RemoteAddrNamed]
  -> UI (Element, NodeStateElements, [PeerInfoItem])
mkNodePane window nsTVar tmpElsTVar NodeState {..} nameOfNode acceptors = do
  let MempoolMetrics {..}    = mempoolMetrics
      ForgeMetrics {..}      = forgeMetrics
      RTSMetrics {..}        = rtsMetrics
      BlockchainMetrics {..} = blockchainMetrics
      KESMetrics {..}        = kesMetrics
      NodeMetrics {..}       = nodeMetrics
      -- ErrorsMetrics {..}     = nodeErrors

  -- Create |Element|s containing node state (info, metrics).
  -- These elements will be part of the complete page,
  -- later they will be updated by acceptor thread.
  elIdleNode <- string "Idle" #. [IdleNode] # hideIt

  let acceptorEndpoint = mkTraceAcceptorEndpoint nameOfNode acceptors

  elNodeProtocol              <- string $ showText nodeProtocol
  elNodeVersion               <- string $ showText nodeVersion
  elNodePlatform              <- string $ showText nodePlatform
  elNodeStarttime             <- string $ showTime nodeStartTime
  elNodeUptime                <- string   showInitTime
  elSystemStartTime           <- string $ showTime systemStartTime
  elEpoch                     <- string $ showInteger epoch
  elSlot                      <- string $ showInteger slot
  elBlocksNumber              <- string $ showInteger blocksNumber
  elBlocksForgedNumber        <- string $ showInteger blocksForgedNumber
  elNodeCannotForge           <- string $ showInteger nodeCannotForge
  elChainDensity              <- string $ showDouble  chainDensity
  elNodeIsLeaderNumber        <- string $ showInteger nodeIsLeaderNum
  elSlotsMissedNumber         <- string $ showInteger slotsMissedNumber
  elTxsProcessed              <- string $ showInteger txsProcessed
  elTraceAcceptorEndpoint     <- string   acceptorEndpoint
                                        # set UI.title__ (fullEndpointTitle acceptorEndpoint)
  elOpCertStartKESPeriod      <- string $ showInteger opCertStartKESPeriod
  elOpCertExpiryKESPeriod     <- string $ showInteger opCertExpiryKESPeriod
  elCurrentKESPeriod          <- string $ showInteger currentKESPeriod
  elRemainingKESPeriods       <- string $ showInteger remKESPeriods
  elRemainingKESPeriodsInDays <- string $ showInteger remKESPeriodsInDays
  elMempoolTxsNumber          <- string $ showInteger mempoolTxsNumber
  elMempoolTxsPercent         <- string $ showDouble  mempoolTxsPercent
  elMempoolBytes              <- string $ showWord64  mempoolBytes
  elMempoolBytesPercent       <- string $ showDouble  mempoolBytesPercent
  elMempoolMaxTxs             <- string $ showInteger mempoolMaxTxs
  elMempoolMaxBytes           <- string $ showInteger mempoolMaxBytes
  elRTSGcMajorNum             <- string $ showInteger rtsGcMajorNum
  elRTSGcMinorNum             <- string $ showInteger rtsGcMinorNum

  elNodeCommitHref
    <- if T.null nodeShortCommit
         then UI.span #+ [string none] -- No real commit was received from the node yet.
         else UI.span #+
                [ UI.anchor # set UI.href ("https://github.com/input-output-hk/cardano-node/commit/"
                                       <> T.unpack nodeCommit)
                            # set UI.target "_blank"
                            # set UI.title__ "Browse cardano-node repository on this commit"
                            # set UI.text (showText nodeShortCommit)
                ]

  -- Progress bars.
  elMempoolBytesProgress    <- UI.div #. [ProgressBar] #+
                                 [ UI.span #. [HSpacer] #+ []
                                 , element elMempoolBytes
                                 , string "/" #. [PercentsSlashHSpacer]
                                 , element elMempoolBytesPercent
                                 , string "%"
                                 ]
  elMempoolBytesProgressBox <- UI.div #. [ProgressBarBox] #+ [element elMempoolBytesProgress]

  elMempoolTxsProgress      <- UI.div #. [ProgressBar] #+
                                 [ UI.span #. [HSpacer] #+ []
                                 , element elMempoolTxsNumber
                                 , string "/" #. [PercentsSlashHSpacer]
                                 , element elMempoolTxsPercent
                                 , string "%"
                                 ]
  elMempoolTxsProgressBox   <- UI.div #. [ProgressBarBox] #+ [element elMempoolTxsProgress]

  -- Create content area for each tab.
  nodeTabContent
    <- UI.div #. [TabContainer, W3Row] # showIt #+
         [ UI.div #. [W3Half] #+
             [ UI.div #+ [string "Node protocol"   # set UI.title__ "Node's protocol"]
             , UI.div #+ [string "Node version"    # set UI.title__ "Version of the node"]
             -- TODO: platform will be traced in nodeBasicInfo.
             -- , UI.div #+ [string "Node platform"   # set UI.title__ "Platform the node is working on"]
             , UI.div #+ [string "Node commit"     # set UI.title__ "Git commit the node was built from"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Node start time" # set UI.title__ "The time when this node has started"]
             , UI.div #+ [string "Node uptime"     # set UI.title__ "How long the node is working"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Node endpoint"   # set UI.title__ "Socket/pipe used to connect the node with RTView,\ntaken from corresponding remoteAddr field in RTView configuration"]
             , vSpacer NodeInfoVSpacer
             ]
         , UI.div #. [W3Half, NodeInfoValues] #+
             [ UI.div #+ [element elNodeProtocol]
             , UI.div #+ [element elNodeVersion]
             -- TODO: platform will be traced in nodeBasicInfo.
             -- , UI.div #+ [element elNodePlatform]
             , UI.div #. [CommitLink] #+ [element elNodeCommitHref]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [element elNodeStarttime]
             , UI.div #+ [element elNodeUptime]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [element elTraceAcceptorEndpoint]
             , vSpacer NodeInfoVSpacer
             ]
         ]

  kesTabContent
    <- UI.div #. [TabContainer, W3Row] # hideIt #+
         [ UI.div #. [W3Half] #+
             [ UI.div #+ [string "Start KES period"    # set UI.title__ "Start KES period of configured\noperational certificate"]
             , UI.div #+ [string "Expiry KES period"   # set UI.title__ "KES expiry period, calculated as a sum of\nstart KES period and configured maximum KES evolutions"]
             , UI.div #+ [string "Current KES period"  # set UI.title__ "Current KES period, calculated as a sum of\nstart KES period and current KES period of the hot key"]
             , UI.div #+ [string "KES remaining"       # set UI.title__ "KES periods until expiry, calculated as a diff\nbetween expiry KES period and current KES period"]
             , UI.div #+ [string "KES remaining, days" # set UI.title__ "KES periods until expiry, in days"]
             , vSpacer NodeInfoVSpacer
             ]
         , UI.div #. [W3Half, NodeInfoValues] #+
             [ UI.div #+ [element elOpCertStartKESPeriod]
             , UI.div #+ [element elOpCertExpiryKESPeriod]
             , UI.div #+ [element elCurrentKESPeriod]
             , UI.div #+ [element elRemainingKESPeriods]
             , UI.div #+ [element elRemainingKESPeriodsInDays]
             , vSpacer NodeInfoVSpacer
             ]
         ]

  -- List of items corresponding to each peer. To avoid dynamic changes of DOM
  -- (unfortunately, it can be a reason of space leak), we create 20 (hidden) rows
  -- corresponding to 20 connected peers. Theoretically, the number of connected
  -- peers can be bigger, but the policy of ouroboros-network is about 20 hot peers
  -- (or less).
  let supportedPeersNum = 20 :: Int -- TODO: Probably cardano-node cab trace this number?
  peersList :: [(UI Element, PeerInfoItem)]
    <- forM [1..supportedPeersNum] $ const $ do
         endpoint   <- string ""
         slotNumber <- string ""
         bytesInF   <- string ""
         reqsInF    <- string ""
         blocksInF  <- string ""
         status     <- string ""

         peerItem
           <- UI.div #. [W3Row] # set UI.style [("display", "none")] #+
                [ UI.div #. [W3Quarter, NodeMetricsValues] #+
                    [element endpoint]
                , UI.div #. [W3Quarter, W3RightAlign, NodeMetricsValues] #+
                    [element slotNumber]
                , UI.div #. [W3Quarter, W3RightAlign, NodeMetricsValues] #+
                    [ element bytesInF # set UI.title__ "Sum of the byte count of blocks expected\nfrom all in-flight fetch requests"
                    , string " / "
                    , element reqsInF # set UI.title__ "Number of blocks fetch requests\nthat are currently in-flight"
                    , string " / "
                    , element blocksInF # set UI.title__ "Blocks that are currently in-flight"
                    ]
                , UI.div #. [W3Quarter, W3RightAlign, NodeMetricsValues] #+
                    [element status]
                ]
         return ( element peerItem
                , PeerInfoItem
                    peerItem
                    (PeerInfoElements endpoint bytesInF reqsInF blocksInF slotNumber status)
                )
  let (elPeersList, peerInfoItems) = unzip peersList

  peersTabContent
    <- UI.div #. [TabContainer, W3Row] # hideIt #+
         [ UI.div #. [W3Quarter] #+
             [ string "Endpoint" # set UI.title__ "How the peer connected to this node"
             ]
         , UI.div #. [W3Quarter, W3RightAlign] #+
             [ string "Slot No." # set UI.title__ "Total number of peers reported by peer"
             ]
         , UI.div #. [W3Quarter, W3RightAlign] #+
             [ string "In Flight: b/r/bl" # set UI.title__ "In Flight: bytes/requests/blocks"
             ]
         , UI.div #. [W3Quarter, W3RightAlign] #+
             [ string "Status" # set UI.title__ "Peer's status"
             ]
         , UI.div #+ elPeersList
         ]

  blockchainTabContent
    <- UI.div #. [TabContainer, W3Row] # hideIt #+
         [ UI.div #. [W3Half] #+
             [ UI.div #+ [string "Blockchain start time"
                          # set UI.title__ "The time when this blockchain has started"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Epoch / Slot in epoch"
                          # set UI.title__ "Number of current epoch / number of the current slot in this epoch"]
             , UI.div #+ [string "Blocks number"
                          # set UI.title__ "Total number of blocks in this blockchain"]
             , UI.div #+ [string "Forged blocks number"
                          # set UI.title__ "Number of blocks forged by this node"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Chain density"
                          # set UI.title__ "Chain density, in percents"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Slot leader, number"
                          # set UI.title__ "Number of slots when this node was a leader"]
             , UI.div #+ [string "Cannot forge, number"
                          # set UI.title__ "Number of slots when this node was a leader\nbut because of misconfiguration, it was impossible to forge a new block"]
             , UI.div #+ [string "Missed slots number"
                          # set UI.title__ "Number of slots when this node was a leader\nbut didn't forge a new block by some reason"]
             , vSpacer NodeInfoVSpacer
             ]
         , UI.div #. [W3Half, NodeInfoValues] #+
             [ UI.div #+
                 [element elSystemStartTime]
             , vSpacer NodeInfoVSpacer
             , UI.div #+
                 [ element elEpoch
                 , string " / "
                 , element elSlot
                 ]
             , UI.div #+
                 [element elBlocksNumber]
             , UI.div #+
                 [element elBlocksForgedNumber]
             , vSpacer NodeInfoVSpacer
             , UI.div #+
                 [ element elChainDensity
                 , string "%" #. [DensityPercent]
                 ]
             , vSpacer NodeInfoVSpacer
             , UI.div #+
                 [element elNodeIsLeaderNumber]
             , UI.div #+
                 [element elNodeCannotForge]
             , UI.div #+
                 [element elSlotsMissedNumber]
             , vSpacer NodeInfoVSpacer
             ]
         ]

  mempoolTabContent
    <- UI.div #. [TabContainer] # hideIt #+
         [ UI.div #. [W3Container] #+
             [ UI.div #. [W3Row] #+
                 [ UI.div #. [W3Half] #+
                     [string "Mempool | bytes"
                      # set UI.title__ "Size of all transactions in the mempool, in bytes"]
                 , UI.div #. [W3Half, W3RightAlign] #+
                     [ element elMempoolMaxBytes
                     , infoMark "Maximum in bytes"
                     ]
                 ]
                 , element elMempoolBytesProgressBox
             ]
         , vSpacer NodeMetricsVSpacer
         , UI.div #. [W3Container] #+
             [ UI.div #. [W3Row] #+
                 [ UI.div #. [W3Half] #+
                     [string "Mempool | TXs"
                      # set UI.title__ "Number of transactions in the mempool"]
                 , UI.div #. [W3Half, W3RightAlign] #+
                     [ element elMempoolMaxTxs
                     , infoMark "Maximum in txs"
                     ]
                 ]
                 , element elMempoolTxsProgressBox
             ]
         , vSpacer NodeMetricsVSpacer
         , UI.div #. [W3Row] #+
              [ string "TXs processed"
                  # set UI.title__ "Number of processed transactions in this blockchain\n(these transactions are already removed from the mempool)"
              , element elTxsProcessed #. [TXsProcessed]
              ]
         , vSpacer NodeMetricsVSpacer
         ]

  resourcesTabMemoryContent
    <- UI.div #. [W3Container, TabContainer] # hideIt #+
         [ UI.canvas ## (show MemoryUsageChartId <> T.unpack nameOfNode)
                     #. [ChartArea]
                     #+ []
         ]

  resourcesTabCPUContent
    <- UI.div #. [W3Container, TabContainer] # hideIt #+
         [ UI.canvas ## (show CPUUsageChartId <> T.unpack nameOfNode)
                     #. [ChartArea]
                     #+ []
         ]

  resourcesTabDiskContent
    <- UI.div #. [W3Container, TabContainer] # hideIt #+
         [ UI.canvas ## (show DiskUsageChartId <> T.unpack nameOfNode)
                     #. [ChartArea]
                     #+ []
         ]

  resourcesTabNetworkContent
    <- UI.div #. [W3Container, TabContainer] # hideIt #+
         [ UI.canvas ## (show NetworkUsageChartId <> T.unpack nameOfNode)
                     #. [ChartArea]
                     #+ []
         ]

  ghcRTSTabContent
    <- UI.div #. [TabContainer] # hideIt #+
         [ UI.div #. [W3Row] #+
             [ UI.div #. [W3Half] #+
                 [ UI.div #+ [string "Number of major GC runs" # set UI.title__ "Total number of major (oldest generation) garbage collections"]
                 , UI.div #+ [string "Number of minor GC runs" # set UI.title__ "Total number of minor garbage collections"]
                 ]
             , UI.div #. [W3Half, NodeInfoValues] #+
                 [ UI.div #+ [element elRTSGcMajorNum]
                 , UI.div #+ [element elRTSGcMinorNum]
                 ]
             ]
         , vSpacer NodeMetricsVSpacer
         ]

  -- List of node errors, it will be changed dynamically!
  elNodeErrorsList <- UI.div #. [ErrorsTabList] #+ []

  let dataSortByTime = "data-sortByTime"
      dataSortBySev  = "data-sortBySev"
      desc = "desc"
      asc  = "asc"

  elSortByTime  <- UI.img #. [ErrorsSortIcon]
                          # set UI.src "/static/images/sort.svg"
                          # set (dataAttr dataSortByTime) desc
                          # set UI.title__ "Sort by time, latest message first"
  elSortBySev   <- UI.img #. [ErrorsSortIcon]
                          # set UI.src "/static/images/sort.svg"
                          # set (dataAttr dataSortBySev) desc
                          # set UI.title__ "Sort by severity level, worst message first"
  elFilterBySev <- UI.img #. [ErrorsFilterIcon]
                          # set UI.src "/static/images/filter.svg"
                          # set UI.title__ "Filter by severity level"

  elRemoveAllErrors <- UI.img #. [ErrorsRemoveIcon]
                              # set UI.src "/static/images/trash.svg"
                              # set UI.title__ "Remove all messages"

  let nameOfNodeS = T.unpack nameOfNode

  filterWarning   <- UI.anchor #. [W3BarItem, W3Button, W3Mobile]
                               ## (show WarningMessageId <> nameOfNodeS)
                               # set UI.href "#" #+
                       [ UI.string "W" #. [WarningMessageTagNoHelp]
                       , UI.string "Warning"
                       ]
  filterError     <- UI.anchor #. [W3BarItem, W3Button, W3Mobile]
                               ## (show ErrorMessageId <> nameOfNodeS)
                               # set UI.href "#" #+
                       [ UI.string "E" #. [ErrorMessageTagNoHelp]
                       , UI.string "Error"
                       ]
  filterCritical  <- UI.anchor #. [W3BarItem, W3Button, W3Mobile]
                               ## (show CriticalMessageId <> nameOfNodeS)
                               # set UI.href "#" #+
                       [ UI.string "C" #. [CriticalMessageTagNoHelp]
                       , UI.string "Critical"
                       ]
  filterAlert     <- UI.anchor #. [W3BarItem, W3Button, W3Mobile]
                               ## (show AlertMessageId <> nameOfNodeS)
                               # set UI.href "#" #+
                       [ UI.string "A" #. [AlertMessageTagNoHelp]
                       , UI.string "Alert"
                       ]
  filterEmergency <- UI.anchor #. [W3BarItem, W3Button, W3Mobile]
                               ## (show EmergencyMessageId <> nameOfNodeS)
                               # set UI.href "#" #+
                       [ UI.string "E" #. [EmergencyMessageTagNoHelp]
                       , UI.string "Emergency"
                       ]
  unFilter        <- UI.anchor #. [W3BarItem, W3Button, W3Mobile, W3BorderTop, W3Disabled]
                               # set UI.href "#"
                               #+ [ UI.string "Reset" ]

  elDownloadErrorsAsCSV
    <- UI.img #. [ErrorsDownloadIcon]
              # set UI.src "/static/images/file-download.svg"
              # set UI.title__ "Download all errors as CSV file"

  void $ UI.onEvent (UI.click elDownloadErrorsAsCSV) $ \_ -> do
    nss <- liftIO $ readTVarIO nsTVar
    let NodeState _ _ _ _ _ _ _ _ em _ = nss ! nameOfNode
        csvFileName = "cardano-rt-view-" <> nameOfNodeS <> "-errors.csv"
        errorsAsCSV = mkCSVWithErrorsForHref (errors em)
    UI.runFunction $ UI.ffi downloadCSVFile csvFileName errorsAsCSV

  elSearchErrorInput
    <- UI.input ## (show SearchErrorInputId <> nameOfNodeS)
                #. [W3Input, SearchErrorInput]
                # set (UI.attr "placeholder") "Search message..."

  errorsTabContent
    <- UI.div #. [TabContainer] # hideIt #+
         [ UI.div #. [W3Row, ErrorsTabHeader] #+
             [ UI.div #. [W3Third] #+
                 [ string "Timestamp"
                 , element elSortByTime
                 ]
             , UI.div #. [W3TwoThird] #+
                 [ UI.div #. [W3Row] #+
                     [ UI.div #. [W3TwoThird] #+
                         [ string "Message"
                         , element elSortBySev
                         , UI.div #. [W3DropdownHover, W3Mobile] #+
                             [ element elFilterBySev
                             , UI.img #. [ErrorsFilterDropdownIcon] # set UI.src "/static/images/dropdown-dark.svg"
                             , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+
                                 [ element filterWarning
                                 , element filterError
                                 , element filterCritical
                                 , element filterAlert
                                 , element filterEmergency
                                 , element unFilter
                                 ]
                             ]
                         , element elDownloadErrorsAsCSV
                         ]
                     , UI.div #. [W3Third, W3RightAlign] #+
                         [ element elRemoveAllErrors
                         ]
                     ]
                 ]
             ]
         , element elNodeErrorsList
         , UI.div #. [W3Row] #+
             [ UI.div #. [W3Third] #+
                 [ UI.span # set UI.html "&nbsp;"
                 ]
             , UI.div #. [W3TwoThird, SearchErrorArea] #+
                 [ UI.img #. [SearchErrorIcon]
                          # set UI.src "/static/images/search.svg"
                 , element elSearchErrorInput
                 ]
             ]
         ]

  -- Tabs for corresponding sections.
  let tabButton :: String -> String -> Maybe Element -> UI Element
      tabButton title iconName maybeBadge = do
        let buttonContent =
              case maybeBadge of
                Just badge ->
                  [ UI.img #. [NodeMenuIcon] # set UI.src ("/static/images/" <> iconName)
                  , element badge
                  ]
                Nothing ->
                  [ UI.img #. [NodeMenuIcon] # set UI.src ("/static/images/" <> iconName)
                  ]
        UI.button #. [W3BarItem, W3Button, W3Mobile]
                  # set UI.title__ title
                  #+ buttonContent

      anchorButton title iconName =
        UI.anchor #. [W3BarItem, W3Button, W3Mobile]
                  #+ [ UI.img #. [ResourcesIcon]
                              # set UI.src ("/static/images/" <> iconName)
                     , string title
                     ]

  nodeTab       <- tabButton "Node info" "info.svg" Nothing
                             ## (show NodeInfoTab <> nameOfNodeS)
                             # makeItActive
  kesTab        <- tabButton "Key Evolving Signature" "key.svg" Nothing
                             ## (show KESTab <> nameOfNodeS)
  peersTab      <- tabButton "Peers" "peers.svg" Nothing
                             ## (show PeersTab <> nameOfNodeS)
  blockchainTab <- tabButton "Blockchain" "blockchain.svg" Nothing
                             ## (show BlockchainTab <> nameOfNodeS)
  mempoolTab    <- tabButton "Mempool" "mempool.svg" Nothing
                             ## (show MempoolTab <> nameOfNodeS)
  ghcRTSTab     <- tabButton "RTS GC" "rts.svg" Nothing
                             ## (show RTSGCTab <> nameOfNodeS)
  errorsBadge   <- UI.span #. [W3Badge, ErrorsBadge] # hideIt #+ [string ""]
  errorsTab     <- tabButton "Errors" "bugs.svg" (Just errorsBadge)
                             ## (show ErrorsTab <> nameOfNodeS)
                             # set UI.enabled False

  -- If we already have some errors in the node's state - update the list in "Errors" tab right now.
  let errors' = errors nodeErrors
      shouldWeRebuild = True
  justUpdateErrorsListAndTab window tmpElsTVar nameOfNode errors' shouldWeRebuild elNodeErrorsList errorsTab errorsBadge

  void $ UI.onEvent (UI.click elSortByTime) $ \_ -> do
    UI.get (dataAttr dataSortByTime) elSortByTime >>= \case
      "desc" -> do
        setErrorsViewMode nsTVar nameOfNode $ sortErrors sortErrorsByTimeDesc
        void $ element elSortByTime # set (dataAttr dataSortByTime) asc
                                    # set UI.title__ "Sort by time, earlier message first"
      _ -> do
        setErrorsViewMode nsTVar nameOfNode $ sortErrors sortErrorsByTimeAsc
        void $ element elSortByTime # set (dataAttr dataSortByTime) desc
                                    # set UI.title__ "Sort by time, latest message first"
    immediatelyUpdateErrors window nsTVar tmpElsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge

  void $ UI.onEvent (UI.click elSortBySev) $ \_ -> do
    UI.get (dataAttr dataSortBySev) elSortBySev >>= \case
      "desc" -> do
        setErrorsViewMode nsTVar nameOfNode $ sortErrors sortErrorsBySevDesc
        void $ element elSortBySev # set (dataAttr dataSortBySev) asc
                                   # set UI.title__ "Sort by severity level, warning messages first"
      _ -> do
        setErrorsViewMode nsTVar nameOfNode $ sortErrors sortErrorsBySevAsc
        void $ element elSortBySev # set (dataAttr dataSortBySev) desc
                                   # set UI.title__ "Sort by severity level, worst messages first"
    immediatelyUpdateErrors window nsTVar tmpElsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge

  let makeResetButtonActive   = void $ element unFilter #. [W3BarItem, W3Button, W3Mobile, W3BorderTop]
      makeResetButtonInactive = void $ element unFilter #. [W3BarItem, W3Button, W3Mobile, W3BorderTop, W3Disabled]

  let registerClickOnFilter :: Element -> Severity -> UI ()
      registerClickOnFilter el sev =
        void $ UI.onEvent (UI.click el) $ \_ -> do
          setErrorsViewMode nsTVar nameOfNode $ filterErrors sev
          makeResetButtonActive
          immediatelyUpdateErrors window nsTVar tmpElsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge

  registerClickOnFilter filterWarning   Warning
  registerClickOnFilter filterError     Error
  registerClickOnFilter filterCritical  Critical
  registerClickOnFilter filterAlert     Alert
  registerClickOnFilter filterEmergency Emergency

  let filterItemsThatCanBeAcive =
        zip [1 :: Int ..]
            [ filterWarning
            , filterError
            , filterCritical
            , filterAlert
            , filterEmergency
            ]

  void $ UI.onEvent (UI.click unFilter) $ \_ -> do
    setErrorsViewMode nsTVar nameOfNode unFilterErrors
    immediatelyUpdateErrors window nsTVar tmpElsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge
    makeResetButtonInactive
    forM_ filterItemsThatCanBeAcive $ \(_, el) -> void $ element el # makeItInactive

  void $ UI.onEvent (UI.click elRemoveAllErrors) $ \_ -> do
    setErrorsViewMode nsTVar nameOfNode removeAllErrors
    immediatelyUpdateErrors window nsTVar tmpElsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge
    liftIO $ sleep 0.4
    UI.runFunction $ UI.ffi goToTab (show NodeInfoTab <> nameOfNodeS)

  forM_ filterItemsThatCanBeAcive $ \(ix, el) ->
    void $ UI.onEvent (UI.click el) $ \_ ->
      forM_ filterItemsThatCanBeAcive $ \(ix', el') ->
        if ix == ix'
          then void $ element el' # makeItActive
          else void $ element el' # makeItInactive

  void $ UI.onEvent (UI.valueChange elSearchErrorInput) $ \currentText -> do
    let filterAction =
          if null currentText
            then unFilterErrors
            else filterErrorsByText currentText
    setErrorsViewMode nsTVar nameOfNode filterAction
    immediatelyUpdateErrors window nsTVar tmpElsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge

  resourcesTabMemory  <- anchorButton "Memory" "memory.svg"
                           ## (show ResTabMemory <> nameOfNodeS)
  resourcesTabCPU     <- anchorButton "CPU" "cpu.svg"
                           ## (show ResTabCPU <> nameOfNodeS)
  resourcesTabDisk    <- anchorButton "Disk" "disk.svg"
                           ## (show ResTabDisk <> nameOfNodeS)
  resourcesTabNetwork <- anchorButton "Network" "network.svg"
                           ## (show ResTabNetwork <> nameOfNodeS)

  resourcesTab
    <- UI.div #. [W3DropdownHover, W3Mobile] #+
         [ UI.button #. [W3Button]
                     # set UI.title__ "Resources"
                     #+
             [ UI.img #. [NodeMenuIcon] # set UI.src "/static/images/resources.svg"
             , UI.img #. [ResourcesDropdownIcon] # set UI.src "/static/images/dropdown-blue.svg"
             ]
         , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+
             [ element resourcesTabMemory
             , element resourcesTabCPU
             , element resourcesTabDisk
                 #. [W3BarItem, W3Button, W3Mobile, W3Disabled] # set UI.title__ "Disabled in this release"
             , element resourcesTabNetwork
                 #. [W3BarItem, W3Button, W3Mobile, W3Disabled] # set UI.title__ "Disabled in this release"
             ]
         ]

  let tabs :: [((Element, Element), Int)]
      tabs =
        let allTabs = [ (nodeTab,             nodeTabContent)
                      , (kesTab,              kesTabContent)
                      , (peersTab,            peersTabContent)
                      , (blockchainTab,       blockchainTabContent)
                      , (mempoolTab,          mempoolTabContent)
                      , (resourcesTabMemory,  resourcesTabMemoryContent)
                      , (resourcesTabCPU,     resourcesTabCPUContent)
                      -- Temporarily disabled (new metrics in iohk-monitoring should be implemented).
                      -- , (resourcesTabDisk,    resourcesTabDiskContent)
                      -- , (resourcesTabNetwork, resourcesTabNetworkContent)
                      , (errorsTab,           errorsTabContent)
                      , (ghcRTSTab,           ghcRTSTabContent)
                      ]
        in zip allTabs [1..length allTabs]

  registerClicksOnTabs tabs

  -- Make a widget for one node.
  nodePane <-
    UI.div #. [W3Container, W3Margin, W3Border, W3Card2, NodeContainer] #+
      [ UI.div #. [NodeNameArea] #+
          [ string "Name: "
          , string (T.unpack nameOfNode) #. [NodeName]
                                         # set UI.title__ "Name of this node taken from corresponding\nnodeName field in RTView configuration"
          , element elIdleNode
          ]
      , UI.div #. [W3Bar, NodeBar] #+
          [ element nodeTab
          , element kesTab
          , element peersTab
          , element blockchainTab
          , element mempoolTab
          , element resourcesTab
          , element errorsTab
          , element ghcRTSTab
          ]
      , element nodeTabContent
      , element kesTabContent
      , element peersTabContent
      , element blockchainTabContent
      , element mempoolTabContent
      , element resourcesTabMemoryContent
      , element resourcesTabCPUContent
      , element resourcesTabDiskContent
      , element resourcesTabNetworkContent
      , element errorsTabContent
      , element ghcRTSTabContent
      ]

  -- Return these elements, they will be updated by another thread later.
  let nodeStateElems = HM.fromList
        [ (ElNodePane,                nodePane)
        , (ElIdleNode,                elIdleNode)
        , (ElNodeProtocol,            elNodeProtocol)
        , (ElNodeVersion,             elNodeVersion)
        , (ElNodePlatform,            elNodePlatform)
        , (ElNodeCommitHref,          elNodeCommitHref)
        , (ElNodeStarttime,           elNodeStarttime)
        , (ElNodeUptime,              elNodeUptime)
        , (ElSystemStartTime,         elSystemStartTime)
        , (ElEpoch,                   elEpoch)
        , (ElSlot,                    elSlot)
        , (ElBlocksNumber,            elBlocksNumber)
        , (ElBlocksForgedNumber,      elBlocksForgedNumber)
        , (ElNodeCannotForge,         elNodeCannotForge)
        , (ElChainDensity,            elChainDensity)
        , (ElNodeIsLeaderNumber,      elNodeIsLeaderNumber)
        , (ElSlotsMissedNumber,       elSlotsMissedNumber)
        , (ElTxsProcessed,            elTxsProcessed)
        , (ElTraceAcceptorEndpoint,   elTraceAcceptorEndpoint)
        , (ElOpCertStartKESPeriod,    elOpCertStartKESPeriod)
        , (ElOpCertExpiryKESPeriod,   elOpCertExpiryKESPeriod)
        , (ElCurrentKESPeriod,        elCurrentKESPeriod)
        , (ElRemainingKESPeriods,     elRemainingKESPeriods)
        , (ElRemainingKESPeriodsInDays, elRemainingKESPeriodsInDays)
        , (ElNodeErrors,              elNodeErrorsList)
        , (ElNodeErrorsTab,           errorsTab)
        , (ElNodeErrorsTabBadge,      errorsBadge)
        , (ElMempoolTxsNumber,        elMempoolTxsNumber)
        , (ElMempoolTxsPercent,       elMempoolTxsPercent)
        , (ElMempoolBytes,            elMempoolBytes)
        , (ElMempoolBytesPercent,     elMempoolBytesPercent)
        , (ElMempoolMaxTxs,           elMempoolMaxTxs)
        , (ElMempoolMaxBytes,         elMempoolMaxBytes)
        , (ElRTSGcMajorNum,           elRTSGcMajorNum)
        , (ElRTSGcMinorNum,           elRTSGcMinorNum)
        -- Progress bars
        , (ElMempoolBytesProgress,    elMempoolBytesProgress)
        , (ElMempoolBytesProgressBox, elMempoolBytesProgressBox)
        , (ElMempoolTxsProgress,      elMempoolTxsProgress)
        , (ElMempoolTxsProgressBox,   elMempoolTxsProgressBox)
        ]

  return (nodePane, nodeStateElems, peerInfoItems)

---

setErrorsViewMode
  :: TVar NodesState
  -> Text
  -> (NodeState -> NodeState)
  -> UI ()
setErrorsViewMode nsTVar nameOfNode mkNewNS =
  liftIO . atomically $ modifyTVar' nsTVar $ \currentNS ->
    case currentNS !? nameOfNode of
      Just ns -> HM.adjust (const $ mkNewNS ns) nameOfNode currentNS
      Nothing -> currentNS

sortErrors
  :: (NodeError -> NodeError -> Ordering)
  -> NodeState
  -> NodeState
sortErrors orderF ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = sortBy orderF currentErrors
    , errorsChanged = True
    , errorsRebuild = True
    }
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics

filterErrors
  :: Severity
  -> NodeState
  -> NodeState
filterErrors targetSev ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = map showOnlyThisSeverity currentErrors
    , errorsChanged = True
    , errorsRebuild = True
    }
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics
  showOnlyThisSeverity (NodeError ts sev msg _) = NodeError ts sev msg visible
   where
    visible = targetSev == sev

filterErrorsByText
  :: String
  -> NodeState
  -> NodeState
filterErrorsByText s ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = map showOnlyWithThisText currentErrors
    , errorsChanged = True
    , errorsRebuild = True
    }
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics
  textWeSearch = T.strip . T.pack $ s
  showOnlyWithThisText (NodeError ts sev msg _) = NodeError ts sev msg visible
   where
    visible = (T.toLower textWeSearch) `T.isInfixOf` (T.toLower msg)

unFilterErrors
  :: NodeState
  -> NodeState
unFilterErrors ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = map makeVisible currentErrors
    , errorsChanged = True
    , errorsRebuild = True
    }
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics
  makeVisible (NodeError ts sev msg _) = NodeError ts sev msg True

removeAllErrors
  :: NodeState
  -> NodeState
removeAllErrors ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = []
    , errorsChanged = True
    , errorsRebuild = True
    }
  currentMetrics = nodeErrors ns

immediatelyUpdateErrors
  :: UI.Window
  -> TVar NodesState
  -> TVar TmpElements
  -> Text
  -> Element
  -> Element
  -> Element
  -> UI ()
immediatelyUpdateErrors window nsTVar tmpElsTVar nameOfNode el elTab elTabBadge = do
  updatedState <- liftIO $ readTVarIO nsTVar
  let NodeState {..} = updatedState ! nameOfNode
      errors' = errors nodeErrors
      shouldWeRebuild = errorsRebuild nodeErrors
  justUpdateErrorsListAndTab window tmpElsTVar nameOfNode errors' shouldWeRebuild el elTab elTabBadge

vSpacer :: HTMLClass -> UI Element
vSpacer className = UI.div #. [className] #+ []

-- | Since information and metrics are splitted to tabs,
--   we have to make them clickable and show which one is active.
registerClicksOnTabs
  :: [((Element, Element), Int)]
  -> UI ()
registerClicksOnTabs tabs =
  forM_ tabs $ \((tab, _), tabNum) ->
    void $ UI.onEvent (UI.click tab) $ \_ -> showTabAndMakeItActive tabNum
 where
  showTabAndMakeItActive num =
    forM_ tabs $ \((tab', tabContent), tabNum') ->
      if num == tabNum'
        then do
          void $ element tabContent # showIt
          void $ element tab' # makeItActive
        else do
          void $ element tabContent # hideIt
          void $ element tab' # makeItInactive

makeItActive, makeItInactive :: UI Element -> UI Element
makeItActive el   = el #. [W3BarItem, W3Button, W3Mobile, ActiveTab]
makeItInactive el = el #. [W3BarItem, W3Button, W3Mobile]

infoMark :: String -> UI Element
infoMark aTitle =
  UI.span #. [InfoMark]
          #  set UI.title__ aTitle
          #+ [ UI.img #. [InfoMarkImg]
                      # set UI.src "/static/images/question.svg" #+ []
             ]
