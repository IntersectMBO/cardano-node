{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Handlers.RTView.UI.Updater
  ( updateUI
  ) where

import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (readIORef)
import           Graphics.UI.Threepenny.Core (Element, UI, liftIO)

import           Cardano.Tracer.Handlers.RTView.UI.Elements
import           Cardano.Tracer.Types (AcceptedItems)

updateUI
  :: AcceptedItems
  -> TVar PageElements
  -> Element
  -> Element
  -> UI ()
updateUI acceptedItemsIORef pageElementsTVar noNodesNotify rootElemForNodePanels = do
  pageElements <- liftIO . readTVarIO $ pageElementsTVar
  acceptedItems <- liftIO . readIORef $ acceptedItemsIORef
  forM_ (HM.toList acceptedItems) $ \(nodeId, (niStore, traceObjects, metrics)) ->
    case HM.lookup nodeId pageElements of
      Just (nodePanel, nodePanelEls) ->
        -- Such node panel is already here, check if we need to update some of its elements.
        -- check .
        return ()
      Nothing ->
        -- No such node panel, it means that the new node with 'nodeId'
        -- is connected to 'cardano-tracer' since the last check.
        return ()

-- AcceptedItems: HashMap NodeId (NodeInfoStore, TraceObjects, Metrics)
-- PageElements:  HashMap NodeId (Element, NodePanelElements)

  {-

import           Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import           Control.Monad (void, forM, forM_, unless, when)
import           Control.Monad.STM (atomically)
import           Control.Monad.Extra (whenJust, whenJustM)
import qualified Data.List as L
import           Data.Maybe (fromJust, isJust)
import           Data.HashMap.Strict ((!), (!?))
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack, strip, unpack)
import qualified Data.Text as T
import           Data.Time.Calendar (diffDays)
import           Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, getCurrentTime,
                                  diffUTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, children, element, liftIO, set, style,
                                              text, (#), (#+))

import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Trace (Trace, logDebug)

import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.GUI.Elements (ElementName (..), ElementValue (..),
                                              HTMLClass (..), HTMLId (..),
                                              NodeStateElements, NodesStateElements,
                                              PeerInfoElements (..), PeerInfoItem (..),
                                              TmpElements,
                                              (#.), hideIt, showInline, showIt, pageTitle,
                                              pageTitleNotify)
import qualified Cardano.RTView.GUI.JS.Charts as Chart
import           Cardano.RTView.NodeState.Types
import           Cardano.RTView.SupportedNodes (supportedNodesVersions, showSupportedNodesVersions)

-- | This function is calling by the timer. It updates the node' state elements
--   on the page automatically, because threepenny-gui is based on websockets.
updateGUI
  :: Trace IO Text
  -> UI.Window
  -> TVar NodesState
  -> TVar TmpElements
  -> RTViewParams
  -> NodesStateElements
  -> UI ()
updateGUI tr window tv tmpElsTVar params nodesStateElems = do
  liftIO $ logDebug tr "Update GUI elements..."
  nodesState <- liftIO $ readTVarIO tv
  resetPageTitleIfNeeded window tv

  forM_ nodesStateElems $ \(nName, els, peerInfoItems) -> do
    let ns@NodeState {..}      = nodesState ! nName
        PeerMetrics {..}       = peersMetrics
        MempoolMetrics {..}    = mempoolMetrics
        ForgeMetrics {..}      = forgeMetrics
        RTSMetrics {..}        = rtsMetrics
        BlockchainMetrics {..} = blockchainMetrics
        KESMetrics {..}        = kesMetrics
        NodeMetrics {..}       = nodeMetrics
        ErrorsMetrics {..}     = nodeErrors

    liftIO $ logDebug tr $ "Current state for node " <> nName <> ": " <> T.pack (show ns)

    updateErrorsListAndTab window tv tmpElsTVar nName errors errorsChanged els
                           ElNodeErrors ElNodeErrorsTab ElNodeErrorsTabBadge
    updatePeersList tv nName peersInfo peersInfoChanged peerInfoItems

    -- TODO: temporary solution, progress bars will be replaced by charts soon.
    updateProgressBar mempoolBytesPercent  els ElMempoolBytesProgress
    updateProgressBar mempoolTxsPercent    els ElMempoolTxsProgress

    nodeIsIdle <- checkIfNodeIsIdlePane params metricsLastUpdate (els ! ElIdleNode) (els ! ElNodePane)
    unless nodeIsIdle $ do
      setNodeStartTime  tv nName nodeStartTime nodeStartTimeChanged els ElNodeStarttime
      setNodeUpTime nodeStartTime els ElNodeUptime

      setNodeVersion    tv nName (TextV    nodeVersion)          nodeVersionChanged        els ElNodeVersion
      setNodeProtocol   tv nName (TextV    nodeProtocol)         nodeProtocolChanged       els ElNodeProtocol
      setNodePlatform   tv nName (TextV    nodePlatform)         nodePlatformChanged       els ElNodePlatform
      setEpoch          tv nName (IntegerV epoch)                epochChanged              els ElEpoch
      setSlot           tv nName (IntegerV slot)                 slotChanged               els ElSlot
      setBlocksNumber   tv nName (IntegerV blocksNumber)         blocksNumberChanged       els ElBlocksNumber
      setChainDensity   tv nName (DoubleV  chainDensity)         chainDensityChanged       els ElChainDensity
      setForgedNum      tv nName (IntegerV blocksForgedNumber)   blocksForgedNumberChanged els ElBlocksForgedNumber
      setCannotForge    tv nName (IntegerV nodeCannotForge)      nodeCannotForgeChanged    els ElNodeCannotForge
      setNodeIsLeader   tv nName (IntegerV nodeIsLeaderNum)      nodeIsLeaderNumChanged    els ElNodeIsLeaderNumber
      setSlotsMissed    tv nName (IntegerV slotsMissedNumber)    slotsMissedNumberChanged  els ElSlotsMissedNumber
      setTxsProcessed   tv nName (IntegerV txsProcessed)         txsProcessedChanged       els ElTxsProcessed
      setMPoolTxsNum    tv nName (IntegerV mempoolTxsNumber)     mempoolTxsNumberChanged   els ElMempoolTxsNumber
      setMPoolTxsPerc   tv nName (DoubleV  mempoolTxsPercent)    mempoolTxsPercentChanged   els ElMempoolTxsPercent
      setMPoolBytes     tv nName (Word64V  mempoolBytes)         mempoolBytesChanged       els ElMempoolBytes
      setMPoolBytesPerc tv nName (DoubleV  mempoolBytesPercent)  mempoolBytesPercentChanged els ElMempoolBytesPercent
      setMPoolMaxTxs    tv nName (IntegerV mempoolMaxTxs)        mempoolMaxTxsChanged      els ElMempoolMaxTxs
      setMPoolMaxBytes  tv nName (IntegerV mempoolMaxBytes)      mempoolMaxBytesChanged    els ElMempoolMaxBytes
      setRtsGcMajorNum  tv nName (IntegerV rtsGcMajorNum)        rtsGcMajorNumChanged      els ElRTSGcMajorNum
      setRtsGcMinorNum  tv nName (IntegerV rtsGcMinorNum)        rtsGcMinorNumChanged      els ElRTSGcMinorNum
      setStartKES       tv nName (IntegerV opCertStartKESPeriod)  opCertStartKESPeriodChanged  els ElOpCertStartKESPeriod
      setExpiryKES      tv nName (IntegerV opCertExpiryKESPeriod) opCertExpiryKESPeriodChanged els ElOpCertExpiryKESPeriod
      setCurrentKES     tv nName (IntegerV currentKESPeriod)      currentKESPeriodChanged      els ElCurrentKESPeriod
      setRemKES         tv nName (IntegerV remKESPeriods)       remKESPeriodsChanged       els ElRemainingKESPeriods
      setRemKESDays     tv nName (IntegerV remKESPeriodsInDays) remKESPeriodsInDaysChanged els ElRemainingKESPeriodsInDays
      setSystemStart    tv nName systemStartTime  systemStartTimeChanged els ElSystemStartTime
      setNodeCommit     tv nName nodeCommit nodeShortCommit nodeCommitChanged els ElNodeCommitHref

      updateCharts nName resourcesMetrics rtsMetrics nodeMetrics

type Setter = TVar NodesState
              -> Text
              -> ElementValue
              -> Bool
              -> NodeStateElements
              -> ElementName
              -> UI ()

setNodeVersion :: Setter
setNodeVersion _ _ _ False _ _ = return ()
setNodeVersion tv nameOfNode nodeVersion True els elName =
  whenJust (els !? elName) $ \el -> do
    let nodeVersionT = pack $ elValueToStr nodeVersion
    if strip nodeVersionT `elem` supportedNodesVersions
      then void $ setElement nodeVersion el
                    #. [] # set UI.title__ ""
      else void $ setElement nodeVersion el
                    #. [UnsupportedVersion]
                    # set UI.title__ ("Unsupported node version, please use these versions only: "
                                      <> unpack showSupportedNodesVersions)
    setChangedFlag tv nameOfNode $ \ns -> ns { nodeMetrics = (nodeMetrics ns) { nodeVersionChanged = False } }

setNodeProtocol
  , setNodePlatform
  , setEpoch
  , setSlot
  , setBlocksNumber
  , setChainDensity
  , setForgedNum
  , setCannotForge
  , setNodeIsLeader
  , setSlotsMissed
  , setTxsProcessed
  , setMPoolTxsNum
  , setMPoolTxsPerc
  , setMPoolBytes
  , setMPoolBytesPerc
  , setMPoolMaxTxs
  , setMPoolMaxBytes
  , setRtsGcMajorNum
  , setRtsGcMinorNum
  , setStartKES
  , setExpiryKES
  , setCurrentKES
  , setRemKES
  , setRemKESDays :: Setter
setNodeProtocol   = evSetter (\ns -> ns { nodeMetrics = (nodeMetrics ns) { nodeProtocolChanged = False } })
setNodePlatform   = evSetter (\ns -> ns { nodeMetrics = (nodeMetrics ns) { nodePlatformChanged = False } })
setEpoch          = evSetter (\ns -> ns { blockchainMetrics = (blockchainMetrics ns) { epochChanged        = False } })
setSlot           = evSetter (\ns -> ns { blockchainMetrics = (blockchainMetrics ns) { slotChanged         = False } })
setBlocksNumber   = evSetter (\ns -> ns { blockchainMetrics = (blockchainMetrics ns) { blocksNumberChanged = False } })
setChainDensity   = evSetter (\ns -> ns { blockchainMetrics = (blockchainMetrics ns) { chainDensityChanged = False } })
setForgedNum      = evSetter (\ns -> ns { forgeMetrics = (forgeMetrics ns) { blocksForgedNumberChanged = False } })
setCannotForge    = evSetter (\ns -> ns { forgeMetrics = (forgeMetrics ns) { nodeCannotForgeChanged    = False } })
setNodeIsLeader   = evSetter (\ns -> ns { forgeMetrics = (forgeMetrics ns) { nodeIsLeaderNumChanged    = False } })
setSlotsMissed    = evSetter (\ns -> ns { forgeMetrics = (forgeMetrics ns) { slotsMissedNumberChanged  = False } })
setTxsProcessed   = evSetter (\ns -> ns { mempoolMetrics = (mempoolMetrics ns) { txsProcessedChanged     = False } })
setMPoolTxsNum    = evSetter (\ns -> ns { mempoolMetrics = (mempoolMetrics ns) { mempoolTxsNumberChanged    = False } })
setMPoolTxsPerc   = evSetter (\ns -> ns { mempoolMetrics = (mempoolMetrics ns) { mempoolTxsPercentChanged   = False } })
setMPoolBytes     = evSetter (\ns -> ns { mempoolMetrics = (mempoolMetrics ns) { mempoolBytesChanged        = False } })
setMPoolBytesPerc = evSetter (\ns -> ns { mempoolMetrics = (mempoolMetrics ns) { mempoolBytesPercentChanged = False } })
setMPoolMaxTxs    = evSetter (\ns -> ns { mempoolMetrics = (mempoolMetrics ns) { mempoolMaxTxsChanged       = False } })
setMPoolMaxBytes  = evSetter (\ns -> ns { mempoolMetrics = (mempoolMetrics ns) { mempoolMaxBytesChanged     = False } })
setRtsGcMajorNum  = evSetter (\ns -> ns { rtsMetrics = (rtsMetrics ns) { rtsGcMajorNumChanged = False } })
setRtsGcMinorNum  = evSetter (\ns -> ns { rtsMetrics = (rtsMetrics ns) { rtsGcMinorNumChanged = False } })
setStartKES       = evSetter (\ns -> ns { kesMetrics = (kesMetrics ns) { opCertStartKESPeriodChanged  = False } })
setExpiryKES      = evSetter (\ns -> ns { kesMetrics = (kesMetrics ns) { opCertExpiryKESPeriodChanged = False } })
setCurrentKES     = evSetter (\ns -> ns { kesMetrics = (kesMetrics ns) { currentKESPeriodChanged      = False } })
setRemKES         = evSetter (\ns -> ns { kesMetrics = (kesMetrics ns) { remKESPeriodsChanged         = False } })
setRemKESDays     = evSetter (\ns -> ns { kesMetrics = (kesMetrics ns) { remKESPeriodsInDaysChanged   = False } })

evSetter
  :: (NodeState -> NodeState)
  -> TVar NodesState
  -> Text
  -> ElementValue
  -> Bool
  -> NodeStateElements
  -> ElementName
  -> UI ()
evSetter _ _ _ _ False _ _ = return ()
evSetter flagSetter tv nameOfNode ev True els elName =
  whenJust (els !? elName) $ \el -> do
    -- If the value is still default one, don't display it (it's meaningless).
    let nothing = StringV none
        ev' =
          case ev of
            IntV     _ -> ev
            IntegerV i -> if i < 0    then nothing else ev
            Word64V  w -> if w == 0   then nothing else ev
            DoubleV  d -> if d < 0    then nothing else ev
            StringV  s -> if null s   then nothing else ev
            TextV    t -> if T.null t then nothing else ev
    void $ setElement ev' el
    setChangedFlag tv nameOfNode flagSetter

setElement
  :: ElementValue
  -> Element
  -> UI Element
setElement ev el = element el # set text (elValueToStr ev)

elValueToStr :: ElementValue -> String
elValueToStr (IntV     i) = show i
elValueToStr (IntegerV i) = show i
elValueToStr (Word64V  w) = show w
elValueToStr (DoubleV  d) = showWith1DecPlace d
elValueToStr (StringV  s) = s
elValueToStr (TextV    t) = unpack t

updateProgressBar
  :: Double
  -> NodeStateElements
  -> ElementName
  -> UI ()
updateProgressBar percents els elName =
  whenJust (els !? elName) $ \bar ->
    void $ element bar # set style [("width", showWith1DecPlace preparedPercents <> "%")]
 where
  -- Sometimes (for CPU usage) percents can be bigger than 100%,
  -- in this case actual width of bar should be 100%.
  preparedPercents = if percents > 100.0 then 100.0 else percents

setSystemStart
  :: TVar NodesState
  -> Text
  -> UTCTime
  -> Bool
  -> NodeStateElements
  -> ElementName
  -> UI ()
setSystemStart _ _ _ False _ _ = return ()
setSystemStart tv nameOfNode systemStart True els elName =
  whenJust (els !? elName) $ \el -> do
    void $ element el # set text systemStartFormatted
    setChangedFlag tv
                   nameOfNode
                   (\ns -> ns { blockchainMetrics = (blockchainMetrics ns) { systemStartTimeChanged = False } })
 where
  systemStartFormatted = formatTime defaultTimeLocale "%F %T %Z" systemStart

setNodeUpTime
  :: UTCTime
  -> NodeStateElements
  -> ElementName
  -> UI ()
setNodeUpTime startTime els elName =
  whenJust (els !? elName) $ \el -> do
    upTimeDiff <-
      if startTime /= nullTime
        then do
          -- nodeStartTime received from the node.
          now <- liftIO getCurrentTime
          let upTimeDiff = now `diffUTCTime` startTime
          return upTimeDiff
        else
          -- No nodeStartTime were received (yet).
          return 0

    if upTimeDiff == 0
      then
        void $ element el # set text "00:00:00"
      else do
        let upTime = upTimeDiff `addUTCTime` nullTime
            upTimeFormatted = formatTime defaultTimeLocale "%X" upTime
            daysNum = utctDay upTime `diffDays` utctDay nullTime
            upTimeWithDays = if daysNum > 0
                               -- Show days only if upTime is bigger than 23:59:59.
                               then show daysNum <> "d " <> upTimeFormatted
                               else upTimeFormatted
        void $ element el # set text upTimeWithDays

setNodeStartTime
  :: TVar NodesState
  -> Text
  -> UTCTime
  -> Bool
  -> NodeStateElements
  -> ElementName
  -> UI ()
setNodeStartTime _ _ _ False _ _ = return ()
setNodeStartTime tv nameOfNode startTime True els elName =
  whenJust (els !? elName) $ \el -> do
    if startTime /= nullTime
      then void $ element el # set text startTimeFormatted
      else void $ element el # set text none
    setChangedFlag tv
                   nameOfNode
                   (\ns -> ns { nodeMetrics = (nodeMetrics ns) { nodeStartTimeChanged = False } })
 where
  startTimeFormatted = formatTime defaultTimeLocale "%F %T %Z" startTime

setNodeCommit
  :: TVar NodesState
  -> Text
  -> Text
  -> Text
  -> Bool
  -> NodeStateElements
  -> ElementName
  -> UI ()
setNodeCommit _ _ _ _ False _ _ = return ()
setNodeCommit tv nameOfNode commit shortCommit True els elName =
  whenJust (els !? elName) $ \el -> do
    void $ element el # set children []
    void $ element el #+ [ UI.anchor # set UI.href ("https://github.com/input-output-hk/cardano-node/commit/"
                                       <> unpack commit)
                                     # set UI.target "_blank"
                                     # set UI.title__ "Browse cardano-node repository on this commit"
                                     # set UI.text (showText shortCommit)
                         ]
    setChangedFlag tv
                   nameOfNode
                   (\ns -> ns { nodeMetrics = (nodeMetrics ns) { nodeCommitChanged = False } })

-- | Since peers list will be changed dynamically, we need it
--   to update corresponding HTML-murkup dynamically as well.
--   Please note that we don't change DOM actully (to avoid possible space leak).
updatePeersList
  :: TVar NodesState
  -> Text
  -> [PeerInfo]
  -> Bool
  -> [PeerInfoItem]
  -> UI ()
updatePeersList _ _ _ False _ = return ()
updatePeersList tv nameOfNode peersInfo' True peersInfoItems = do
  -- The number of connected peers may reduce, so first of all hide all items.
  mapM_ (hideElement . piItem) peersInfoItems

  let peersInfo =
        if length peersInfo' > length peersInfoItems
          then
            -- We prepared peer items for known number of connected peers,
            -- but the number of connected peers is bigger than prepared items.
            -- Show only first N items.
            take (length peersInfoItems) peersInfo'
          else
            peersInfo'
  -- Show N items, corresponding to the number of connected peers,
  -- and fill them with actual values.
  let peersInfoWithIndices = zip peersInfo [0 .. length peersInfo - 1]
  forM_ peersInfoWithIndices $ \(PeerInfo {..}, i) -> do
    let item  = peersInfoItems L.!! i
        PeerInfoElements {..} = piItemElems item
    -- Update internal elements of item using actual values.
    void $ setElement (TextV piEndpoint)   pieEndpoint
    void $ setElement (TextV piBytesInF)   pieBytesInF
    void $ setElement (TextV piReqsInF)    pieReqsInF
    void $ setElement (TextV piBlocksInF)  pieBlocksInF
    void $ setElement (TextV piSlotNumber) pieSlotNumber
    void $ setElement (TextV piStatus)     pieStatus
    -- Make item visible.
    showElement $ piItem item
  setChangedFlag tv
                 nameOfNode
                 (\ns -> ns { peersMetrics = (peersMetrics ns) { peersInfoChanged = False } })

updateErrorsListAndTab
  :: UI.Window
  -> TVar NodesState
  -> TVar TmpElements
  -> Text
  -> [NodeError]
  -> Bool
  -> NodeStateElements
  -> ElementName
  -> ElementName
  -> ElementName
  -> UI ()
updateErrorsListAndTab _ _ _ _ _ False _ _ _ _ = return ()
updateErrorsListAndTab window tv tmpElsTVar nameOfNode nodeErrors' True els
                       elName elTabName elTabBadgeName = do
  let maybeEl         = els !? elName
      maybeElTab      = els !? elTabName
      maybeElTabBadge = els !? elTabBadgeName
  when (   isJust maybeEl
        && isJust maybeElTab
        && isJust maybeElTabBadge) $ do
    let el         = fromJust maybeEl
        elTab      = fromJust maybeElTab
        elTabBadge = fromJust maybeElTabBadge

    nss <- liftIO $ readTVarIO tv
    let shouldWeRebuild = errorsRebuild . nodeErrors $ (nss ! nameOfNode)
    justUpdateErrorsListAndTab window
                               tmpElsTVar
                               nameOfNode
                               nodeErrors'
                               shouldWeRebuild
                               el elTab elTabBadge

    unless (null nodeErrors') $
      void $ return window # set UI.title pageTitleNotify

    setChangedFlag tv
                   nameOfNode
                   (\ns -> ns { nodeErrors = (nodeErrors ns) { errorsChanged = False } })

justUpdateErrorsListAndTab
  :: UI.Window
  -> TVar TmpElements
  -> Text
  -> [NodeError]
  -> Bool
  -> Element
  -> Element
  -> Element
  -> UI ()
justUpdateErrorsListAndTab window
                           tmpElsTVar
                           nameOfNode
                           nodeErrors'
                           shouldWeRebuild
                           elErrors elTab elTabBadge = do
  if null nodeErrors'
    then do
      void $ element elTab # set UI.enabled False
                           # set UI.title__ errorsTabTitle
      void $ element elTabBadge # hideIt
                                # set text ""
      whenJustM (UI.getElementById window (show ErrorsTabsSwitcher)) $ \switcher ->
        void $ element switcher #. [W3BarItem, W3Button, W3Mobile, W3Disabled]
                                # set UI.title__ "Good news: there are no errors!"
    else do
      void $ element elTab # set UI.enabled True
                           # set UI.title__ errorsTabTitle
      void $ element elTabBadge # showInline
                                # set text (show . length $ nodeErrors')
      whenJustM (UI.getElementById window (show ErrorsTabsSwitcher)) $ \switcher ->
        void $ element switcher #. [W3BarItem, W3Button, W3Mobile]
                                # set UI.title__ "Open Errors tab for all nodes"

  showFiltersOnlyForExistingSeverities

  tmpEls <- liftIO $ readTVarIO tmpElsTVar
  let tmpElsForThisNode = maybe [] id $ tmpEls !? nameOfNode
  visibleNodeErrors <-
    if shouldWeRebuild
      then do
        -- It means that the user sorted, (un)filtered or deleted errors.
        -- In this case we have to rebuild all the list from scratch,
        -- so explicitly delete all tmp Elements corresponding to shown errors.
        -- GC will clean them up later.
        mapM_ UI.delete tmpElsForThisNode
        -- Please note that when the user filters errors,
        -- we don't remove them, just hide them.
        -- So only visible errors should be displayed.
        return $ filter eVisible nodeErrors'
      else do
        -- It means that there's no actions from the user's side,
        -- just some new errors arrived. In this case append them
        -- at the end of already shown errors.
        let alreadyShownErrors = tmpElsForThisNode
            onlyNewErrors = drop (length alreadyShownErrors) nodeErrors'
        return onlyNewErrors

  errors <- forM visibleNodeErrors $ \(NodeError utcTimeStamp sev msg _) -> do
    let (aClass, aTagClass, aTag, aTagTitle) =
          case sev of
            Warning   -> (WarningMessage,   WarningMessageTag,   "W", "Warning")
            Error     -> (ErrorMessage,     ErrorMessageTag,     "E", "Error")
            Critical  -> (CriticalMessage,  CriticalMessageTag,  "C", "Critical")
            Alert     -> (AlertMessage,     AlertMessageTag,     "A", "Alert")
            Emergency -> (EmergencyMessage, EmergencyMessageTag, "E", "Emergency")
            _         -> (NoClass,          NoClass,             "",  "")

    let timeStamp = formatTime defaultTimeLocale "%F %T %Z" utcTimeStamp

    UI.div #. [W3Row, ErrorRow] #+
      [ UI.div #. [W3Third, ErrorTimestamp] #+
          [ UI.string timeStamp
          ]
      , UI.div #. [W3TwoThird] #+
          [ UI.string aTag #. [aTagClass] # set UI.title__ aTagTitle
          , UI.string (unpack msg) #. [aClass]
          ]
      ]

  if shouldWeRebuild
    then do
      void $ element elErrors # set children []
      void $ element elErrors # set children errors
    else do
      errors' <- mapM (fmap element . return) errors
      void $ element elErrors #+ errors'

  liftIO . atomically $ modifyTVar' tmpElsTVar $ \tmpEls' ->
    if shouldWeRebuild
      then
        HM.adjust (const errors) nameOfNode tmpEls'
      else
        let alreadyShownErrors = maybe [] id $ tmpEls' !? nameOfNode
            errorsWithNewOnes = alreadyShownErrors ++ errors
        in HM.adjust (const errorsWithNewOnes) nameOfNode tmpEls'
 where
  errorsTabTitle =
    case length nodeErrors' of
      0 -> "Good news: there are no errors!"
      1 -> "There is one error from node"
      n -> "There are " <> show n <> " errors from node"

  -- There are 5 severity levels, but we have to provide an ability to filter
  -- errors using only existing severities (i.e. if there is no errors with Alert
  -- severity, we have to hide Alert filter).
  showFiltersOnlyForExistingSeverities = do
    let existingSeverities = map eSeverity nodeErrors'
        nameOfNodeS = unpack nameOfNode

        forFilter anId action =
          whenJustM (UI.getElementById window (show anId <> nameOfNodeS)) $ \aFilter ->
            void $ element aFilter # action

        showOrHide sev anId =
          if sev `elem` existingSeverities
            then forFilter anId showIt
            else forFilter anId hideIt

    showOrHide Warning   WarningMessageId
    showOrHide Error     ErrorMessageId
    showOrHide Critical  CriticalMessageId
    showOrHide Alert     AlertMessageId
    showOrHide Emergency EmergencyMessageId

-- Check the errors for all nodes: if there's no errors at all,
-- set the page's title to default one.
resetPageTitleIfNeeded
  :: UI.Window
  -> TVar NodesState
  -> UI ()
resetPageTitleIfNeeded window tv = do
  nodesState <- liftIO $ readTVarIO tv
  noErrors <- forM (HM.elems nodesState) (return . null . errors . nodeErrors)
  when (all (True ==) noErrors) $
    void $ return window # set UI.title pageTitle

showElement, hideElement :: Element -> UI Element
showElement w = element w # set UI.style [("display", "inline")]
hideElement w = element w # set UI.style [("display", "none")]

updateCharts
  :: Text
  -> ResourcesMetrics
  -> RTSMetrics
  -> NodeMetrics
  -> UI ()
updateCharts nameOfNode rm rtm nm = do
  now <- liftIO getCurrentTime
  let ts :: String
      ts = formatTime defaultTimeLocale "%M:%S" time
      time = timeDiff `addUTCTime` nullTime
      timeDiff :: NominalDiffTime
      timeDiff = now `diffUTCTime` nodeStartTime nm

  UI.runFunction $ UI.ffi Chart.updateMemoryUsageChartJS  mN ts (memory rm) (rtsMemoryUsed rtm)
  UI.runFunction $ UI.ffi Chart.updateCPUUsageChartJS     cN ts (cpuPercent rm) (rtsMutPercent rtm) (rtsGCPercent rtm)
  UI.runFunction $ UI.ffi Chart.updateDiskUsageChartJS    dN ts (diskUsageR rm) (diskUsageW rm)
  UI.runFunction $ UI.ffi Chart.updateNetworkUsageChartJS nN ts (networkUsageIn rm) (networkUsageOut rm)
 where
  mN = showt MemoryUsageChartId  <> nameOfNode
  cN = showt CPUUsageChartId     <> nameOfNode
  dN = showt DiskUsageChartId    <> nameOfNode
  nN = showt NetworkUsageChartId <> nameOfNode

  showt :: Show a => a -> Text
  showt = pack . show

-- | If no metrics was received from the node for a long time
--   (more than 'active-node-life') this node is treated as idle.
--   Technically it means that the node is disconnected from RTView
--   or it wasn't connected to RTView at all.
checkIfNodeIsIdlePane
  :: RTViewParams
  -> Word64
  -> Element
  -> Element
  -> UI Bool
checkIfNodeIsIdlePane params metricsLastUpdate idleTag nodePane =
  checkIfNodeIsIdle params
                    metricsLastUpdate
                    idleTag
                    (void $ element nodePane # set UI.style [("opacity", "0.7")])
                    (void $ element nodePane # set UI.style [("opacity", "1.0")])

checkIfNodeIsIdle
  :: RTViewParams
  -> Word64
  -> Element
  -> UI ()
  -> UI ()
  -> UI Bool
checkIfNodeIsIdle RTViewParams {..}
                  metricsLastUpdate
                  idleTag
                  additionalActionOnIdle
                  additionalActionOnActive = do
  let lifetimeInNSec = secToNanosec rtvActiveNodeLife
  now <- liftIO getMonotonicTimeNSec
  if now - metricsLastUpdate > lifetimeInNSec
    then do
      markNodeAsIdle
      return True
    else do
      markNodeAsActive
      return False
 where
  secToNanosec :: Int -> Word64
  secToNanosec s = fromIntegral $ s * 1000000000
  markNodeAsIdle = do
    void $ showElement idleTag # set UI.title__ ("Node metrics have not been received for more than "
                                                 <> show rtvActiveNodeLife <> " seconds")
    additionalActionOnIdle
  markNodeAsActive = do
    void $ hideElement idleTag # set UI.title__ ""
    additionalActionOnActive

-- | After we updated the value of DOM-element, we set 'changed'-flag
--   to False to avoid useless re-update by the same value.
setChangedFlag
  :: TVar NodesState
  -> Text
  -> (NodeState -> NodeState)
  -> UI ()
setChangedFlag nsTVar nameOfNode mkNewNS =
  liftIO . atomically $ modifyTVar' nsTVar $ \currentNS ->
    case currentNS !? nameOfNode of
      Just ns -> HM.adjust (const $ mkNewNS ns) nameOfNode currentNS
      Nothing -> currentNS
      -}
