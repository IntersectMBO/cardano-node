{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Tracer.Handlers.RTView.UI.Elements
  ( PageElements
  , NodePanelElements
  , TraceObjectName
  , ElementValue (..)
    {-
  HTMLClass (..)
  , HTMLId (..)
  , NodesStateElements
  , NodeStateElements
  , ElementName (..)
  , ElementValue (..)
  , PeerInfoItem (..)
  , PeerInfoElements (..)
  , TmpElements
  , initialTmpElements
  , (#.)
  , (##)
  , dataAttr
  , showIt
  , showInline
  , hideIt
  , pageTitle
  , pageTitleNotify
  -}
  ) where

import           Graphics.UI.Threepenny.Core (Element, UI, (#))
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)

import           Cardano.Tracer.Types (NodeId)

-- | TraceObjectName is created from TraceObject's Namespace.
type TraceObjectName = Text

-- | The displayed value of each Element will be taken from
--   the corresponding TraceObject, that's why TraceObject's
--   name is a key here.
--   The second value in a pair stores the current value of Element,
--   we need it to check if the new value of the corresponding TraceObject
--   differs from the current one (to avoid useless updating of Element).
type NodePanelElements = HashMap TraceObjectName (Element, ElementValue)

-- | The first value in a pair is a node panel: we keep it to be able to
--   delete it if the corresponding node was disconnected from 'cardano-tracer'.
type PageElements = HashMap NodeId (Element, NodePanelElements)

data ElementValue
  = StringV !String
  | TextV   !Text




  {-
import           Control.DeepSeq (NFData (..), rwhnf)
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, (#))
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           Data.Word (Word64)

import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))

instance NFData Element where
  rnf = rwhnf

-- | GUI elements containing current node state (info, metrics).
--   These elements are continuously updating using |LogObject|s
--   received by |TraceAcceptor|s.
type NodeStateElements = HashMap ElementName Element

-- | GUI elements for all nodes, tuples from nodeName, its elements and prepared peers items.
type NodesStateElements = [(Text, NodeStateElements, [PeerInfoItem])]

data ElementName
  = ElNodePane
  | ElIdleNode
  | ElNodeProtocol
  | ElNodeVersion
  | ElNodePlatform
  | ElNodeCommitHref
  | ElNodeStarttime
  | ElNodeUptime
  | ElSystemStartTime
  | ElEpoch
  | ElSlot
  | ElBlocksNumber
  | ElBlocksForgedNumber
  | ElNodeCannotForge
  | ElChainDensity
  | ElNodeIsLeaderNumber
  | ElSlotsMissedNumber
  | ElTxsProcessed
  | ElPeersNumber
  | ElTraceAcceptorHost
  | ElTraceAcceptorPort
  | ElTraceAcceptorEndpoint
  | ElOpCertStartKESPeriod
  | ElOpCertExpiryKESPeriod
  | ElCurrentKESPeriod
  | ElRemainingKESPeriods
  | ElRemainingKESPeriodsInDays
  | ElNodeErrors
  | ElNodeErrorsTab
  | ElNodeErrorsTabBadge
  | ElMempoolTxsNumber
  | ElMempoolTxsPercent
  | ElMempoolBytes
  | ElMempoolBytesPercent
  | ElMempoolMaxTxs
  | ElMempoolMaxBytes
  | ElRTSGcMajorNum
  | ElRTSGcMinorNum
  -- Progress bars.
  | ElMempoolBytesProgress
  | ElMempoolBytesProgressBox
  | ElMempoolTxsProgress
  | ElMempoolTxsProgressBox
  -- Charts
  | ElMemoryUsageChart
  | ElCPUUsageChart
  | ElDiskUsageChart
  | ElNetworkUsageChart
  deriving (Eq, Generic, NFData, Ord, Show)

instance Hashable ElementName

data ElementValue
  = IntV     !Int
  | IntegerV !Integer
  | Word64V  !Word64
  | DoubleV  !Double
  | StringV  !String
  | TextV    !Text
  deriving (Generic, NFData)

-- | An item for each connected peer, contains a parent element
--   and list of child elements.
data PeerInfoItem = PeerInfoItem
  { piItem      :: !Element
  , piItemElems :: !PeerInfoElements
  } deriving (Generic, NFData)

data PeerInfoElements = PeerInfoElements
  { pieEndpoint   :: !Element
  , pieBytesInF   :: !Element
  , pieReqsInF    :: !Element
  , pieBlocksInF  :: !Element
  , pieSlotNumber :: !Element
  , pieStatus     :: !Element
  } deriving (Generic, NFData)

-- | The storage for temporary Elements, which should be deleted explicitly (separated for each node).
--   Please read the documentation for details:
--   https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:delete
type TmpElements = HashMap Text [Element]

initialTmpElements :: [RemoteAddrNamed] -> TmpElements
initialTmpElements = HM.fromList . map (\(RemoteAddrNamed nameOfNode _) -> (nameOfNode, []))

-- | HTML elements identifiers, we use them in HTML, CSS and JS FFI.

data HTMLClass
  = NoClass
  | ActiveTab
  | AllTabsIcon
  | BarValueUnit
  | CardanoLogo
  | CommitLink
  | DensityPercent
  | ErrorRow
  | ErrorsBadge
  | ErrorsTabHeader
  | ErrorsTabList
  | ErrorsDownloadIcon
  | ErrorsSortIcon
  | ErrorsFilterIcon
  | ErrorsFilterDropdownIcon
  | ErrorsRemoveIcon
  | ErrorTimestamp
  | HSpacer
  | IdleNode
  | InfoMark
  | InfoMarkImg
  | MetricsArea
  | NodeContainer
  | NodeBar
  | NodeInfoValues
  | NodeInfoVSpacer
  | NodeMetricsValues
  | NodeMetricsVSpacer
  | NodeMenuIcon
  | NodeName
  | NodeNameArea
  | NodePaneArea
  | NotificationsBar
  | NotificationsEventsHeader
  | NotificationsIcon
  | NotificationsIconSlash
  | NotificationsInput
  | NotificationsHR
  | NotificationsMainSwitch
  | NotificationsSwitch
  | NotificationsSwitches
  | NotificationsTabContainer
  | NotificationsVSpacer
  | PercentsSlashHSpacer
  | ProgressBar
  | ProgressBarBox
  | ResourcesIcon
  | ResourcesDropdownIcon
  | RequiredInput
  | Round
  | RTViewInfoClose
  | RTViewInfoContainer
  | RTViewInfoCopyPathIcon
  | RTViewInfoIcon
  | RTViewInfoTop
  | SearchErrorArea
  | SearchErrorIcon
  | SearchErrorInput
  | SelectMetricCheck
  | SelectMetricCheckArea
  | SelectNodeCheck
  | SelectNodeCheckArea
  | ServiceName
  | ShowHideIcon
  | Slider
  | Switch
  | SwitchContainer
  | TabContainer
  | TestEmailButton
  | TestEmailButtonArea
  | TestEmailContainer
  | TestEmailDismiss
  | TestEmailResult
  | TestEmailResultSuccess
  | TestEmailResultError
  | TopBar
  | TopNavDropdownIcon
  | TXsProcessed
  | UnsupportedVersion
  | ValueUnit
  | ValueUnitPercent
  -- Charts
  | ChartArea
  -- Error messages
  | WarningMessage
  | WarningMessageTag
  | WarningMessageTagNoHelp
  | ErrorMessage
  | ErrorMessageTag
  | ErrorMessageTagNoHelp
  | CriticalMessage
  | CriticalMessageTag
  | CriticalMessageTagNoHelp
  | AlertMessage
  | AlertMessageTag
  | AlertMessageTagNoHelp
  | EmergencyMessage
  | EmergencyMessageTag
  | EmergencyMessageTagNoHelp
  -- W3C classes
  | W3AnimateTop
  | W3Badge
  | W3Bar
  | W3BarBlock
  | W3BarItem
  | W3Border
  | W3Bordered
  | W3BorderBottom
  | W3BorderTop
  | W3Button
  | W3Card2
  | W3Card4
  | W3Check
  | W3Col
  | W3Container
  | W3Disabled
  | W3DisplayTopright
  | W3DropdownContent
  | W3DropdownHover
  | W3Half
  | W3HideMedium
  | W3HideSmall
  | W3Input
  | W3Large
  | W3Margin
  | W3Mobile
  | W3Modal
  | W3ModalContent
  | W3PaddingSmall
  | W3Responsive
  | W3Rest
  | W3Right
  | W3RightAlign
  | W3Round
  | W3Row
  | W3RowPadding
  | W3Select
  | W3Sidebar
  | W3Table
  | W3Theme
  | W3Third
  | W3ThreeQuarter
  | W3TwoThird
  | W3Quarter
  | W3L3
  | W3L4
  | W3L6
  | W3L12
  | W3M12
  | W3S12

instance Show HTMLClass where
  show NoClass                    = ""
  show ActiveTab                  = "ActiveTab"
  show AllTabsIcon                = "AllTabsIcon"
  show BarValueUnit               = "BarValueUnit"
  show CardanoLogo                = "CardanoLogo"
  show CommitLink                 = "CommitLink"
  show DensityPercent             = "DensityPercent"
  show ErrorRow                   = "ErrorRow"
  show ErrorsBadge                = "ErrorsBadge"
  show ErrorsTabHeader            = "ErrorsTabHeader"
  show ErrorsTabList              = "ErrorsTabList"
  show ErrorsDownloadIcon         = "ErrorsDownloadIcon"
  show ErrorsSortIcon             = "ErrorsSortIcon"
  show ErrorsFilterIcon           = "ErrorsFilterIcon"
  show ErrorsFilterDropdownIcon   = "ErrorsFilterDropdownIcon"
  show ErrorsRemoveIcon           = "ErrorsRemoveIcon"
  show ErrorTimestamp             = "ErrorTimestamp"
  show HSpacer                    = "HSpacer"
  show IdleNode                   = "IdleNode"
  show InfoMark                   = "InfoMark"
  show InfoMarkImg                = "InfoMarkImg"
  show MetricsArea                = "MetricsArea"
  show NodeContainer              = "NodeContainer"
  show NodeBar                    = "NodeBar"
  show NodeInfoValues             = "NodeInfoValues"
  show NodeInfoVSpacer            = "NodeInfoVSpacer"
  show NodeMetricsValues          = "NodeMetricsValues"
  show NodeMetricsVSpacer         = "NodeMetricsVSpacer"
  show NodeMenuIcon               = "NodeMenuIcon"
  show NodeName                   = "NodeName"
  show NodeNameArea               = "NodeNameArea"
  show NodePaneArea               = "NodePaneArea"
  show NotificationsBar           = "NotificationsBar"
  show NotificationsEventsHeader  = "NotificationsEventsHeader"
  show NotificationsIcon          = "NotificationsIcon"
  show NotificationsIconSlash     = "NotificationsIconSlash"
  show NotificationsInput         = "NotificationsInput"
  show NotificationsHR            = "NotificationsHR"
  show NotificationsMainSwitch    = "NotificationsMainSwitch"
  show NotificationsSwitch        = "NotificationsSwitch"
  show NotificationsSwitches      = "NotificationsSwitches"
  show NotificationsTabContainer  = "NotificationsTabContainer"
  show NotificationsVSpacer       = "NotificationsVSpacer"
  show PercentsSlashHSpacer       = "PercentsSlashHSpacer"
  show ProgressBar                = "ProgressBar"
  show ProgressBarBox             = "ProgressBarBox"
  show ResourcesIcon              = "ResourcesIcon"
  show ResourcesDropdownIcon      = "ResourcesDropdownIcon"
  show RequiredInput              = "RequiredInput"
  show Round                      = "Round"
  show RTViewInfoClose            = "RTViewInfoClose"
  show RTViewInfoContainer        = "RTViewInfoContainer"
  show RTViewInfoCopyPathIcon     = "RTViewInfoCopyPathIcon"
  show RTViewInfoIcon             = "RTViewInfoIcon"
  show RTViewInfoTop              = "RTViewInfoTop"
  show SearchErrorArea            = "SearchErrorArea"
  show SearchErrorIcon            = "SearchErrorIcon"
  show SearchErrorInput           = "SearchErrorInput"
  show SelectMetricCheck          = "SelectMetricCheck"
  show SelectMetricCheckArea      = "SelectMetricCheckArea"
  show SelectNodeCheck            = "SelectNodeCheck"
  show SelectNodeCheckArea        = "SelectNodeCheckArea"
  show ServiceName                = "ServiceName"
  show ShowHideIcon               = "ShowHideIcon"
  show Slider                     = "Slider"
  show Switch                     = "Switch"
  show SwitchContainer            = "SwitchContainer"
  show TabContainer               = "TabContainer"
  show TestEmailButton            = "TestEmailButton"
  show TestEmailButtonArea        = "TestEmailButtonArea"
  show TestEmailContainer         = "TestEmailContainer"
  show TestEmailDismiss           = "TestEmailDismiss"
  show TestEmailResult            = "TestEmailResult"
  show TestEmailResultSuccess     = "TestEmailResultSuccess"
  show TestEmailResultError       = "TestEmailResultError"
  show TopBar                     = "TopBar"
  show TopNavDropdownIcon         = "TopNavDropdownIcon"
  show TXsProcessed               = "TXsProcessed"
  show UnsupportedVersion         = "UnsupportedVersion"
  show ValueUnit                  = "ValueUnit"
  show ValueUnitPercent           = "ValueUnitPercent"
  show ChartArea                  = "ChartArea"
  show WarningMessage             = "WarningMessage"
  show WarningMessageTag          = "WarningMessageTag"
  show WarningMessageTagNoHelp    = "WarningMessageTagNoHelp"
  show ErrorMessage               = "ErrorMessage"
  show ErrorMessageTag            = "ErrorMessageTag"
  show ErrorMessageTagNoHelp      = "ErrorMessageTagNoHelp"
  show CriticalMessage            = "CriticalMessage"
  show CriticalMessageTag         = "CriticalMessageTag"
  show CriticalMessageTagNoHelp   = "CriticalMessageTagNoHelp"
  show AlertMessage               = "AlertMessage"
  show AlertMessageTag            = "AlertMessageTag"
  show AlertMessageTagNoHelp      = "AlertMessageTagNoHelp"
  show EmergencyMessage           = "EmergencyMessage"
  show EmergencyMessageTag        = "EmergencyMessageTag"
  show EmergencyMessageTagNoHelp  = "EmergencyMessageTagNoHelp"
  -- Names of these classes are taken from W3C-library.
  show W3AnimateTop      = "w3-animate-top"
  show W3Badge           = "w3-badge"
  show W3Bar             = "w3-bar"
  show W3BarBlock        = "w3-bar-block"
  show W3BarItem         = "w3-bar-item"
  show W3Border          = "w3-border"
  show W3Bordered        = "w3-bordered"
  show W3BorderBottom    = "w3-border-bottom"
  show W3BorderTop       = "w3-border-top"
  show W3Button          = "w3-button"
  show W3Card2           = "w3-card-2"
  show W3Card4           = "w3-card-4"
  show W3Check           = "w3-check"
  show W3Col             = "w3-col"
  show W3Container       = "w3-container"
  show W3Disabled        = "w3-disabled"
  show W3DisplayTopright = "w3-display-topright"
  show W3DropdownContent = "w3-dropdown-content"
  show W3DropdownHover   = "w3-dropdown-hover"
  show W3Half            = "w3-half"
  show W3HideMedium      = "w3-hide-medium"
  show W3HideSmall       = "w3-hide-small"
  show W3Input           = "w3-input"
  show W3Large           = "w3-large"
  show W3Margin          = "w3-margin"
  show W3Mobile          = "w3-mobile"
  show W3Modal           = "w3-modal"
  show W3ModalContent    = "w3-modal-content"
  show W3PaddingSmall    = "w3-padding-small"
  show W3Responsive      = "w3-responsive"
  show W3Rest            = "w3-rest"
  show W3Right           = "w3-right"
  show W3RightAlign      = "w3-right-align"
  show W3Round           = "w3-round"
  show W3Row             = "w3-row"
  show W3RowPadding      = "w3-row-padding"
  show W3Select          = "w3-select"
  show W3Sidebar         = "w3-sidebar"
  show W3Table           = "w3-table"
  show W3Theme           = "w3-theme"
  show W3Third           = "w3-third"
  show W3ThreeQuarter    = "w3-threequarter"
  show W3TwoThird        = "w3-twothird"
  show W3Quarter         = "w3-quarter"
  show W3L3              = "l3"
  show W3L4              = "l4"
  show W3L6              = "l6"
  show W3L12             = "l12"
  show W3M12             = "m12"
  show W3S12             = "s12"

data HTMLId
  = SelectMetricButton
  | HideAllMetricsButton
  | ShowAllMetricsButton
  | HideAllNodesButton
  | ShowAllNodesButton
  | ViewModeButton
  -- Tabs
  | NodeInfoTab
  | KESTab
  | PeersTab
  | BlockchainTab
  | MempoolTab
  | RTSGCTab
  | ResTabCPU
  | ResTabMemory
  | ResTabDisk
  | ResTabNetwork
  | ErrorsTab
  | ErrorsTabsSwitcher
  -- Id parts (the final id will be formed using unique name of node).
  | CPUUsageChartId
  | DiskUsageChartId
  | MemoryUsageChartId
  | NetworkUsageChartId
  -- Form inputs
  | EmailFromInput
  | EmailToInput
  | PasswordInput
  | ServerHostInput
  | ServerPortInput
  | SSLInput
  | SubjectInput
  | UsernameInput
  -- | Errors filters
  | WarningMessageId
  | ErrorMessageId
  | CriticalMessageId
  | AlertMessageId
  | EmergencyMessageId
  | SearchErrorInputId
  deriving Show

(##) :: UI Element -> String  -> UI Element
(##) el i = el # UI.set UI.id_ i

(#.) :: UI Element -> [HTMLClass] -> UI Element
(#.) el []   = el # UI.set UI.class_ ""
(#.) el [cl] = el # UI.set UI.class_ (show cl)
(#.) el cls  = el # UI.set UI.class_ (unwords $ map show cls)

showIt, showInline, hideIt :: UI Element -> UI Element
showIt     = UI.set UI.style [("display", "block")]
showInline = UI.set UI.style [("display", "inline")]
hideIt     = UI.set UI.style [("display", "none")]

pageTitle, pageTitleNotify :: String
pageTitle       = "Cardano RTView"
pageTitleNotify = "(!) " <> pageTitle

dataAttr :: String -> UI.Attr UI.Element String
dataAttr name = UI.mkReadWriteAttr getData setData
 where
  getData   el = UI.callFunction $ UI.ffi "$(%1).data(%2)" el name
  setData v el = UI.runFunction  $ UI.ffi "$(%1).data(%2,%3)" el name v
  -}
