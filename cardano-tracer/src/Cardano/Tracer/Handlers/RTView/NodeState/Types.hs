{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.RTView.NodeState.Types
    ( NodesState
    , PeerMetrics (..)
    , MempoolMetrics (..)
    , ForgeMetrics (..)
    , ResourcesMetrics (..)
    , RTSMetrics (..)
    , BlockchainMetrics (..)
    , KESMetrics (..)
    , NodeMetrics (..)
    , ErrorsMetrics (..)
    , NodeState (..)
    , NodeError (..)
    , PeerInfo (..)
    , Era (..)
    , initialNodesState
    , showText
    , showInitTime
    , showTime
    , showInteger
    , showDouble
    , showWith1DecPlace
    , showWord64
    , none
    , mkTraceAcceptorEndpoint
    , fullEndpointTitle
    , nullTime
    , sortErrorsByTimeAsc
    , sortErrorsByTimeDesc
    , sortErrorsBySevAsc
    , sortErrorsBySevDesc
    ) where

import           Control.DeepSeq (NFData (..))
import qualified Data.List as L
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day (..))
import           Data.Time.Clock (UTCTime (..))
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Generics (Generic)
import           Formatting (fixed, sformat, (%))

import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Configuration (RemoteAddr (..), RemoteAddrNamed (..))
import           Cardano.BM.Data.Severity (Severity (..))

type NodesState = HashMap Text NodeState

data PeerInfo = PeerInfo
  { piEndpoint   :: !Text
  , piBytesInF   :: !Text
  , piReqsInF    :: !Text
  , piBlocksInF  :: !Text
  , piSlotNumber :: !Text
  , piStatus     :: !Text
  } deriving (Eq, Generic, NFData, Show)

-- Severity type already has Generic instance.
instance NFData Severity

data NodeError = NodeError
  { eTimestamp :: !UTCTime
  , eSeverity  :: !Severity
  , eMessage   :: !Text
  , eVisible   :: !Bool
  } deriving (Generic, NFData, Show)

sortErrorsByTimeAsc
  , sortErrorsByTimeDesc
  , sortErrorsBySevAsc
  , sortErrorsBySevDesc :: NodeError -> NodeError -> Ordering
sortErrorsByTimeAsc  ne1 ne2 = eTimestamp ne1 `compare` eTimestamp ne2
sortErrorsByTimeDesc ne1 ne2 = eTimestamp ne2 `compare` eTimestamp ne1
sortErrorsBySevAsc   ne1 ne2 = eSeverity  ne1 `compare` eSeverity  ne2
sortErrorsBySevDesc  ne1 ne2 = eSeverity  ne2 `compare` eSeverity  ne1

data NodeState = NodeState
  { peersMetrics      :: !PeerMetrics
  , mempoolMetrics    :: !MempoolMetrics
  , forgeMetrics      :: !ForgeMetrics
  , resourcesMetrics  :: !ResourcesMetrics
  , rtsMetrics        :: !RTSMetrics
  , blockchainMetrics :: !BlockchainMetrics
  , kesMetrics        :: !KESMetrics
  , nodeMetrics       :: !NodeMetrics
  , nodeErrors        :: !ErrorsMetrics
  , metricsLastUpdate :: !Word64
  } deriving (Generic, NFData, Show)

data PeerMetrics = PeerMetrics
  { peersInfo :: ![PeerInfo]
  , peersInfoChanged :: !Bool
  } deriving (Generic, NFData, Show)

data MempoolMetrics = MempoolMetrics
  { mempoolTxsNumber           :: !Integer
  , mempoolTxsNumberChanged    :: !Bool
  , mempoolTxsPercent          :: !Double
  , mempoolTxsPercentChanged   :: !Bool
  , mempoolBytes               :: !Word64
  , mempoolBytesChanged        :: !Bool
  , mempoolBytesPercent        :: !Double
  , mempoolBytesPercentChanged :: !Bool
  , mempoolMaxTxs              :: !Integer
  , mempoolMaxTxsChanged       :: !Bool
  , mempoolMaxBytes            :: !Integer
  , mempoolMaxBytesChanged     :: !Bool
  , txsProcessed               :: !Integer
  , txsProcessedChanged        :: !Bool
  } deriving (Generic, NFData, Show)

data ForgeMetrics = ForgeMetrics
  { nodeIsLeaderNum           :: !Integer
  , nodeIsLeaderNumChanged    :: !Bool
  , slotsMissedNumber         :: !Integer
  , slotsMissedNumberChanged  :: !Bool
  , nodeCannotForge           :: !Integer
  , nodeCannotForgeChanged    :: !Bool
  , blocksForgedNumber        :: !Integer
  , blocksForgedNumberChanged :: !Bool
  } deriving (Generic, NFData, Show)

data ResourcesMetrics = ResourcesMetrics
  { memory              :: !Double
  , cpuPercent          :: !Double
  , cpuLast             :: !Integer
  , cpuNs               :: !Word64
  , diskUsageR          :: !Double
  , diskUsageRLast      :: !Word64
  , diskUsageRNs        :: !Word64
  , diskUsageW          :: !Double
  , diskUsageWLast      :: !Word64
  , diskUsageWNs        :: !Word64
  , networkUsageIn      :: !Double
  , networkUsageInLast  :: !Word64
  , networkUsageInNs    :: !Word64
  , networkUsageOut     :: !Double
  , networkUsageOutLast :: !Word64
  , networkUsageOutNs   :: !Word64
  } deriving (Generic, NFData, Show)

data RTSMetrics = RTSMetrics
  { rtsMemoryUsed        :: !Double
  , rtsGcMajorNum        :: !Integer
  , rtsGcMajorNumChanged :: !Bool
  , rtsGcMinorNum        :: !Integer
  , rtsGcMinorNumChanged :: !Bool
  , rtsGCPercent         :: !Double
  , rtsGCLast            :: !Integer
  , rtsGCNs              :: !Word64
  , rtsMutPercent        :: !Double
  , rtsMutLast           :: !Integer
  , rtsMutNs             :: !Word64
  } deriving (Generic, NFData, Show)

data BlockchainMetrics = BlockchainMetrics
  { systemStartTime          :: !UTCTime
  , systemStartTimeChanged   :: !Bool
  , epoch                    :: !Integer
  , epochChanged             :: !Bool
  , slot                     :: !Integer
  , slotChanged              :: !Bool
  , blocksNumber             :: !Integer
  , blocksNumberChanged      :: !Bool
  , chainDensity             :: !Double
  , chainDensityChanged      :: !Bool
  , slotLengthShelley        :: !Integer
  , slotLengthAllegra        :: !Integer
  , slotLengthMary           :: !Integer
  , slotsPerKESPeriodShelley :: !Integer
  , slotsPerKESPeriodAllegra :: !Integer
  , slotsPerKESPeriodMary    :: !Integer
  } deriving (Generic, NFData, Show)

data KESMetrics = KESMetrics
  { remKESPeriods                :: !Integer
  , remKESPeriodsChanged         :: !Bool
  , remKESPeriodsInDays          :: !Integer
  , remKESPeriodsInDaysChanged   :: !Bool
  , opCertStartKESPeriod         :: !Integer
  , opCertStartKESPeriodChanged  :: !Bool
  , opCertExpiryKESPeriod        :: !Integer
  , opCertExpiryKESPeriodChanged :: !Bool
  , currentKESPeriod             :: !Integer
  , currentKESPeriodChanged      :: !Bool
  } deriving (Generic, NFData, Show)

data NodeMetrics = NodeMetrics
  { nodeProtocol         :: !Text
  , nodeProtocolChanged  :: !Bool
  , nodeVersion          :: !Text
  , nodeVersionChanged   :: !Bool
  , nodeCommit           :: !Text
  , nodeCommitChanged    :: !Bool
  , nodeShortCommit      :: !Text
  , nodePlatform         :: !Text
  , nodePlatformChanged  :: !Bool
  , nodeStartTime        :: !UTCTime
  , nodeStartTimeChanged :: !Bool
  , nodeEndpointChanged  :: !Bool
  } deriving (Generic, NFData, Show)

data ErrorsMetrics = ErrorsMetrics
  { errors        :: ![NodeError]
  , errorsChanged :: !Bool
  , errorsRebuild :: !Bool
  } deriving (Generic, NFData, Show)

data Era
  = Shelley
  | Allegra
  | Mary

initialNodesState
  :: Configuration
  -> IO NodesState
initialNodesState config =
  CM.getAcceptAt config >>= \case
    Just addrs -> do
      now <- getMonotonicTimeNSec
      return $ HM.fromList [(name, initialNodeState now) | (RemoteAddrNamed name _) <- addrs]
    Nothing ->
      -- Actually it's impossible, because at this point we already know
      -- that at least one |TraceAcceptor| is defined in the config.
      return HM.empty

initialNodeState :: Word64 -> NodeState
initialNodeState now = NodeState
  { peersMetrics =
      PeerMetrics
        { peersInfo        = []
        , peersInfoChanged = True
        }
  , mempoolMetrics =
      MempoolMetrics
        { mempoolTxsNumber           = -1
        , mempoolTxsNumberChanged    = False
        , mempoolTxsPercent          = -0.1
        , mempoolTxsPercentChanged   = False
        , mempoolBytes               = 0
        , mempoolBytesChanged        = False
        , mempoolBytesPercent        = -0.1
        , mempoolBytesPercentChanged = False
        , mempoolMaxTxs              = -1
        , mempoolMaxTxsChanged       = False
        , mempoolMaxBytes            = -1
        , mempoolMaxBytesChanged     = False
        , txsProcessed               = -1
        , txsProcessedChanged        = False
        }
  , forgeMetrics =
      ForgeMetrics
        { nodeIsLeaderNum           = -1
        , nodeIsLeaderNumChanged    = False
        , slotsMissedNumber         = -1
        , slotsMissedNumberChanged  = False
        , nodeCannotForge           = -1
        , nodeCannotForgeChanged    = False
        , blocksForgedNumber        = -1
        , blocksForgedNumberChanged = False
        }
  , resourcesMetrics =
      ResourcesMetrics
        { memory                  = 0.1
        , cpuPercent              = 0.1
        , cpuLast                 = 0
        , cpuNs                   = 10000
        , diskUsageR              = 0.1
        , diskUsageRLast          = 0
        , diskUsageRNs            = 10000
        , diskUsageW              = 0.1
        , diskUsageWLast          = 0
        , diskUsageWNs            = 10000
        , networkUsageIn          = 0.1
        , networkUsageInLast      = 0
        , networkUsageInNs        = 10000
        , networkUsageOut         = 0.1
        , networkUsageOutLast     = 0
        , networkUsageOutNs       = 10000
        }
  , rtsMetrics =
      RTSMetrics
        { rtsMemoryUsed        = 0.1
        , rtsGcMajorNum        = -1
        , rtsGcMajorNumChanged = False
        , rtsGcMinorNum        = -1
        , rtsGcMinorNumChanged = False
        , rtsGCPercent         = 0.1
        , rtsGCLast            = 0
        , rtsGCNs              = 10000
        , rtsMutPercent        = 0.1
        , rtsMutLast           = 0
        , rtsMutNs             = 10000
        }
  , blockchainMetrics =
      BlockchainMetrics
        { systemStartTime          = nullTime
        , systemStartTimeChanged   = False
        , epoch                    = -1
        , epochChanged             = False
        , slot                     = -1
        , slotChanged              = False
        , blocksNumber             = -1
        , blocksNumberChanged      = False
        , chainDensity             = -0.1
        , chainDensityChanged      = False
        , slotLengthShelley        = 1
        , slotLengthAllegra        = 1
        , slotLengthMary           = 1
        , slotsPerKESPeriodShelley = 129600
        , slotsPerKESPeriodAllegra = 129600
        , slotsPerKESPeriodMary    = 129600
        }
  , kesMetrics =
      KESMetrics
        { remKESPeriods                = -1
        , remKESPeriodsChanged         = False
        , remKESPeriodsInDays          = -1
        , remKESPeriodsInDaysChanged   = False
        , opCertStartKESPeriod         = -1
        , opCertStartKESPeriodChanged  = False
        , opCertExpiryKESPeriod        = -1
        , opCertExpiryKESPeriodChanged = False
        , currentKESPeriod             = -1
        , currentKESPeriodChanged      = False
        }
  , nodeMetrics =
      NodeMetrics
        { nodeProtocol         = ""
        , nodeProtocolChanged  = False
        , nodeVersion          = ""
        , nodeVersionChanged   = False
        , nodeCommit           = ""
        , nodeCommitChanged    = False
        , nodeShortCommit      = ""
        , nodePlatform         = ""
        , nodePlatformChanged  = False
        , nodeStartTime        = nullTime
        , nodeStartTimeChanged = False
        , nodeEndpointChanged  = True -- update endpoint once, because it's taken from configuration.
        }
  , nodeErrors =
      ErrorsMetrics
        { errors        = []
        , errorsChanged = True
        , errorsRebuild = False
        }
  , metricsLastUpdate = now
  }

nullTime :: UTCTime
nullTime = UTCTime (ModifiedJulianDay 0) 0

none :: String
none = "—"

-- Default values as they will be shown in markup after re-connection.

showText :: Text -> String
showText t = if T.null t then none else T.unpack t

showInitTime :: String
showInitTime = "00:00:00"

showTime :: UTCTime -> String
showTime t = if t == nullTime then none else formatTime defaultTimeLocale "%F %T %Z" t

showInteger :: Integer -> String
showInteger i = if i == (-1) then none else show i

showWord64 :: Word64 -> String
showWord64 i = if i == (maxBound :: Word64) then none else show i

showDouble :: Double -> String
showDouble d = if d < 0.0 then none else showWith1DecPlace d

-- Aux

showWith1DecPlace :: Double -> String
showWith1DecPlace = T.unpack . sformat ("" % fixed 1)

mkTraceAcceptorEndpoint
  :: Text
  -> [RemoteAddrNamed]
  -> String
mkTraceAcceptorEndpoint nameOfNode acceptors =
  case maybeActiveNode of
    Just (RemoteAddrNamed _ (RemoteSocket host port)) -> host <> ":" <> port
    Just (RemoteAddrNamed _ (RemotePipe pipePath))    -> pipePath
    Nothing                                           -> none
 where
  maybeActiveNode = flip L.find acceptors $ \(RemoteAddrNamed name _) -> name == nameOfNode

fullEndpointTitle :: String -> String
fullEndpointTitle endpoint = if shortened == endpoint then "" else endpoint
 where
  len = length endpoint
  shortened = if len > 20
                then take 10 endpoint <> "..." <> drop (len - 10) endpoint
                else endpoint
