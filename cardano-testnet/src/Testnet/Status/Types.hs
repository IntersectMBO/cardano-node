{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Testnet.Status.Types
  ( CheckStatusOptions (..)
  , NetworkStatus(..)
  , NodeProbeResult(..)
  , NodeState(..)
  , OutputFormat(..)
  , StatusReport(..)
  , TipInfo(..)
  ) where

import           Cardano.Api (BlockHeader, BlockNo (..), Hash, SlotNo (..), ToJSON (..))

import           Cardano.Prelude (ExitCode (..), Nat)

import           Prelude

import           Data.Aeson (Value, object, (.=))
import           Data.Default.Class (Default (..))
import           Data.Time (NominalDiffTime, UTCTime)
import           System.Process (Pid)

import           Testnet.Manifest (ManifestNodeRole)

-- Option types

-- | Parameters for the `status` subcommand
data CheckStatusOptions = CheckStatusOptions
  { testnetDir :: Maybe FilePath -- ^ the directory with the testnet environment, by default "./testnet"
  , statusCheckTimeout :: Nat -- ^ the timeout for waiting for each node to respond to the status check in seconds
  , outputFormat :: OutputFormat -- ^ the format in which to output the status information
  }

-- | Output format for the `status` subcommand
data OutputFormat = OutputFormatText | OutputFormatJson

instance Default OutputFormat where
  def :: OutputFormat
  def = OutputFormatText

-- Output types

-- | Version of the JSON output, pinned by
-- @cardano-testnet/schemas/status.schema.json@.
statusSchemaVersion :: Int
statusSchemaVersion = 1

-- | The state of the whole network.
data NetworkStatus
  = NetworkRunning  -- ^ every node is 'NodeOk'
  | NetworkDegraded -- ^ some nodes are 'NodeOk', but not all
  | NetworkStalled  -- ^ some nodes answer, but none is 'NodeOk'
  | NetworkStopped  -- ^ no node answers
  | NoTestnet       -- ^ there is no manifest file in the 'testnetDir'
  deriving (Eq, Show)

instance ToJSON NetworkStatus where
  toJSON :: NetworkStatus -> Value
  toJSON NetworkRunning  = "running"
  toJSON NetworkDegraded = "degraded"
  toJSON NetworkStalled  = "stalled"
  toJSON NetworkStopped  = "stopped"
  toJSON NoTestnet       = "no-testnet"

-- | The state of one node.
data NodeState
  = NodeOk          -- ^ it answers, and its tip is fresh
  | NodeStalled     -- ^ it answers, but its tip is too old, or it has no blocks yet
  | NodeUnreachable -- ^ it does not answer, but process may be alive
  | NodeDown        -- ^ it does not answer and the process is gone
  deriving (Eq, Show)

instance ToJSON NodeState where
  toJSON :: NodeState -> Value
  toJSON NodeOk          = "ok"
  toJSON NodeStalled     = "stalled"
  toJSON NodeUnreachable = "unreachable"
  toJSON NodeDown        = "down"

-- | A node's chain tip.
data TipInfo = TipInfo
  { tipSlot    :: !SlotNo
  , tipBlockNo :: !BlockNo
  , tipHash    :: Hash BlockHeader -- ^ hex
  , tipAge     :: !NominalDiffTime -- ^ time since the start of the tip's slot
  } deriving (Eq, Show)

instance ToJSON TipInfo where
  toJSON :: TipInfo -> Value
  toJSON t = object
    [ "slot"       .= unSlotNo (tipSlot t)
    , "blockNo"    .= unBlockNo (tipBlockNo t)
    , "hash"       .= tipHash t
    , "ageSeconds" .= tipAge t
    ]

-- | What 'checkStatus' found out about one node.
data NodeProbeResult = NodeProbeResult
  { probeNodeName    :: !String
  , probeNodeRole    :: !ManifestNodeRole
  , probePid         :: !(Maybe Pid)
  , probePidIsAlive  :: !(Maybe Bool)     -- ^ 'Nothing' when unknown: no pid, or Windows
  , probeNodeState   :: !NodeState
  , probeNodeTipInfo :: !(Maybe TipInfo)  -- ^ 'Just' when the node answered with a tip
  } deriving (Eq, Show)

instance ToJSON NodeProbeResult where
  toJSON :: NodeProbeResult -> Value
  toJSON n = object
    [ "name"     .= probeNodeName n
    , "role"     .= probeNodeRole n
    , "pid"      .= (fromIntegral <$> probePid n :: Maybe Int)
    , "pidAlive" .= probePidIsAlive n
    , "state"    .= probeNodeState n
    , "tip"      .= probeNodeTipInfo n
    ]

data StatusReport = StatusReport
  { reportOutputDir          :: !FilePath
  , reportCheckedAt          :: !UTCTime
  , reportStatus             :: !NetworkStatus
  , reportBestTip            :: !(Maybe TipInfo)         -- ^ the most tip of all nodes
  , reportNodes              :: ![NodeProbeResult]       -- ^ empty for 'NoTestnet'
  , reportProblemExplanation :: !(Maybe String)          -- ^ why there is no testnet
  } deriving (Eq, Show)

instance ToJSON StatusReport where
  toJSON :: StatusReport -> Value
  toJSON r = object
    [ "schemaVersion"        .= statusSchemaVersion
    , "checkedAt"            .= reportCheckedAt r
    , "status"               .= reportStatus r
    , "exitCode"             .= exitCodeNumber (exitCodeForStatus (reportStatus r))
    , "chainProducingBlocks" .= (exitCodeForStatus (reportStatus r) == ExitSuccess)
    , "bestTip"              .= reportBestTip r
    , "nodes"                .= reportNodes r
    ]
    where
      -- Extract code number from 'ExitCode'
      exitCodeNumber :: ExitCode -> Int
      exitCodeNumber ExitSuccess = 0
      exitCodeNumber (ExitFailure n) = n

      -- | Convert the network state to an exit code
      exitCodeForStatus :: NetworkStatus -> ExitCode
      exitCodeForStatus NetworkRunning = ExitSuccess
      exitCodeForStatus NetworkDegraded = ExitSuccess
      exitCodeForStatus NetworkStalled = ExitFailure 3
      exitCodeForStatus NetworkStopped = ExitFailure 4
      exitCodeForStatus NoTestnet = ExitFailure 5
