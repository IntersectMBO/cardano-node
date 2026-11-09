{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Testnet.Status.Types
  ( ChainInfo (..)
  , CheckStatusOptions (..)
  , MempoolInfo (..)
  , NetworkStatus(..)
  , NoTestnetReason(..)
  , NodeAnswer(..)
  , NodeProbeResult(..)
  , NodeState(..)
  , OutputFormat(..)
  , StatusReport(..)
  , TipInfo(..)
  , exitCodeForStatus
  ) where

import           Cardano.Api (AnyCardanoEra, BlockHeader, BlockNo (..), EpochNo (..), Hash,
                   SlotNo (..), ToJSON (..))

import           Cardano.Prelude (ExitCode (..), Nat)

import           Prelude

import           Data.Aeson (Value, object, (.=))
import           Data.Default.Class (Default (..))
import           Data.Time (NominalDiffTime, UTCTime)
import           Data.Word (Word32, Word64)
import           System.Process (Pid)

import           Testnet.Manifest (ManifestNodeRole)
import           Testnet.Start.Types (anyEraToString)

-- Option types

-- | Parameters for the `status` subcommand
data CheckStatusOptions = CheckStatusOptions
  { testnetDir :: Maybe FilePath -- ^ the directory with the testnet environment, by default "./testnet"
  , statusCheckTimeout :: Nat -- ^ the timeout for waiting for each answer from a node, in seconds
  , outputFormat :: OutputFormat -- ^ the format in which to output the status information
  }

-- | Output format for the `status` subcommand
data OutputFormat = OutputFormatText | OutputFormatJson

instance Default OutputFormat where
  def :: OutputFormat
  def = OutputFormatText

-- Output types

-- | Version of the JSON output, pinned by
-- @cardano-testnet/schemas/status.schema.json@. Any change to the output
-- shape, including new fields, bumps it.
statusSchemaVersion :: Int
statusSchemaVersion = 2

-- | The state of the whole network.
data NetworkStatus
  = NetworkRunning  -- ^ every node is 'NodeOk'
  | NetworkDegraded -- ^ some nodes are 'NodeOk', but not all
  | NetworkStalled  -- ^ some nodes answer, but none is 'NodeOk'
  | NetworkStopped  -- ^ no node answers
  | NoTestnet       -- ^ there is no manifest file in the 'testnetDir': there is no
                    -- testnet, or it is still starting (the manifest is written
                    -- once the nodes are ready)
  deriving (Eq, Show)

instance ToJSON NetworkStatus where
  toJSON :: NetworkStatus -> Value
  toJSON NetworkRunning  = "running"
  toJSON NetworkDegraded = "degraded"
  toJSON NetworkStalled  = "stalled"
  toJSON NetworkStopped  = "stopped"
  toJSON NoTestnet       = "no-testnet"

-- | Why there is no manifest file in the 'testnetDir'.
data NoTestnetReason
  = DirMissing     -- ^ the directory does not exist
  | NotReady       -- ^ it has genesis files: the testnet is still starting, or it never became ready
  | NotATestnetDir -- ^ it is not the output directory of a testnet
  deriving (Eq, Show)

instance ToJSON NoTestnetReason where
  toJSON :: NoTestnetReason -> Value
  toJSON DirMissing     = "dir-missing"
  toJSON NotReady       = "not-ready"
  toJSON NotATestnetDir = "not-a-testnet-dir"

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

-- | What a node said when it was asked for its chain tip.
data NodeAnswer
  = AnsweredTip !TipInfo -- ^ it has blocks; this is the newest
  | AnsweredNoBlocks     -- ^ it answered, but it has no blocks yet
  | NoAnswer !String     -- ^ it refused, failed, or did not answer in time: why
  deriving (Eq, Show)

-- | A node's chain tip.
data TipInfo = TipInfo
  { tipSlot    :: !SlotNo
  , tipBlockNo :: !BlockNo
  , tipHash    :: !(Hash BlockHeader) -- ^ hex
  , tipAge     :: !NominalDiffTime    -- ^ time since the start of the tip's slot
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
  , probeError       :: !(Maybe String)   -- ^ 'Just' why, when the node did not answer
  , probeStdoutFile  :: !FilePath         -- ^ its stdout log, relative to the testnet directory
  , probeStderrFile  :: !FilePath         -- ^ its stderr log, relative to the testnet directory
  , probeBlocksBehind :: !(Maybe Word64)  -- ^ how many blocks it is behind the node with the
                                          -- most blocks; 'Nothing' when it has no tip
  , probeMempool     :: !(Maybe MempoolInfo) -- ^ 'Nothing' when it did not answer
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
    , "error"    .= probeError n
    , "stdoutFile" .= probeStdoutFile n
    , "stderrFile" .= probeStderrFile n
    , "blocksBehind" .= probeBlocksBehind n
    , "mempool"  .= probeMempool n
    ]

-- | What is in a node's mempool.
data MempoolInfo = MempoolInfo
  { mempoolTxs           :: !Word32 -- ^ how many transactions
  , mempoolBytes         :: !Word32 -- ^ their size, in bytes
  , mempoolCapacityBytes :: !Word32 -- ^ how many bytes the mempool can hold
  } deriving (Eq, Show)

instance ToJSON MempoolInfo where
  toJSON :: MempoolInfo -> Value
  toJSON m = object
    [ "txs"           .= mempoolTxs m
    , "bytes"         .= mempoolBytes m
    , "capacityBytes" .= mempoolCapacityBytes m
    ]

-- | Where the chain is, as the node with the best tip sees it.
data ChainInfo = ChainInfo
  { chainEra             :: !AnyCardanoEra -- ^ the era the node is in
  , chainEpoch           :: !EpochNo       -- ^ the epoch of the best tip
  , chainSlotInEpoch     :: !Word64        -- ^ the slot of the best tip in its epoch, from 0
  , chainSlotsToEpochEnd :: !Word64        -- ^ slots until the end of that epoch
  } deriving (Eq, Show)

instance ToJSON ChainInfo where
  toJSON :: ChainInfo -> Value
  toJSON c = object
    [ "era"             .= anyEraToString (chainEra c)
    , "epoch"           .= unEpochNo (chainEpoch c)
    , "slotInEpoch"     .= chainSlotInEpoch c
    , "slotsToEpochEnd" .= chainSlotsToEpochEnd c
    ]

-- | Output of status command
data StatusReport = StatusReport
  { reportOutputDir          :: !FilePath
  , reportCheckedAt          :: !UTCTime
  , reportStatus             :: !NetworkStatus
  , reportBestTip            :: !(Maybe TipInfo)         -- ^ the newest tip of all nodes: the one with the highest slot
  , reportNodes              :: ![NodeProbeResult]       -- ^ empty for 'NoTestnet'
  , reportProblemExplanation :: !(Maybe NoTestnetReason) -- ^ why there is no testnet
  , reportChain              :: !(Maybe ChainInfo)       -- ^ 'Nothing' without a best tip, or when
                                                         -- its node does not tell
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
    , "noTestnetReason"      .= reportProblemExplanation r
    , "chain"                .= reportChain r
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
