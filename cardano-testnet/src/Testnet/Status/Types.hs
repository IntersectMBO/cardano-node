{-# LANGUAGE InstanceSigs #-}
module Testnet.Status.Types
  ( CheckStatusOptions (..)
  , OutputFormat(..)
  ) where

import           Cardano.Api (BlockHeader, BlockNo, Hash, SlotNo)

import           Cardano.Prelude (Nat)

import           Prelude

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

-- | The state of the whole network.
data NetworkStatus
  = NetworkRunning  -- ^ every node is 'NodeOk'
  | NetworkDegraded -- ^ some nodes are 'NodeOk', but not all
  | NetworkStalled  -- ^ some nodes answer, but none is 'NodeOk'
  | NetworkStopped  -- ^ no node answers
  | NoTestnet       -- ^ there is no manifest file in the 'testnetDir'
  deriving (Eq, Show)

-- | The state of one node.
data NodeState
  = NodeOk          -- ^ it answers, and its tip is fresh
  | NodeStalled     -- ^ it answers, but its tip is too old, or it has no blocks yet
  | NodeUnreachable -- ^ it does not answer, but process may be alive
  | NodeDown        -- ^ it does not answer and the process is gone
  deriving (Eq, Show)

-- | A node's chain tip.
data TipInfo = TipInfo
  { tipSlot    :: !SlotNo
  , tipBlockNo :: !BlockNo
  , tipHash    :: Hash BlockHeader -- ^ hex
  , tipAge     :: !NominalDiffTime -- ^ time since the start of the tip's slot
  } deriving (Eq, Show)

-- | What 'checkStatus' found out about one node.
data NodeProbeResult = NodeProbeResult
  { probeNodeName    :: !String
  , probeNodeRole    :: !ManifestNodeRole
  , probePid         :: !(Maybe Pid)
  , probePidIsAlive  :: !(Maybe Bool)     -- ^ 'Nothing' when unknown: no pid, or Windows
  , probeNodeState   :: !NodeState
  , probeNodeTipInfo :: !(Maybe TipInfo)  -- ^ 'Just' when the node answered with a tip
  } deriving (Eq, Show)

data StatusReport = StatusReport
  { reportOutputDir          :: !FilePath
  , reportCheckedAt          :: !UTCTime
  , reportStatus             :: !NetworkStatus
  , reportBestTip            :: !(Maybe TipInfo)         -- ^ the most tip of all nodes
  , reportNodes              :: ![NodeProbeResult]       -- ^ empty for 'NoTestnet'
  , reportProblemExplanation :: !(Maybe String)          -- ^ why there is no testnet
  } deriving (Eq, Show)
