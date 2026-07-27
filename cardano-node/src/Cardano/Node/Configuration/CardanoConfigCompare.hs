{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

-- The 'ToExpr'/'Generic' instances below are orphans, used only for diffing here.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Diff the node's POM-resolved 'NodeConfiguration' against the one produced by
-- the @cardano-config@ adapter. The composite fields (the per-era protocol
-- configuration records and the LedgerDB configuration) are diffed structurally
-- with @tree-diff@; scalar fields use a plain @node=… vs cardano-config=…@ line.
module Cardano.Node.Configuration.CardanoConfigCompare
  ( compareConfigurations
  , deprecatedFlagWarnings
  ) where

import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types (NodeProtocolConfiguration (..))

import           Data.List (intercalate)

import           Cardano.Crypto (RequiresNetworkMagic)
import           Cardano.Ledger.BaseTypes.NonZero (NonZero, unNonZero)
import           Cardano.Node.Configuration.LedgerDB (DeprecatedOptions (..),
                   LedgerDbConfiguration (..), LedgerDbSelectorFlag (..))
import           Cardano.Node.Types (CheckpointsFile (..), CheckpointsHash,
                   GenesisFile (..), GenesisHash,
                   MaxConcurrencyBulkSync (..), MaxConcurrencyDeadline (..),
                   NodeAlonzoProtocolConfiguration (..),
                   NodeByronProtocolConfiguration (..),
                   NodeCheckpointsConfiguration (..),
                   NodeConwayProtocolConfiguration (..),
                   NodeDijkstraProtocolConfiguration (..),
                   NodeHardForkProtocolConfiguration (..),
                   NodeShelleyProtocolConfiguration (..))
import           Cardano.Slotting.Slot (EpochNo, SlotNo (..))
import           Data.Time.Clock (DiffTime, secondsToDiffTime)
import           Data.TreeDiff (Expr (App, Rec), ToExpr (..), ediff, prettyEditExpr)
import qualified Data.TreeDiff.OMap as OMap
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Mempool (MempoolCapacityBytesOverride (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Args (QueryBatchSize (..),
                   defaultQueryBatchSize)
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
                   (NumOfDiskSnapshots (..), SnapshotDelayRange (..),
                   SnapshotFrequency (..), SnapshotFrequencyArgs (..),
                   SnapshotPolicyArgs (..))
import           Ouroboros.Consensus.Util.Args (OverrideOrDefault (..))

-- | Guidance for deprecated node CLI flags that cardano-config's parser rejects:
-- the legacy aliases have a new spelling, and the mempool flags were removed
-- (mempool capacity is a config-file setting now). Pure, so it is unit-testable.
deprecatedFlagWarnings :: [String] -> [String]
deprecatedFlagWarnings = concatMap diagnose
  where
    -- Deprecated alias -> new (cardano-config-accepted) spelling.
    renamed =
      [ ("--delegation-certificate", "--byron-delegation-certificate")
      , ("--signing-key",            "--byron-signing-key")
      , ("--non-producing-node",     "--start-as-non-producing-node")
      ]
    removed = ["--mempool-capacity-override", "--no-mempool-capacity-override"]

    diagnose tok =
      -- Accept both @--flag value@ and @--flag=value@ spellings.
      let opt = takeWhile (/= '=') tok
      in case lookup opt renamed of
           Just new ->
             [ "warning: deprecated CLI flag '" <> opt <> "'; use '" <> new
                 <> "' (required for cardano-config parsing / the upcoming config parser)" ]
           Nothing
             | opt `elem` removed ->
                 [ "warning: '" <> opt <> "' is deprecated and no longer supported; remove it"
                     <> " and set 'MempoolCapacityBytesOverride' in the configuration file instead" ]
             | otherwise -> []

-- | Compare a POM-resolved configuration against the adapter-produced one, field
-- by field. Returns one entry per diverging field; empty means they agree.
compareConfigurations :: NodeConfiguration -> NodeConfiguration -> [String]
compareConfigurations pom adapted =
    concat
      [ compareProtocol (ncProtocolConfig pom) (ncProtocolConfig adapted)
      , cmp "ValidateDB" ncValidateDB
      , cmp "TopologyFile" ncTopologyFile
      , cmp "DatabaseFile" ncDatabaseFile
      , cmp "StartAsNonProducingNode" ncStartAsNonProducingNode
      , cmp "ProtocolFiles" ncProtocolFiles
      , cmp "ShutdownConfig" ncShutdownConfig
      , cmp "SocketConfig" ncSocketConfig
      , cmp "DiffusionMode" ncDiffusionMode
      , cmp "ExperimentalProtocolsEnabled" ncExperimentalProtocolsEnabled
      , cmp "MaxConcurrencyBulkSync" (normalizeMaxConcurrencyBulkSync . ncMaxConcurrencyBulkSync)
      , cmp "MaxConcurrencyDeadline" (normalizeMaxConcurrencyDeadline . ncMaxConcurrencyDeadline)
      , cmp "TraceForwardSocket" ncTraceForwardSocket
      , cmp "MaybeMempoolCapacityOverride" (normalizeMempoolOverride . ncMaybeMempoolCapacityOverride)
      , cmpTree "LedgerDbConfig" (normalizeLedgerDb . ncLedgerDbConfig)
      , cmp "ProtocolIdleTimeout" ncProtocolIdleTimeout
      , cmp "TimeWaitTimeout" ncTimeWaitTimeout
      , cmp "EgressPollInterval" ncEgressPollInterval
      , cmp "ChainSyncIdleTimeout" ncChainSyncIdleTimeout
      , cmp "MempoolTimeoutSoft" ncMempoolTimeoutSoft
      , cmp "MempoolTimeoutHard" ncMempoolTimeoutHard
      , cmp "MempoolTimeoutCapacity" ncMempoolTimeoutCapacity
      , cmp "AcceptedConnectionsLimit" ncAcceptedConnectionsLimit
      , cmp "DeadlineTargetOfRootPeers" ncDeadlineTargetOfRootPeers
      , cmp "DeadlineTargetOfKnownPeers" ncDeadlineTargetOfKnownPeers
      , cmp "DeadlineTargetOfEstablishedPeers" ncDeadlineTargetOfEstablishedPeers
      , cmp "DeadlineTargetOfActivePeers" ncDeadlineTargetOfActivePeers
      , cmp "DeadlineTargetOfKnownBigLedgerPeers" ncDeadlineTargetOfKnownBigLedgerPeers
      , cmp "DeadlineTargetOfEstablishedBigLedgerPeers" ncDeadlineTargetOfEstablishedBigLedgerPeers
      , cmp "DeadlineTargetOfActiveBigLedgerPeers" ncDeadlineTargetOfActiveBigLedgerPeers
      , cmp "SyncTargetOfRootPeers" ncSyncTargetOfRootPeers
      , cmp "SyncTargetOfKnownPeers" ncSyncTargetOfKnownPeers
      , cmp "SyncTargetOfEstablishedPeers" ncSyncTargetOfEstablishedPeers
      , cmp "SyncTargetOfActivePeers" ncSyncTargetOfActivePeers
      , cmp "SyncTargetOfKnownBigLedgerPeers" ncSyncTargetOfKnownBigLedgerPeers
      , cmp "SyncTargetOfEstablishedBigLedgerPeers" ncSyncTargetOfEstablishedBigLedgerPeers
      , cmp "SyncTargetOfActiveBigLedgerPeers" ncSyncTargetOfActiveBigLedgerPeers
      , cmp "ConsensusMode" ncConsensusMode
      , cmp "MinBigLedgerPeersForTrustedState" ncMinBigLedgerPeersForTrustedState
      , cmp "PeerSharing" ncPeerSharing
      , cmp "GenesisConfig" ncGenesisConfig
      , cmp "ResponderCoreAffinityPolicy" ncResponderCoreAffinityPolicy
      , cmp "RpcConfig" ncRpcConfig
      , cmp "TxSubmissionLogicVersion" ncTxSubmissionLogicVersion
      , cmp "TxSubmissionInitDelay" ncTxSubmissionInitDelay
      ]
  where
    cmp :: (Eq a, Show a) => String -> (NodeConfiguration -> a) -> [String]
    cmp label accessor = cmpValues label (accessor pom) (accessor adapted)

    cmpTree :: (Eq a, ToExpr a) => String -> (NodeConfiguration -> a) -> [String]
    cmpTree label accessor = cmpValuesTree label (accessor pom) (accessor adapted)

-- | Report a divergence between two scalar values.
cmpValues :: (Eq a, Show a) => String -> a -> a -> [String]
cmpValues label a b
  | a == b = []
  | otherwise = [label <> ": node=" <> show a <> " vs cardano-config=" <> show b]

-- | Report a divergence between two composite values as a @tree-diff@ structural
-- diff (@-@ is the node value, @+@ the cardano-config one), as one indented entry.
cmpValuesTree :: (Eq a, ToExpr a) => String -> a -> a -> [String]
cmpValuesTree label a b
  | a == b = []
  | otherwise =
      [ label <> ":\n"
          <> intercalate "\n"
               (map ("      " <>) (lines (show (prettyEditExpr (ediff a b))))) ]

-- | Compare the Cardano protocol configuration per era/component.
compareProtocol :: NodeProtocolConfiguration -> NodeProtocolConfiguration -> [String]
compareProtocol
  (NodeProtocolConfigurationCardano b1 s1 a1 c1 d1 h1 k1)
  (NodeProtocolConfigurationCardano b2 s2 a2 c2 d2 h2 k2) =
    concat
      [ cmpValuesTree "Byron protocol config" (normalizeByron b1) (normalizeByron b2)
      , cmpValuesTree "Shelley protocol config" s1 s2
      , cmpValuesTree "Alonzo protocol config" a1 a2
      , cmpValuesTree "Conway protocol config" c1 c2
      , cmpValuesTree "Dijkstra protocol config" d1 d2
      , cmpValuesTree "HardFork protocol config" h1 h2
      , cmpValuesTree "Checkpoints protocol config" k1 k2
      ]

-- ---------------------------------------------------------------------------
-- Normalization: hide representational-only divergences, where POM keeps a
-- "use the default" sentinel and cardano-config spells the default out.
-- ---------------------------------------------------------------------------

-- | Normalize a 'LedgerDbConfiguration' before diffing (snapshot policy and
-- query batch size).
normalizeLedgerDb :: LedgerDbConfiguration -> LedgerDbConfiguration
normalizeLedgerDb (LedgerDbConfiguration spa qbs sel dep) =
  LedgerDbConfiguration (normalizeSnapshotPolicy spa) (normalizeQueryBatchSize qbs) sel dep

-- | 'DefaultQueryBatchSize' and an explicit request for the same size are
-- equivalent; collapse the latter.
normalizeQueryBatchSize :: QueryBatchSize -> QueryBatchSize
normalizeQueryBatchSize q
  | defaultQueryBatchSize q == defaultQueryBatchSize DefaultQueryBatchSize = DefaultQueryBatchSize
  | otherwise = q

-- | Unset ('Nothing') resolves to the 'defaultBlockFetchConfiguration' value of
-- 1, which cardano-config spells out; treat @Just 1@ as 'Nothing'.
normalizeMaxConcurrencyBulkSync :: Maybe MaxConcurrencyBulkSync -> Maybe MaxConcurrencyBulkSync
normalizeMaxConcurrencyBulkSync (Just (MaxConcurrencyBulkSync 1)) = Nothing
normalizeMaxConcurrencyBulkSync x                                 = x

normalizeMaxConcurrencyDeadline :: Maybe MaxConcurrencyDeadline -> Maybe MaxConcurrencyDeadline
normalizeMaxConcurrencyDeadline (Just (MaxConcurrencyDeadline 1)) = Nothing
normalizeMaxConcurrencyDeadline x                                 = x

-- | @Just NoMempoolCapacityBytesOverride@ and 'Nothing' both mean "no override".
normalizeMempoolOverride :: Maybe MempoolCapacityBytesOverride -> Maybe MempoolCapacityBytesOverride
normalizeMempoolOverride (Just NoMempoolCapacityBytesOverride) = Nothing
normalizeMempoolOverride x                                     = x

-- | The Byron supported-protocol version is not modelled by cardano-config and
-- has no effect for a genesis-only Byron era; blank it on both sides.
normalizeByron :: NodeByronProtocolConfiguration -> NodeByronProtocolConfiguration
normalizeByron c =
  c { npcByronSupportedProtocolVersionMajor = 0
    , npcByronSupportedProtocolVersionMinor = 0
    , npcByronSupportedProtocolVersionAlt   = 0
    }

-- | Collapse the snapshot overrides that merely restate the consensus default
-- (rate limit, delay range, count). The offset and interval genuinely differ and
-- stay flagged; the interval default is security-parameter dependent (@2*k@) and
-- cannot be reconstructed here. Constants mirror 'defaultSnapshotPolicy'.
normalizeSnapshotPolicy :: SnapshotPolicyArgs -> SnapshotPolicyArgs
normalizeSnapshotPolicy (SnapshotPolicyArgs freq num) =
    SnapshotPolicyArgs (normalizeFrequency freq) (collapse defaultNumSnapshots num)
  where
    defaultNumSnapshots = NumOfDiskSnapshots 2
    defaultOffset       = SlotNo 0
    defaultRateLimit    = secondsToDiffTime (10 * 60)
    defaultDelayRange   = SnapshotDelayRange (secondsToDiffTime 300) (secondsToDiffTime 600)

    normalizeFrequency DisableSnapshots = DisableSnapshots
    normalizeFrequency (SnapshotFrequency (SnapshotFrequencyArgs interval offset rateLimit delayRange)) =
      SnapshotFrequency $ SnapshotFrequencyArgs
        interval
        (collapse defaultOffset offset)
        (collapse defaultRateLimit rateLimit)
        (collapse defaultDelayRange delayRange)

    collapse :: Eq a => a -> OverrideOrDefault a -> OverrideOrDefault a
    collapse def (Override v) | v == def = UseDefault
    collapse _   x = x

-- ---------------------------------------------------------------------------
-- tree-diff instances for the composite records. Node-local records derive
-- 'Generic' here; opaque leaves (hashes, 'DiffTime') render via 'Show'; the
-- consensus snapshot-policy types lack 'Generic' and get explicit instances.
-- ---------------------------------------------------------------------------

deriving instance Generic GenesisFile
deriving instance Generic CheckpointsFile
deriving instance Generic NodeByronProtocolConfiguration
deriving instance Generic NodeShelleyProtocolConfiguration
deriving instance Generic NodeAlonzoProtocolConfiguration
deriving instance Generic NodeConwayProtocolConfiguration
deriving instance Generic NodeDijkstraProtocolConfiguration
deriving instance Generic NodeHardForkProtocolConfiguration
deriving instance Generic NodeCheckpointsConfiguration
deriving instance Generic LedgerDbConfiguration
deriving instance Generic LedgerDbSelectorFlag
deriving instance Generic DeprecatedOptions

instance ToExpr GenesisFile
instance ToExpr CheckpointsFile
instance ToExpr NodeByronProtocolConfiguration
instance ToExpr NodeShelleyProtocolConfiguration
instance ToExpr NodeAlonzoProtocolConfiguration
instance ToExpr NodeConwayProtocolConfiguration
instance ToExpr NodeDijkstraProtocolConfiguration
instance ToExpr NodeHardForkProtocolConfiguration
instance ToExpr NodeCheckpointsConfiguration
instance ToExpr LedgerDbConfiguration
instance ToExpr LedgerDbSelectorFlag
instance ToExpr DeprecatedOptions

-- External types that already derive 'Generic'.
instance ToExpr RequiresNetworkMagic
instance ToExpr QueryBatchSize

-- Opaque leaves rendered through 'Show'.
instance ToExpr GenesisHash where toExpr = exprViaShow
instance ToExpr CheckpointsHash where toExpr = exprViaShow
instance ToExpr EpochNo where toExpr = exprViaShow
instance ToExpr DiffTime where toExpr = exprViaShow

-- The consensus snapshot-policy types (no usable 'Generic').
instance ToExpr SnapshotPolicyArgs where
  toExpr (SnapshotPolicyArgs freq num) =
    Rec "SnapshotPolicyArgs" $ OMap.fromList
      [ ("spaFrequency", toExpr freq)
      , ("spaNum", toExpr num)
      ]

instance ToExpr SnapshotFrequency where
  toExpr (SnapshotFrequency args) = App "SnapshotFrequency" [toExpr args]
  toExpr DisableSnapshots         = App "DisableSnapshots" []

instance ToExpr SnapshotFrequencyArgs where
  toExpr (SnapshotFrequencyArgs interval offset rateLimit delayRange) =
    Rec "SnapshotFrequencyArgs" $ OMap.fromList
      [ ("sfaInterval", toExpr interval)
      , ("sfaOffset", toExpr offset)
      , ("sfaRateLimit", toExpr rateLimit)
      , ("sfaDelaySnapshotRange", toExpr delayRange)
      ]

instance ToExpr a => ToExpr (OverrideOrDefault a) where
  toExpr (Override a) = App "Override" [toExpr a]
  toExpr UseDefault   = App "UseDefault" []

-- 'NonZero' hides its constructor, so unwrap via 'unNonZero'.
instance ToExpr a => ToExpr (NonZero a) where
  toExpr n = App "NonZero" [toExpr (unNonZero n)]

instance ToExpr SlotNo where
  toExpr (SlotNo w) = App "SlotNo" [toExpr w]

instance ToExpr SnapshotDelayRange
instance ToExpr NumOfDiskSnapshots

-- | Render a value as an opaque @tree-diff@ leaf via its 'Show' instance.
exprViaShow :: Show a => a -> Expr
exprViaShow x = App (show x) []
