-- | Diff the node's POM-resolved 'NodeConfiguration' against the one produced by
-- the @cardano-config@ adapter ('Cardano.Node.Configuration.CardanoConfigAdapter').
--
-- Both sides are the node's own 'NodeConfiguration', so every field is compared
-- in the node's own representation — a reported divergence reflects a genuine
-- difference in the resolved value, with no representation mismatch and no
-- deferred set. A field the adapter cannot yet populate from cardano-config keeps
-- the node default and therefore surfaces here as a divergence (see
-- 'Cardano.Node.Configuration.CardanoConfigAdapter.adapterGaps').
module Cardano.Node.Configuration.CardanoConfigCompare
  ( compareConfigurations
  , deprecatedFlagWarnings
  ) where

import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Handlers.Shutdown (ShutdownConfig (..),
                   ShutdownOn (..))
import           Cardano.Node.Types (MaxConcurrencyBulkSync (..),
                   MaxConcurrencyDeadline (..), NodeProtocolConfiguration (..))

-- | Diagnose node CLI flags that cardano-config's own CLI parser rejects, in
-- terms the operator can act on. All of these flags are deprecated: the three
-- legacy aliases have a new spelling cardano-config accepts, and the two mempool
-- flags have been removed by design (mempool capacity is a config-file setting
-- now). Given the node's argv flag list, returns one guidance line per offending
-- flag found. A pure function so the dual-parse warning path is unit-testable.
deprecatedFlagWarnings :: [String] -> [String]
deprecatedFlagWarnings = concatMap diagnose
  where
    -- Deprecated alias -> new (cardano-config-accepted) spelling.
    renamed =
      [ ("--delegation-certificate", "--byron-delegation-certificate")
      , ("--signing-key",            "--byron-signing-key")
      , ("--non-producing-node",     "--start-as-non-producing-node")
      ]
    -- Deprecated and removed by design; not something cardano-config should grow.
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

-- | Compare a POM-resolved configuration (first argument) against the
-- adapter-produced one (second argument), field by field. Returns one line per
-- diverging field; an empty list means they agree on everything compared.
compareConfigurations :: NodeConfiguration -> NodeConfiguration -> [String]
compareConfigurations pom adapted =
    concat
      [ -- Protocol config, compared per era/component for readable diffs.
        compareProtocol (ncProtocolConfig pom) (ncProtocolConfig adapted)
      , cmp "ValidateDB" ncValidateDB
      , cmp "TopologyFile" ncTopologyFile
      , cmp "DatabaseFile" ncDatabaseFile
      , cmp "StartAsNonProducingNode" ncStartAsNonProducingNode
      , cmp "ProtocolFiles" ncProtocolFiles
      , cmp "ShutdownConfig" (normaliseShutdown . ncShutdownConfig)
      , cmp "SocketConfig" ncSocketConfig
      , cmp "DiffusionMode" ncDiffusionMode
      , cmp "ExperimentalProtocolsEnabled" ncExperimentalProtocolsEnabled
      , cmp "MaxConcurrencyBulkSync"
          (dropConsensusDefault (MaxConcurrencyBulkSync 1) . ncMaxConcurrencyBulkSync)
      , cmp "MaxConcurrencyDeadline"
          (dropConsensusDefault (MaxConcurrencyDeadline 1) . ncMaxConcurrencyDeadline)
      , cmp "TraceForwardSocket" ncTraceForwardSocket
      , cmp "MaybeMempoolCapacityOverride" ncMaybeMempoolCapacityOverride
      , cmp "LedgerDbConfig" ncLedgerDbConfig
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

-- | 'Nothing' and @'Just' 'NoShutdown'@ both mean do not shut down: the use site
-- guards on @'Just' lim | lim '/=' 'NoShutdown'@. Which one the node's own parser
-- produces depends on the entry point, as its option parser ends in
-- @'pure' 'NoShutdown'@, so @run@ yields the latter and @resolve@ the former.
-- Collapse them so that spelling is not reported as a divergence.
normaliseShutdown :: ShutdownConfig -> ShutdownConfig
normaliseShutdown sc =
  sc {scOnSyncLimit = if scOnSyncLimit sc == Just NoShutdown then Nothing else scOnSyncLimit sc}

-- | Collapse a value that merely restates the consensus default to 'Nothing',
-- the node's way of saying "leave the default alone".
--
-- The node's own parser leaves @MaxConcurrency{BulkSync,Deadline}@ unset, so
-- 'Nothing' reaches consensus and @defaultBlockFetchConfiguration@ stands.
-- cardano-config carries an explicit @1@ for both in its @defaults@, which the
-- adapter faithfully reports, so the two differ only in who supplies the number.
--
-- NOTE: this assumes @defaultBlockFetchConfiguration@ uses 1 for both. If
-- consensus ever changes either default, this stops being a spelling difference
-- and becomes a real one that the enveloped path would silently pin to the old
-- value; the normalisation would then hide it. The durable fix is for
-- cardano-config to defer to consensus rather than restate the number.
dropConsensusDefault :: Eq a => a -> Maybe a -> Maybe a
dropConsensusDefault dflt v = if v == Just dflt then Nothing else v

-- | Report a divergence between two values of the same type.
cmpValues :: (Eq a, Show a) => String -> a -> a -> [String]
cmpValues label a b
  | a == b = []
  | otherwise = [label <> ": node=" <> show a <> " vs cardano-config=" <> show b]

-- | Compare the Cardano protocol configuration per component (each era's genesis
-- settings, the hard-fork triggers and the checkpoints) for readable diffs.
compareProtocol :: NodeProtocolConfiguration -> NodeProtocolConfiguration -> [String]
compareProtocol
  (NodeProtocolConfigurationCardano b1 s1 a1 c1 d1 h1 k1)
  (NodeProtocolConfigurationCardano b2 s2 a2 c2 d2 h2 k2) =
    concat
      [ cmpValues "Byron protocol config" b1 b2
      , cmpValues "Shelley protocol config" s1 s2
      , cmpValues "Alonzo protocol config" a1 a2
      , cmpValues "Conway protocol config" c1 c2
      , cmpValues "Dijkstra protocol config" d1 d2
      , cmpValues "HardFork protocol config" h1 h2
      , cmpValues "Checkpoints protocol config" k1 k2
      ]
