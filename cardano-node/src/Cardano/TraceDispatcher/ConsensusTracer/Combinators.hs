
module Cardano.TraceDispatcher.ConsensusTracer.Combinators
  (
    severityChainSyncClientEvent
  , namesForChainSyncClientEvent
  ) where

import           Data.Text (Text)

import           Cardano.Logging

import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client

severityChainSyncClientEvent :: TraceChainSyncClientEvent blk -> SeverityS
severityChainSyncClientEvent TraceDownloadedHeader {}  = Info
severityChainSyncClientEvent TraceFoundIntersection {} = Info
severityChainSyncClientEvent TraceRolledBack {}        = Notice
severityChainSyncClientEvent TraceException {}         = Warning
severityChainSyncClientEvent TraceTermination {}       = Notice

namesForChainSyncClientEvent :: TraceChainSyncClientEvent blk -> [Text]
namesForChainSyncClientEvent TraceDownloadedHeader {} =
      ["DownloadedHeader"]
namesForChainSyncClientEvent TraceFoundIntersection {} =
      ["FoundIntersection"]
namesForChainSyncClientEvent TraceRolledBack {} =
      ["RolledBack"]
namesForChainSyncClientEvent TraceException {} =
      ["Exception"]
namesForChainSyncClientEvent TraceTermination {} =
      ["Termination"]
