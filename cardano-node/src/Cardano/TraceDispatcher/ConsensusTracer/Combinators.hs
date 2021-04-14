
module Cardano.TraceDispatcher.ConsensusTracer.Combinators
  (
    severityChainSyncClientEvent
  , namesForChainSyncClientEvent

  , severityChainSyncServerEvent
  , namesForChainSyncServerEvent

  , severityBlockFetchDecision
  , namesForBlockFetchDecision
  ) where


import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))

import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Consensus.Block (Point)

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

severityChainSyncServerEvent :: TraceChainSyncServerEvent blk -> SeverityS
severityChainSyncServerEvent TraceChainSyncServerRead        {} = Info
severityChainSyncServerEvent TraceChainSyncServerReadBlocked {} = Info
severityChainSyncServerEvent TraceChainSyncRollForward       {} = Info
severityChainSyncServerEvent TraceChainSyncRollBackward      {} = Info

namesForChainSyncServerEvent :: TraceChainSyncServerEvent blk -> [Text]
namesForChainSyncServerEvent TraceChainSyncServerRead        {} =
      ["ServerRead"]
namesForChainSyncServerEvent TraceChainSyncServerReadBlocked {} =
      ["ServerReadBlocked"]
namesForChainSyncServerEvent TraceChainSyncRollForward       {} =
      ["RollForward"]
namesForChainSyncServerEvent TraceChainSyncRollBackward      {} =
      ["RollBackward"]

severityBlockFetchDecision ::
     [TraceLabelPeer peer (FetchDecision [Point header])]
  -> SeverityS
severityBlockFetchDecision []  = Info
severityBlockFetchDecision l   = maximum $
  map (\(TraceLabelPeer _ a) -> fetchDecisionSeverity a) l
    where
      fetchDecisionSeverity :: FetchDecision a -> SeverityS
      fetchDecisionSeverity fd =
        case fd of
          Left FetchDeclineChainNotPlausible     -> Debug
          Left FetchDeclineChainNoIntersection   -> Notice
          Left FetchDeclineAlreadyFetched        -> Debug
          Left FetchDeclineInFlightThisPeer      -> Debug
          Left FetchDeclineInFlightOtherPeer     -> Debug
          Left FetchDeclinePeerShutdown          -> Info
          Left FetchDeclinePeerSlow              -> Info
          Left FetchDeclineReqsInFlightLimit {}  -> Info
          Left FetchDeclineBytesInFlightLimit {} -> Info
          Left FetchDeclinePeerBusy {}           -> Info
          Left FetchDeclineConcurrencyLimit {}   -> Info
          Right _                                -> Info

namesForBlockFetchDecision :: [TraceLabelPeer peer (FetchDecision [Point header])] -> [Text]
namesForBlockFetchDecision _ = []
