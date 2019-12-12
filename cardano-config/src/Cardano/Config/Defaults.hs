module Cardano.Config.Defaults
  ( traceOptionsDefault
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers'(..))


protocolTraceDefault :: ProtocolTraceOptions
protocolTraceDefault =
  ProtocolTracers
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)

traceOptionsDefault :: TraceOptions
traceOptionsDefault =
  TraceOptions
    { traceVerbosity = NormalVerbosity
    , traceChainDB = True
    , traceChainSyncClient = True
    , traceChainSyncHeaderServer = True
    , traceChainSyncBlockServer = True
    , traceBlockFetchDecisions = True
    , traceBlockFetchClient = True
    , traceBlockFetchServer = True
    , traceTxInbound = True
    , traceTxOutbound = True
    , traceLocalTxSubmissionServer = True
    , traceMempool = True
    , traceForge = True
    , traceProtocols = protocolTraceDefault
    , traceIpSubscription = True
    , traceDnsSubscription = True
    , traceDnsResolver = True
    , traceErrorPolicy = True
    , traceMux = True
    }
