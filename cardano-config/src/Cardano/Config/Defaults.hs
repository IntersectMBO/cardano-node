module Cardano.Config.Defaults
  ( traceOptionsDefault
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import qualified Ouroboros.Consensus.Node.Tracers as ConsensusTracers
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers'(..))

consensusTraceDefault :: ConsensusTraceOptions
consensusTraceDefault =
  ConsensusTracers.Tracers
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)
    (Const True)

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
    , traceConsensus = consensusTraceDefault
    , traceProtocols = protocolTraceDefault
    , traceIpSubscription = True
    , traceDnsSubscription = True
    , traceDnsResolver = True
    , traceErrorPolicy = True
    , traceMux = True
    }
