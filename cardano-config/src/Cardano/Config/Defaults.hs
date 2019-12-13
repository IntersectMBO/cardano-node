module Cardano.Config.Defaults
  ( traceOptionsDefault
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))


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
    , traceChainSyncProtocol = True
    , traceBlockFetchProtocol = True
    , traceBlockFetchProtocol' = True
    , traceTxSubmissionProtocol = True
    , traceLocalChainSyncProtocol = True
    , traceLocalTxSubmissionProtocol = True
    , traceIpSubscription = True
    , traceDnsSubscription = True
    , traceDnsResolver = True
    , traceErrorPolicy = True
    , traceMux = True
    }
