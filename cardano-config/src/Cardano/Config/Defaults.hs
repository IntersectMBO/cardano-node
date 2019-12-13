module Cardano.Config.Defaults
  ( muteTracing
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))


muteTracing :: TraceOptions
muteTracing = TraceOptions
  { traceVerbosity = NormalVerbosity
  , traceChainDB = False
  , traceChainSyncClient = False
  , traceChainSyncHeaderServer = False
  , traceChainSyncBlockServer = False
  , traceBlockFetchDecisions = False
  , traceBlockFetchClient = False
  , traceBlockFetchServer = False
  , traceTxInbound = False
  , traceTxOutbound = False
  , traceLocalTxSubmissionServer = False
  , traceMempool = False
  , traceForge = False
  , traceChainSyncProtocol = False
  , traceBlockFetchProtocol = False
  , traceBlockFetchProtocolSerialised = False
  , traceTxSubmissionProtocol = False
  , traceLocalChainSyncProtocol = False
  , traceLocalTxSubmissionProtocol = False
  , traceIpSubscription = False
  , traceDnsSubscription = False
  , traceDnsResolver = False
  , traceErrorPolicy = False
  , traceMux = False
  }
