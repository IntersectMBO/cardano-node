module Cardano.Node.Tracing.DefaultTraceConfig
  ( defaultCardanoConfig
  ) where

import           Cardano.Logging

import           Prelude

import qualified Data.Map.Strict as Map


defaultCardanoConfig :: TraceConfig
defaultCardanoConfig = emptyTraceConfig {
    tcMetricsPrefix = Just "cardano.node.metrics."
  , tcOptions = Map.fromList
     [([],
          [ ConfSeverity (SeverityF (Just Notice)) -- Means Silence
          , ConfDetail DNormal
          , ConfBackend  [Stdout MachineFormat
                         , EKGBackend
                         , Forwarder
                         ]])

     -- more important tracers going here
     ,(["BlockFetch", "Decision"],
          [ ConfSeverity (SeverityF Nothing)])
     ,(["ChainDB"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["ChainDB", "AddBlockEvent", "AddBlockValidation"],
          [ ConfSeverity (SeverityF Nothing)])
     ,(["ChainSync", "Client"],
          [ ConfSeverity (SeverityF (Just Warning))])
     ,(["Net", "ConnectionManager", "Remote"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Net", "Subscription", "DNS"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Startup", "DiffusionInit"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Net", "ErrorPolicy"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Forge", "Loop"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Forge", "StateInfo"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Net", "InboundGovernor", "Remote"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Net", "Subscription", "IP"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Net", "ErrorPolicy", "Local"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Mempool"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Net", "Mux", "Remote"],
          [ ConfSeverity (SeverityF (Just Info))])
     ,(["Net", "InboundGovernor"],
          [ ConfSeverity (SeverityF (Just Warning))])
     ,(["Net", "PeerSelection"],
          [ ConfSeverity (SeverityF Nothing)])
     ,(["Resources"],
          [ ConfSeverity (SeverityF Nothing)])
     --     Limiters
          ,(["ChainDB","AddBlockEvent","AddedBlockToQueue"],
               [ ConfLimiter 2.0])
          ,(["ChainDB","AddBlockEvent","AddedBlockToVolatileDB"],
               [ ConfLimiter 2.0])
          ,(["ChainDB","AddBlockEvent","AddBlockValidation", "ValidCandidate"],
               [ ConfLimiter 2.0])
          ,(["ChainDB", "CopyToImmutableDBEvent", "CopiedBlockToImmutableDB"],
               [ ConfLimiter 2.0])
          ,(["BlockFetch", "Client", "CompletedBlockFetch"],
               [ ConfLimiter 2.0])
     ]
  }
