{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.ConfigCLI (
    -- * Untyped/typed protocol boundary
    Protocol(..)
  , SomeProtocol(..)
  , fromProtocol
    -- * CLI
  , NodeCLIArguments(..)
  , TopologyInfo(..)
  , NodeCommand(..)
  , TraceOptions (..)
  , ConsensusTraceOptions
  , ProtocolTraceOptions
  -- * Handy re-exports
  , execParser
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (read)

import           Data.Functor.Const (Const (..))
import qualified Data.IP as IP
import           Data.Semigroup ((<>))
import           Network.Socket (PortNumber)
import           Options.Applicative

import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers' (..))

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))

import           Cardano.Node.Topology (NodeAddress (..), TopologyInfo (..))

import           Cardano.Node.CLI

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data NodeCLIArguments = NodeCLIArguments {
    slotDuration :: !SlotLength
  , commonCLI    :: !CommonCLI
  , command      :: !NodeCommand
  }


type ConsensusTraceOptions = Consensus.Tracers' () ()    (Const Bool)
type ProtocolTraceOptions  = ProtocolTracers'   () () () (Const Bool)

-- | Tracing options. Each option enables a tracer which adds verbosity to the
-- log output.
data TraceOptions = TraceOptions
  { traceVerbosity       :: !TracingVerbosity
  , traceChainDB         :: !Bool
    -- ^ By default we use 'readableChainDB' tracer, if on this it will use
    -- more verbose tracer
  , traceConsensus       :: ConsensusTraceOptions
  , traceProtocols       :: ProtocolTraceOptions
  , traceIpSubscription  :: !Bool
  , traceDnsSubscription :: !Bool
  , traceDnsResolver     :: !Bool
  }

data NodeCommand =
    SimpleNode  TopologyInfo NodeAddress Protocol ViewMode TraceOptions
  | TxSubmitter TopologyInfo Mock.Tx     Protocol
  | TraceAcceptor
