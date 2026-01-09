{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}


module Cardano.Node.Tracing
  ( Tracers (..)
  , ConsensusStartupException (..)
  ) where

import           Cardano.Logging.Resources
import qualified Cardano.Network.Diffusion as Cardano.Diffusion
import           Cardano.Node.Handlers.Shutdown (ShutdownTrace)
import           Cardano.Node.Protocol (ProtocolInstantiationError)
import           Cardano.Node.Startup (NodeInfo, NodeStartupInfo, StartupTrace (..))
import           Cardano.Node.Tracing.StateRep (NodeState)
import           Cardano.Node.Tracing.Tracers.ConsensusStartupException
                   (ConsensusStartupException (..))
import           Cardano.Node.Tracing.Tracers.LedgerMetrics (LedgerMetrics)
import           Cardano.Node.Tracing.Tracers.NodeVersion (NodeVersionTrace)
import           Cardano.Node.Tracing.Tracers.Peer (PeerT)
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Network.ConnectionId

import           Prelude (IO)

import           Codec.CBOR.Read (DeserialiseFailure)
import           "contra-tracer" Control.Tracer (Tracer (..))

data Tracers peer localPeer blk m = Tracers
  { -- | Trace the ChainDB
    chainDBTracer         :: !(Tracer IO (ChainDB.TraceEvent blk))
    -- | Consensus-specific tracers.
  , consensusTracers      :: !(Consensus.Tracers IO (ConnectionId peer) (ConnectionId localPeer) blk)
    -- | Tracers for the node-to-node protocols.
  , nodeToNodeTracers     :: !(NodeToNode.Tracers IO peer blk DeserialiseFailure)
    --, serialisedBlockTracer :: NodeToNode.SerialisedTracer IO peer blk (SerialisedBlockTrace)
    -- | Tracers for the node-to-client protocols
  , nodeToClientTracers   :: !(NodeToClient.Tracers IO (ConnectionId localPeer) blk DeserialiseFailure)
    -- | Diffusion tracers
  , diffusionTracers      :: !(Cardano.Diffusion.CardanoTracers m)
  , churnModeTracer       :: !(Tracer IO Cardano.Diffusion.TraceChurnMode)
  , startupTracer         :: !(Tracer IO (StartupTrace blk ProtocolInstantiationError))
  , shutdownTracer        :: !(Tracer IO ShutdownTrace)
  , nodeInfoTracer        :: !(Tracer IO NodeInfo)
  , nodeVersionTracer     :: !(Tracer IO NodeVersionTrace)
  , nodeStartupInfoTracer :: !(Tracer IO NodeStartupInfo)
  , nodeStateTracer       :: !(Tracer IO NodeState)
  , resourcesTracer       :: !(Tracer IO ResourceStats)
  , peersTracer           :: !(Tracer IO [PeerT blk])
  , ledgerMetricsTracer   :: !(Tracer IO LedgerMetrics)
  }
