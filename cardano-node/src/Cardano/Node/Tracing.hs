{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Cardano.Node.Tracing
  ( Tracers (..)
  , ConsensusStartupException (..)
  ) where

import Prelude (IO)

import Codec.CBOR.Read (DeserialiseFailure)
import "contra-tracer" Control.Tracer (Tracer)

import Ouroboros.Consensus.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Consensus.Network.NodeToNode   qualified as NodeToNode
import Ouroboros.Consensus.Node.Tracers         qualified as Consensus
import Ouroboros.Consensus.Storage.ChainDB      qualified as ChainDB
import Ouroboros.Network.Diffusion              qualified as Diffusion

import Ouroboros.Network.NodeToNode   (NodeToNodeVersion, RemoteAddress)
import Ouroboros.Network.NodeToClient (LocalAddress, NodeToClientVersion)

import Cardano.Node.Handlers.Shutdown (ShutdownTrace)
import Cardano.Node.Startup           (NodeInfo, StartupTrace)

import Cardano.Logging.Resources
import Cardano.Node.Tracing.StateRep (NodeState)
import Cardano.Node.Tracing.Tracers.Peer (PeerT)
import Cardano.Node.Tracing.Tracers.ConsensusStartupException

data Tracers peer localPeer blk p2p = Tracers
  { -- | Trace the ChainDB
    chainDBTracer         :: Tracer IO (ChainDB.TraceEvent blk)
    -- | Consensus-specific tracers.
  , consensusTracers      :: Consensus.Tracers IO peer localPeer blk
    -- | Tracers for the node-to-node protocols.
  , nodeToNodeTracers     :: NodeToNode.Tracers IO peer blk DeserialiseFailure
    --, serialisedBlockTracer :: NodeToNode.SerialisedTracer IO peer blk (SerialisedBlockTrace)
    -- | Tracers for the node-to-client protocols
  , nodeToClientTracers   :: NodeToClient.Tracers IO localPeer blk DeserialiseFailure
    -- | Diffusion tracers
  , diffusionTracers      :: Diffusion.Tracers RemoteAddress NodeToNodeVersion
                                               LocalAddress  NodeToClientVersion
                                              IO
  , diffusionTracersExtra :: Diffusion.ExtraTracers p2p

  , startupTracer         :: Tracer IO (StartupTrace blk)
  , shutdownTracer        :: Tracer IO ShutdownTrace
  , nodeInfoTracer        :: Tracer IO NodeInfo
  , nodeStateTracer       :: Tracer IO NodeState
  , resourcesTracer       :: Tracer IO ResourceStats
  , peersTracer           :: Tracer IO [PeerT blk]
  }
