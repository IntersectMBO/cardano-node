{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}

module Cardano.Node.Protocol.Types
  ( SomeConsensusProtocol(..)
  , SomeConsensusProtocolConstraints
  ) where

import           Cardano.Prelude

import           Cardano.BM.Tracing (Transformable)
import           Ouroboros.Consensus.Block (BlockProtocol, ForgeState (..))
import qualified Ouroboros.Consensus.Cardano as Consensus (Protocol)
import           Ouroboros.Consensus.Node.Run (RunNode)

import           Cardano.Config.Types (HasKESMetricsData, TraceConstraints)

type SomeConsensusProtocolConstraints blk =
     ( HasKESMetricsData blk
     , RunNode blk
     , TraceConstraints blk
     , Transformable Text IO (ForgeState blk)
     )


data SomeConsensusProtocol where

     SomeConsensusProtocol :: SomeConsensusProtocolConstraints blk
                           => Consensus.Protocol IO blk (BlockProtocol blk)
                           -> SomeConsensusProtocol

