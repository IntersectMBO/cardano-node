{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.Node.Protocol.Types
  ( SomeConsensusProtocol(..)
  ) where

import qualified Cardano.Api as Api

import           Cardano.Node.Orphans ()
import           Cardano.Node.Queries (HasKESInfo, HasKESMetricsData)
import           Cardano.Node.TraceConstraints (TraceConstraints)

data SomeConsensusProtocol where

     SomeConsensusProtocol :: forall blk. ( Api.Protocol IO blk
                                          , HasKESMetricsData blk
                                          , HasKESInfo blk
                                          , TraceConstraints blk
                                          )
                           => Api.BlockType blk
                           -> Api.ProtocolInfoArgs blk
                           -> SomeConsensusProtocol
