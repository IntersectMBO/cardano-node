{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Api.Protocol
  (
    -- * The enumeration of supported protocols
    Protocol(..)

    -- * Node client support
    -- | Support for the context needed to run a client of a node that is using
    -- a protocol.
  , localNodeConnectInfo
  , withlocalNodeConnectInfo
  , LocalNodeConnectInfoForSomeMode(..)
  ) where

import           Cardano.Prelude

import           Cardano.Chain.Slotting (EpochSlots (..))

import           Cardano.Api.Typed

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Node.Run (RunNode)


data Protocol = ByronProtocol !EpochSlots !Consensus.SecurityParam
              | ShelleyProtocol
              | CardanoProtocol !EpochSlots !Consensus.SecurityParam
  deriving (Eq, Show)

data LocalNodeConnectInfoForSomeMode where

     LocalNodeConnectInfoForSomeMode
       :: RunNode block
       => LocalNodeConnectInfo mode block
       -> LocalNodeConnectInfoForSomeMode

withlocalNodeConnectInfo :: Protocol
                         -> NetworkId
                         -> FilePath
                         -> (forall mode block.
                                RunNode block
                             => LocalNodeConnectInfo mode block
                             -> a)
                         -> a
withlocalNodeConnectInfo protocol network socketPath f =
    case localNodeConnectInfo protocol network socketPath of
      LocalNodeConnectInfoForSomeMode connctInfo -> f connctInfo

localNodeConnectInfo :: Protocol
                     -> NetworkId
                     -> FilePath
                     -> LocalNodeConnectInfoForSomeMode
localNodeConnectInfo protocol network socketPath =
    case protocol of

      ByronProtocol epSlots secParam ->
        LocalNodeConnectInfoForSomeMode $
          LocalNodeConnectInfo
            socketPath network
            (ByronMode epSlots secParam)

      ShelleyProtocol ->
        LocalNodeConnectInfoForSomeMode $
          LocalNodeConnectInfo
            socketPath network
            ShelleyMode

      CardanoProtocol epSlots secParam ->
        LocalNodeConnectInfoForSomeMode $
          LocalNodeConnectInfo
            socketPath network
            (CardanoMode epSlots secParam)

