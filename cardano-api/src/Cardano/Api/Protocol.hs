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
  , withLocalNodeConnectInfo
  ) where

import           Cardano.Prelude

import           Cardano.Chain.Slotting (EpochSlots (..))

import           Cardano.Api.Typed


data Protocol = ByronProtocol !EpochSlots
              | ShelleyProtocol
              | CardanoProtocol !EpochSlots
  deriving (Eq, Show)

withLocalNodeConnectInfo :: Protocol
                         -> NetworkId
                         -> FilePath
                         -> (forall mode. LocalNodeConnectInfo mode -> a)
                         -> a
withLocalNodeConnectInfo protocol network socketPath f =
    case protocol of
      ByronProtocol epochSlots ->
        f LocalNodeConnectInfo {
            localNodeConsensusModeParams = ByronModeParams epochSlots,
            localNodeNetworkId           = network,
            localNodeSocketPath          = socketPath
          }

      ShelleyProtocol ->
        f LocalNodeConnectInfo {
            localNodeConsensusModeParams = ShelleyModeParams,
            localNodeNetworkId           = network,
            localNodeSocketPath          = socketPath
          }
      CardanoProtocol epochSlots ->
        f LocalNodeConnectInfo {
            localNodeConsensusModeParams = CardanoModeParams epochSlots,
            localNodeNetworkId           = network,
            localNodeSocketPath          = socketPath
          }

