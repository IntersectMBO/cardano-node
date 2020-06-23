{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Protocol
  (
    -- * The enumeration of supported protocols
    Protocol(..)

    -- * Node client support
    -- | Support for the context needed to run a client of a node that is using
    -- a protocol.
  , mkNodeClientProtocol
  , SomeNodeClientProtocol(..)
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types (Protocol(..))
import           Cardano.Chain.Slotting (EpochSlots(..))

import           Cardano.Api.Protocol.Types
import           Cardano.Api.Protocol.Byron
import           Cardano.Api.Protocol.Cardano
import           Cardano.Api.Protocol.Shelley

import qualified Ouroboros.Consensus.Cardano as Consensus


mkNodeClientProtocol :: Protocol -> SomeNodeClientProtocol
mkNodeClientProtocol protocol =
    case protocol of
{-
      --TODO
      -- Mock protocols
      NodeProtocolConfigurationMock config ->
        case npcMockProtocol config of
          BFT      -> mkNodeClientProtocolMockBFT
          MockPBFT -> mkNodeClientProtocolMockPBFT
          Praos    -> mkNodeClientProtocolMockPraos
-}
      MockProtocol _ ->
        panic "TODO: mkNodeClientProtocol NodeProtocolConfigurationMock"

      -- Real protocols
      ByronProtocol ->
        mkSomeNodeClientProtocolByron
          --TODO: this is only the correct value for mainnet
          -- not for Byron testnets. This value is needed because
          -- to decode legacy EBBs one needs to know how many
          -- slots there are per-epoch. This info comes from
          -- the genesis file, but we don't have that in the
          -- client case.
          (EpochSlots 21600)
          (Consensus.SecurityParam 2160)

      ShelleyProtocol ->
        mkSomeNodeClientProtocolShelley

      CardanoProtocol ->
        mkSomeNodeClientProtocolCardano
          --TODO: this is only the correct value for mainnet
          -- not for Byron testnets. This value is needed because
          -- to decode legacy EBBs one needs to know how many
          -- slots there are per-epoch. This info comes from
          -- the genesis file, but we don't have that in the
          -- client case.
          (EpochSlots 21600)
          (Consensus.SecurityParam 2160)

