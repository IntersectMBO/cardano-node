{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


module Cardano.Api.Protocol
  (
    -- * The enumeration of supported protocols
    Protocol(..)
  , MockProtocol(..)

    -- * Node client support
    -- | Support for the context needed to run a client of a node that is using
    -- a protocol.
  , mkNodeClientProtocol
  , SomeNodeClientProtocol(..)
  ) where

import           Cardano.Prelude

import           Cardano.Chain.Slotting (EpochSlots(..))

import           Cardano.Api.Protocol.Types
import           Cardano.Api.Protocol.Byron
import           Cardano.Api.Protocol.Cardano
import           Cardano.Api.Protocol.Orphans ()
import           Cardano.Api.Protocol.Shelley

import qualified Ouroboros.Consensus.Cardano as Consensus

data Protocol = MockProtocol !MockProtocol
              | ByronProtocol !EpochSlots !Consensus.SecurityParam
              | ShelleyProtocol
              | CardanoProtocol !EpochSlots !Consensus.SecurityParam
  deriving (Eq, Show, Generic)

deriving instance NFData Protocol
deriving instance NoUnexpectedThunks Protocol

data MockProtocol = MockBFT
                  | MockPBFT
                  | MockPraos
  deriving (Eq, Show, Generic)

deriving instance NFData MockProtocol
deriving instance NoUnexpectedThunks MockProtocol

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
      ByronProtocol epSlots secParam ->
        mkSomeNodeClientProtocolByron epSlots secParam

      ShelleyProtocol ->
        mkSomeNodeClientProtocolShelley

      CardanoProtocol epSlots secParam ->
        mkSomeNodeClientProtocolCardano epSlots secParam
