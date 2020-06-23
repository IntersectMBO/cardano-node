{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Protocol
  ( mkConsensusProtocol
  , SomeConsensusProtocol(..)
  , ProtocolInstantiationError(..)
  , renderProtocolInstantiationError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Config.Types
                   (NodeConfiguration(..), NodeProtocolConfiguration(..),
                    NodeMockProtocolConfiguration(..), ProtocolFilepaths(..),
                    MockProtocol(..))

import           Cardano.Node.Protocol.Types (SomeConsensusProtocol(..))
import           Cardano.Node.Protocol.Mock
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Protocol.Cardano

------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol NodeConfiguration{ncProtocolConfig} files =
    case ncProtocolConfig of

      -- Mock protocols
      NodeProtocolConfigurationMock config ->
        case npcMockProtocol config of
          MockBFT   -> pure $ mkSomeConsensusProtocolMockBFT   config
          MockPBFT  -> pure $ mkSomeConsensusProtocolMockPBFT  config
          MockPraos -> pure $ mkSomeConsensusProtocolMockPraos config

      -- Real protocols
      NodeProtocolConfigurationByron config ->
        firstExceptT ByronProtocolInstantiationError $
          mkSomeConsensusProtocolByron config files

      NodeProtocolConfigurationShelley config ->
        firstExceptT ShelleyProtocolInstantiationError $
          mkSomeConsensusProtocolShelley config files

      NodeProtocolConfigurationCardano byronConfig
                                       shelleyConfig
                                       hardForkConfig ->
        firstExceptT CardanoProtocolInstantiationError $
          mkSomeConsensusProtocolCardano
            byronConfig
            shelleyConfig
            hardForkConfig
            files

------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError   ByronProtocolInstantiationError
  | ShelleyProtocolInstantiationError ShelleyProtocolInstantiationError
  | CardanoProtocolInstantiationError CardanoProtocolInstantiationError
  deriving Show


renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
    ByronProtocolInstantiationError bpie ->
      renderByronProtocolInstantiationError bpie

    ShelleyProtocolInstantiationError spie ->
      renderShelleyProtocolInstantiationError spie

    CardanoProtocolInstantiationError cpie ->
      renderCardanoProtocolInstantiationError cpie
