module Cardano.Node.Protocol
  ( mkConsensusProtocol
  , SomeConsensusProtocol(..)
  , ProtocolInstantiationError(..)
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Api

import           Cardano.Node.Types

import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))


------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol ncProtocolConfig mProtocolFiles =
    case ncProtocolConfig of

      NodeProtocolConfigurationByron config ->
        firstExceptT ByronProtocolInstantiationError $
          mkSomeConsensusProtocolByron config mProtocolFiles

      NodeProtocolConfigurationShelley config ->
        firstExceptT ShelleyProtocolInstantiationError $
          mkSomeConsensusProtocolShelley config mProtocolFiles

      NodeProtocolConfigurationCardano byronConfig
                                       shelleyConfig
                                       alonzoConfig
                                       hardForkConfig ->
        firstExceptT CardanoProtocolInstantiationError $
          mkSomeConsensusProtocolCardano
            byronConfig
            shelleyConfig
            alonzoConfig
            hardForkConfig
            mProtocolFiles

------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError   ByronProtocolInstantiationError
  | ShelleyProtocolInstantiationError ShelleyProtocolInstantiationError
  | CardanoProtocolInstantiationError CardanoProtocolInstantiationError
  deriving Show


instance Error ProtocolInstantiationError where
  displayError (ByronProtocolInstantiationError   err) = displayError err
  displayError (ShelleyProtocolInstantiationError err) = displayError err
  displayError (CardanoProtocolInstantiationError err) = displayError err

