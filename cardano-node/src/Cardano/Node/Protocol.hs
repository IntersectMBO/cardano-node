module Cardano.Node.Protocol
  ( mkConsensusProtocol
  , SomeConsensusProtocol(..)
  , ProtocolInstantiationError(..)
  ) where

import           Cardano.Api

import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Types

import           Control.Exception

------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol ncProtocolConfig mProtocolFiles =
  case ncProtocolConfig of
    NodeProtocolConfigurationCardano
        byronConfig
        shelleyConfig
        alonzoConfig
        conwayConfig
        hardForkConfig ->
      firstExceptT CardanoProtocolInstantiationError $
        mkSomeConsensusProtocolCardano
          byronConfig
          shelleyConfig
          alonzoConfig
          conwayConfig
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

instance Exception ProtocolInstantiationError where
  displayException = docToString . prettyError

instance Error ProtocolInstantiationError where
  prettyError (ByronProtocolInstantiationError   err) = prettyError err
  prettyError (ShelleyProtocolInstantiationError err) = prettyError err
  prettyError (CardanoProtocolInstantiationError err) = prettyError err
