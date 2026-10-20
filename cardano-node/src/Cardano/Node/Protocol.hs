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
import           Ouroboros.Consensus.Protocol.Praos.AgentClient (KESPersistHook)

import           Control.Exception

------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> Maybe KESPersistHook
  -- ^ Installed on a file-based KES key, see 'Cardano.Node.KES.Persist'
  -> ExceptT ProtocolInstantiationError IO (SomeConsensusProtocol, GenesisHashShelley)
mkConsensusProtocol ncProtocolConfig mProtocolFiles mKesPersistHook =
  case ncProtocolConfig of
    NodeProtocolConfigurationCardano
        byronConfig
        shelleyConfig
        alonzoConfig
        conwayConfig
        dijkstraConfig
        hardForkConfig
        checkpointsConfig ->
      firstExceptT CardanoProtocolInstantiationError $
        mkSomeConsensusProtocolCardano
          byronConfig
          shelleyConfig
          alonzoConfig
          conwayConfig
          dijkstraConfig
          hardForkConfig
          checkpointsConfig
          mProtocolFiles
          mKesPersistHook

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
