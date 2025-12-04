{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Cardano.Node.Protocol
  ( mkConsensusProtocol
  , SomeConsensusProtocol(..)
  , ProtocolInstantiationError(..)
  ) where

import           Cardano.Api hiding (LedgerState)

import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Types

import           Control.Exception
import NoThunks.Class
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Ledger.Tables

------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NoThunks (LedgerState (CardanoBlock StandardCrypto) EmptyMK) => NodeProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol ncProtocolConfig mProtocolFiles =
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
