{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | This module provides convenience functions when dealing
--   with cardano-node's configuration and config files
--   as well as protocol instantiation.
module Cardano.TxGenerator.Setup.NodeConfig
       (module Cardano.TxGenerator.Setup.NodeConfig)
       where

import           Cardano.Api (BlockType (..), ProtocolInfoArgs (..))

import qualified Cardano.Ledger.Api.Transition as Ledger (tcShelleyGenesisL)
import           Cardano.Node.Configuration.Adapter (nodeConfigurationFromFile)
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Types (GenesisFile, NodeProtocolConfiguration (..),
                   NodeShelleyProtocolConfiguration (..))
import           Cardano.TxGenerator.Types
import qualified Ouroboros.Consensus.Cardano.Node as Consensus

import           Control.Applicative (Const (Const), getConst)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Bifunctor (first)


-- | extract genesis from a Cardano protocol
-- NB. this helper is *only* for protocols created with this module
-- as this guarantees proper error handling when trying to create a non-Cardano protocol.
getGenesis :: SomeConsensusProtocol -> ShelleyGenesis
getGenesis (SomeConsensusProtocol CardanoBlockType proto)
    = getConst $ Ledger.tcShelleyGenesisL Const transCfg
  where
    ProtocolInfoArgsCardano _ Consensus.CardanoProtocolParams
      { Consensus.cardanoLedgerTransitionConfig = transCfg
      } = proto

-- | extract the path to genesis file from a NodeConfiguration for Cardano protocol
getGenesisPath :: NodeConfiguration -> Maybe GenesisFile
getGenesisPath nodeConfig =
  case ncProtocolConfig nodeConfig of
    NodeProtocolConfigurationCardano _ shelleyConfig _ _ _ _ _ ->
      Just $ npcShelleyGenesisFile shelleyConfig

mkConsensusProtocol :: NodeConfiguration -> IO (Either TxGenError SomeConsensusProtocol)
mkConsensusProtocol nodeConfig =
  case ncProtocolConfig nodeConfig of
    NodeProtocolConfigurationCardano byronConfig shelleyConfig alonzoConfig conwayConfig dijkstraConfig hardforkConfig checkpointsConfig ->
      first ProtocolError
        <$> runExceptT (mkSomeConsensusProtocolCardano byronConfig shelleyConfig alonzoConfig conwayConfig dijkstraConfig hardforkConfig checkpointsConfig Nothing)

-- | Creates a NodeConfiguration from a config file;
--   the result is devoid of any keys/credentials.
--
-- The configuration is parsed and resolved through the @cardano-config@ package
-- (via "Cardano.Node.Configuration.Adapter"), applying no CLI overrides, so the
-- credential and socket paths are left unset.
mkNodeConfig :: FilePath -> IO (Either TxGenError NodeConfiguration)
mkNodeConfig configFp =
    first (TxGenError . ("mkNodeConfig: " ++)) <$> nodeConfigurationFromFile configFp
