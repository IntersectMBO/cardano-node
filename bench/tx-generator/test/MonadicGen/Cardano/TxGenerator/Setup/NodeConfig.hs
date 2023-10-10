{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | This module provides convenience functions when dealing
--   with cardano-node's configuration and config files
--   as well as protocol instantiation.
module MonadicGen.Cardano.TxGenerator.Setup.NodeConfig
       (module MonadicGen.Cardano.TxGenerator.Setup.NodeConfig)
       where

import qualified Control.Applicative as Appl (Const(Const), getConst)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Bifunctor (first)
import           Data.Monoid

import           Ouroboros.Consensus.Cardano as Consensus (ProtocolParams (CardanoProtocolParams), ledgerTransitionConfig)

import           Cardano.Api (BlockType (..), ProtocolInfoArgs (..))
import qualified Cardano.Ledger.Api.Transition as Ledger (tcShelleyGenesisL)
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Handlers.Shutdown (ShutdownConfig (..))
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Types (ConfigYamlFilePath (..), GenesisFile,
                   NodeProtocolConfiguration (..), NodeShelleyProtocolConfiguration (..),
                   ProtocolFilepaths (..))
import           MonadicGen.Cardano.TxGenerator.Types


-- | extract genesis from a Cardano protocol
-- NB. this helper is *only* for protocols created with this module
-- as this guarantees proper error handling when trying to create a non-Cardano protocol.
getGenesis :: SomeConsensusProtocol -> ShelleyGenesis
getGenesis (SomeConsensusProtocol CardanoBlockType proto)
    = Appl.getConst $ Ledger.tcShelleyGenesisL Appl.Const transCfg
  where
    ProtocolInfoArgsCardano Consensus.CardanoProtocolParams
      { Consensus.ledgerTransitionConfig = transCfg
      } = proto

-- | extract the path to genesis file from a NodeConfiguration for Cardano protocol
getGenesisPath :: NodeConfiguration -> Maybe GenesisFile
getGenesisPath nodeConfig
 = case ncProtocolConfig nodeConfig of
   NodeProtocolConfigurationCardano _ shelleyConfig _ _ _ -> Just $ npcShelleyGenesisFile shelleyConfig
   _ -> Nothing

mkConsensusProtocol :: NodeConfiguration -> IO (Either TxGenError SomeConsensusProtocol)
mkConsensusProtocol nodeConfig
  = case ncProtocolConfig nodeConfig of
    NodeProtocolConfigurationByron _    -> pure $ Left $ TxGenError "NodeProtocolConfigurationByron not supported"
    NodeProtocolConfigurationShelley _  -> pure $ Left $ TxGenError "NodeProtocolConfigurationShelley not supported"
    NodeProtocolConfigurationCardano byronConfig shelleyConfig alonzoConfig conwayConfig hardforkConfig
        -> first ProtocolError
            <$> runExceptT (mkSomeConsensusProtocolCardano byronConfig shelleyConfig alonzoConfig conwayConfig hardforkConfig Nothing)

-- | Creates a NodeConfiguration from a config file;
--   the result is devoid of any keys/credentials
mkNodeConfig :: FilePath -> IO (Either TxGenError NodeConfiguration)
mkNodeConfig configFp_
  = do
    configYamlPc <- parseNodeConfigurationFP . Just $ configFp
    return
        $ first (TxGenError . ("mkNodeConfig: " ++))
        $! makeNodeConfiguration (configYamlPc <> filesPc)
  where
    configFp = ConfigYamlFilePath configFp_

    filesPc :: PartialNodeConfiguration
    filesPc = defaultPartialNodeConfiguration
               { pncProtocolFiles = Last . Just $
                 ProtocolFilepaths
                 { byronCertFile = Just ""
                 , byronKeyFile = Just ""
                 , shelleyKESFile = Just ""
                 , shelleyVRFFile = Just ""
                 , shelleyCertFile = Just ""
                 , shelleyBulkCredsFile = Just ""
                 }
               , pncValidateDB = Last $ Just False
               , pncShutdownConfig = Last $ Just $ ShutdownConfig Nothing Nothing
               , pncConfigFile = Last $ Just configFp
               }
