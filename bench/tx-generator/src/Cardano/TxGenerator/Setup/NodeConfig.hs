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
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Handlers.Shutdown (ShutdownConfig (..))
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Types (ConfigYamlFilePath (..), GenesisFile, KESSource (..),
                   NodeProtocolConfiguration (..), NodeShelleyProtocolConfiguration (..),
                   ProtocolFilepaths (..), unGenesisFile)
import           Cardano.TxGenerator.Types
import qualified Ouroboros.Consensus.Cardano.Node as Consensus

import           Control.Applicative (Const (Const), getConst)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Bifunctor (first)
import           Data.Monoid
import           System.FilePath (takeDirectory)


-- | extract the Shelley genesis from a Cardano protocol
-- NB. this helper is *only* for protocols created with this module
-- as this guarantees proper error handling when trying to create a non-Cardano
-- protocol.
getShelleyGenesis :: SomeConsensusProtocol -> ShelleyGenesis
getShelleyGenesis (SomeConsensusProtocol CardanoBlockType proto)
    = getConst $ Ledger.tcShelleyGenesisL Const transCfg
  where
    ProtocolInfoArgsCardano _ Consensus.CardanoProtocolParams
      { Consensus.cardanoLedgerTransitionConfig = transCfg
      } = proto

-- | extract the path to the Shelley genesis file from a NodeConfiguration for
-- Cardano protocol.
getShelleyGenesisPath :: NodeConfiguration -> Maybe GenesisFile
getShelleyGenesisPath nodeConfig =
  case ncProtocolConfig nodeConfig of
    NodeProtocolConfigurationCardano _ shelleyConfig _ _ _ _ _ ->
      Just $ npcShelleyGenesisFile shelleyConfig

-- | extract the directory containing the Shelley genesis file: the root
-- extraConfig FILE injections resolve their "file" segments against (see
-- 'Cardano.Node.Protocol.Cardano.mkSomeConsensusProtocolCardano', which mounts
-- its HasFS at this same directory).
getShelleyGenesisDir :: NodeConfiguration -> Maybe FilePath
getShelleyGenesisDir = fmap (takeDirectory . unGenesisFile) . getShelleyGenesisPath

mkConsensusProtocol :: NodeConfiguration -> IO (Either TxGenError SomeConsensusProtocol)
mkConsensusProtocol nodeConfig =
  case ncProtocolConfig nodeConfig of
    NodeProtocolConfigurationCardano byronConfig shelleyConfig alonzoConfig conwayConfig dijkstraConfig hardforkConfig checkpointsConfig ->
      first ProtocolError . fmap fst
        <$> runExceptT (mkSomeConsensusProtocolCardano byronConfig shelleyConfig alonzoConfig conwayConfig dijkstraConfig hardforkConfig checkpointsConfig Nothing)

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
                 , shelleyKESSource = Just (KESKeyFilePath "")
                 , shelleyVRFFile = Just ""
                 , shelleyCertFile = Just ""
                 , shelleyBulkCredsFile = Just ""
                 }
               , pncValidateDB = Last $ Just False
               , pncShutdownConfig = Last $ Just $ ShutdownConfig Nothing Nothing
               , pncConfigFile = Last $ Just configFp
               }
