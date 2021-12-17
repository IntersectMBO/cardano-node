{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition
  ( CliError (..)
  , startProtocol
  ) where

import           Paths_tx_generator (version)
import           Prelude (error, show)

import           Data.Text (pack)
import           Data.Version (showVersion)

import           Cardano.Prelude hiding (TypeError, show)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Ouroboros.Network.Block (MaxSlotNo(..))

import qualified Cardano.Chain.Genesis as Genesis

import           Cardano.Node.Configuration.Logging
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Cardano.Node.Types
import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx

startProtocol
  :: FilePath
  -> ExceptT CliError IO (LoggingLayer, SomeConsensusProtocol)
startProtocol logConfigFile = do
  nc <- liftIO $ mkNodeConfig logConfigFile
  case ncProtocolConfig nc of
    NodeProtocolConfigurationByron _    -> error "NodeProtocolConfigurationByron not supported"
    NodeProtocolConfigurationShelley _  -> error "NodeProtocolConfigurationShelley not supported"
    NodeProtocolConfigurationCardano byronConfig shelleyConfig alonzoConfig hardforkConfig -> do
        ptcl :: SomeConsensusProtocol <- firstExceptT (ProtocolInstantiationError . pack . show) $
                  mkSomeConsensusProtocolCardano byronConfig shelleyConfig alonzoConfig hardforkConfig Nothing

        loggingLayer <- mkLoggingLayer nc ptcl
        return (loggingLayer, ptcl)
 where
  mkLoggingLayer :: NodeConfiguration -> SomeConsensusProtocol -> ExceptT CliError IO LoggingLayer
  mkLoggingLayer nc ptcl =
    firstExceptT (\(ConfigErrorFileNotFound fp) -> ConfigNotFoundError fp) $
    createLoggingLayer (pack $ showVersion version) nc ptcl

  mkNodeConfig :: FilePath -> IO NodeConfiguration
  mkNodeConfig logConfig = do
   let configFp = ConfigYamlFilePath logConfig
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
                 , pncShutdownIPC = Last Nothing
                 , pncShutdownOnSlotSynced = Last $ Just NoMaxSlotNo
                 , pncConfigFile = Last $ Just configFp
                 }
   configYamlPc <- parseNodeConfigurationFP . Just $ configFp
   case makeNodeConfiguration $ configYamlPc <> filesPc of
      Left err -> panic $ "Error in creating the NodeConfiguration: " <> pack err
      Right nc' -> return nc'

data CliError  =
    GenesisReadError !FilePath !Genesis.GenesisDataError
  | FileNotFoundError !FilePath
  | ConfigNotFoundError !FilePath
  | ProtocolInstantiationError !Text
  | BenchmarkRunnerError !GeneratorTx.TxGenError
  deriving stock Show
