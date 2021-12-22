{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition
  ( CliError (..)
  , runBenchmarkScriptWith
  , startProtocol
  ) where

import           Paths_tx_generator (version)
import           Prelude (error, show)

import           Data.Text (pack)
import           Data.Version (showVersion)

import           Cardano.Prelude hiding (TypeError, show)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Ouroboros.Consensus.Config (configBlock, configCodec)
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..), getNetworkMagic)
import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Api

import qualified Cardano.Chain.Genesis as Genesis

import           Cardano.Node.Configuration.Logging
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Cardano.Node.Types

import           Cardano.Benchmarking.DSL
import           Cardano.Benchmarking.Tracer

import           Cardano.Benchmarking.GeneratorTx.NodeToNode
import           Cardano.Benchmarking.OuroborosImports (getGenesis, protocolToNetworkId,
                   protocolToTopLevelConfig)

import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx
import qualified Cardano.Benchmarking.GeneratorTx.Tx as GeneratorTx

mangleLocalProtocolDefinition ::
     SomeConsensusProtocol
  -> IOManager
  -> SocketPath
  -> BenchTracers
  -> MonoDSLs
mangleLocalProtocolDefinition
  ptcl
  iom
  (SocketPath sock)
  tracers
  = (DSL {..}, DSL {..}, DSL {..})
 where
  topLevelConfig = protocolToTopLevelConfig ptcl

  localConnectInfo :: LocalNodeConnectInfo CardanoMode
  localConnectInfo = LocalNodeConnectInfo
     (CardanoModeParams (EpochSlots 21600))        -- TODO: get this from genesis
     networkId
     sock

  connectClient :: ConnectClient
  connectClient  = benchmarkConnectTxSubmit
                     iom
                     (btConnect_ tracers)
                     (btSubmission2_ tracers)
                     (configCodec topLevelConfig)
                     (getNetworkMagic $ configBlock topLevelConfig)

  networkId = protocolToNetworkId ptcl

  keyAddress :: IsShelleyBasedEra era => KeyAddress era
  keyAddress = GeneratorTx.keyAddress networkId

  secureGenesisFund :: IsShelleyBasedEra era => SecureGenesisFund era
  secureGenesisFund = GeneratorTx.secureGenesisFund
              (btTxSubmit_ tracers)
              (submitTxToNodeLocal localConnectInfo)
              networkId
              (getGenesis ptcl)

  splitFunds :: IsShelleyBasedEra era => SplitFunds era
  splitFunds = GeneratorTx.splitFunds
              (btTxSubmit_ tracers)
              (submitTxToNodeLocal localConnectInfo)

  txGenerator :: IsShelleyBasedEra era => TxGenerator era
  txGenerator = GeneratorTx.txGenerator (btTxSubmit_ tracers)

  runBenchmark :: IsShelleyBasedEra era => RunBenchmark era
  runBenchmark = GeneratorTx.runBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient

runBenchmarkScriptWith ::
     IOManager
  -> FilePath
  -> SocketPath
  -> BenchmarkScript a
  -> ExceptT CliError IO a
runBenchmarkScriptWith iocp logConfigFile socketFile script = do
  (loggingLayer, ptcl) <- startProtocol logConfigFile
  let tracers :: BenchTracers
      tracers = createLoggingLayerTracers loggingLayer
      dslSet :: MonoDSLs
      dslSet = mangleLocalProtocolDefinition ptcl iocp socketFile tracers
  res <- firstExceptT BenchmarkRunnerError $ script (tracers, dslSet)
  liftIO $ do
          threadDelay (200*1000) -- Let the logging layer print out everything.
          shutdownLoggingLayer loggingLayer
  return res

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
