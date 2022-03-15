{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Chairman.Commands.Run
  ( cmdRun
  ) where

import           Cardano.Prelude hiding (option)

import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Tracer (Tracer (..), stdoutTracer)
import qualified Data.Text as Text
import qualified Data.Time.Clock as DTC
import           Options.Applicative
import qualified System.IO as IO

import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (PartialNodeConfiguration (..),
                   parseNodeConfigurationFP)
import           Cardano.Node.Protocol
import           Cardano.Node.Types
import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Node.ProtocolInfo


import           Cardano.Api
import qualified Cardano.Api.Protocol.Types as Protocol
import           Cardano.Chairman (chairmanTest)

data RunOpts = RunOpts
  { -- | Stop the test after given number of seconds. The chairman will
    -- observe only for the given period of time, and check the consensus
    -- and progress conditions at the end.
    --
    caRunningTime :: !DiffTime
    -- | Expect this amount of progress (chain growth) by the end of the test.
  , caMinProgress :: !BlockNo
  , caSocketPaths :: ![SocketPath]
  , caConfigYaml :: !ConfigYamlFilePath
  }

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
    <> metavar "NODE-CONFIGURATION"
    <> help "Configuration file for the cardano-node"
    <> completer (bashCompleter "file")
    )

parseSocketPath :: Text -> Parser SocketPath
parseSocketPath helpMessage =
  SocketPath <$> strOption
    ( long "socket-path"
    <> help (toS helpMessage)
    <> completer (bashCompleter "file")
    <> metavar "FILEPATH"
    )

parseRunningTime :: Parser DiffTime
parseRunningTime =
  option ((fromIntegral :: Int -> DiffTime) <$> auto)
    (  long "timeout"
    <> short 't'
    <> metavar "SECONDS"
    <> help "Run the chairman for this length of time in seconds."
    )

parseProgress :: Parser BlockNo
parseProgress =
  option ((fromIntegral :: Int -> BlockNo) <$> auto)
    (  long "require-progress"
    <> short 'p'
    <> metavar "INT"
    <> help "Require this much chain-growth progress, in blocks."
  )



parseRunOpts :: Parser RunOpts
parseRunOpts =
  RunOpts
  <$> parseRunningTime
  <*> parseProgress
  <*> some (parseSocketPath "Path to a cardano-node socket")
  <*> fmap ConfigYamlFilePath parseConfigFile

run :: RunOpts -> IO ()
run RunOpts
    { caRunningTime
    , caMinProgress
    , caSocketPaths
    , caConfigYaml
    } = do

  configYamlPc <- liftIO . parseNodeConfigurationFP $ Just caConfigYaml

  ptclConfig <- case getProtocolConfiguration configYamlPc of
                  Nothing ->
                    panic $ "Node protocol configuration was not specified "<>
                            "in Config yaml filepath: " <> Text.pack (unConfigPath caConfigYaml)
                  Just ptclConfig -> return ptclConfig

  eitherSomeProtocol <- runExceptT $ mkConsensusProtocol ptclConfig Nothing

  p :: SomeConsensusProtocol <-
    case eitherSomeProtocol of
      Left err -> putStrLn (displayError err) >> exitFailure
      Right p  -> pure p

  let (k , nId) = case p of
            SomeConsensusProtocol _ runP ->
              let ProtocolInfo { pInfoConfig } = Protocol.protocolInfo runP
              in ( Consensus.configSecurityParam pInfoConfig
                 , fromNetworkMagic . getNetworkMagic $ Consensus.configBlock pInfoConfig
                 )

      consensusModeParams = getConsensusMode k ptclConfig

  chairmanTest
    (timed stdoutTracer)
    nId
    caRunningTime
    caMinProgress
    caSocketPaths
    consensusModeParams
    k

  return ()
 where
  getConsensusMode :: SecurityParam -> NodeProtocolConfiguration -> AnyConsensusModeParams
  getConsensusMode (SecurityParam k) ncProtocolConfig =
    case ncProtocolConfig of
      NodeProtocolConfigurationByron{} ->
        AnyConsensusModeParams $ ByronModeParams $ EpochSlots k
      NodeProtocolConfigurationShelley{} ->
        AnyConsensusModeParams ShelleyModeParams
      NodeProtocolConfigurationCardano{} ->
        AnyConsensusModeParams $ CardanoModeParams $ EpochSlots k

  getProtocolConfiguration
    :: PartialNodeConfiguration
    -> Maybe NodeProtocolConfiguration
  getProtocolConfiguration PartialNodeConfiguration{pncProtocolConfig} =
    getLast pncProtocolConfig

timed :: Tracer IO a -> Tracer IO a
timed (Tracer runTracer) = Tracer $ \a -> do
  ts <- DTC.getCurrentTime
  IO.putStr ("[" <> show ts <> "] ")
  runTracer a

cmdRun :: Mod CommandFields (IO ())
cmdRun = command "run"  $ flip info idm $ run <$> parseRunOpts
