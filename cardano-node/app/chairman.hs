{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Control.Applicative (some)
import           Control.Exception (Exception)
import           Control.Concurrent (threadDelay)
import           Data.Text (pack)
import           Options.Applicative

import           Control.Monad.Trans.Except.Extra (runExceptT)
import           Control.Tracer (stdoutTracer)

import           Ouroboros.Network.Block (BlockNo)
import           Ouroboros.Network.NodeToClient (withIOManager)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Cardano.Config.CommonCLI
import           Cardano.Config.Protocol ( ProtocolInstantiationError
                                         , SomeProtocol(..), fromProtocol)
import           Cardano.Config.Types (ConfigYamlFilePath(..), DelegationCertFile(..),
                                       GenesisFile (..), NodeConfiguration(..),
                                       SigningKeyFile(..), SocketPath(..), parseNodeConfigurationFP)
import           Cardano.Common.Parsers
import           Cardano.Chairman (runChairman)

main :: IO ()
main = withIOManager $ \iocp -> do
    ChairmanArgs { caSecurityParam
                 , caMaxBlockNo
                 , caTimeout
                 , caTimeoutType
                 , caGenesisFile
                 , caGenesisHash
                 , caSocketPaths
                 , caConfigYaml
                 , caSigningKeyFp
                 , caDelegationCertFp
                 } <- execParser opts

    nc <- liftIO . parseNodeConfigurationFP $ unConfigPath caConfigYaml
    frmPtclRes <- runExceptT $ fromProtocol
                                 caGenesisHash
                                 (ncNodeId nc)
                                 (ncNumCoreNodes nc)
                                 (Just caGenesisFile)
                                 (ncReqNetworkMagic nc)
                                 (ncPbftSignatureThresh nc)
                                 (caDelegationCertFp)
                                 (caSigningKeyFp)
                                 (ncUpdate nc)
                                 (ncProtocol nc)

    SomeProtocol p <- case frmPtclRes of
                        Right (SomeProtocol p) -> pure (SomeProtocol p)
                        Left err -> do putTextLn $ renderPtclInstantiationErr err
                                       exitFailure

    let run = runChairman iocp
                          p
                          caSecurityParam
                          caMaxBlockNo
                          caSocketPaths
                          stdoutTracer

    case caTimeout of
      Nothing      -> run
      Just timeout ->
        run
        `race_`
        do
          threadDelay (timeout * 1_000_000)
          putTextLn $ show caTimeoutType <> " after "<> show timeout <>"seconds."
          case caTimeoutType of
            SuccessTimeout -> exitSuccess
            FailureTimeout -> exitFailure

renderPtclInstantiationErr :: ProtocolInstantiationError -> Text
renderPtclInstantiationErr = pack . show

data TimeoutType
  = SuccessTimeout
  | FailureTimeout
  deriving (Eq, Show)

data ChairmanArgs = ChairmanArgs {
      caSecurityParam   :: !SecurityParam
      -- | stop after seeing given block number
    , caMaxBlockNo      :: !(Maybe BlockNo)
      -- | timeout after given number of seconds, this is useful in combination
      -- with 'caMaxBlockNo'.  The chairman will observe only for the given
      -- period of time and then error.
      --
      -- TODO: when we'll have timeouts for 'typed-protocols' we will be able to
      -- detect progress errors when running 'chain-sync' protocol and we will
      -- be able to remove this option
    , caTimeout         :: !(Maybe Int)
    , caTimeoutType :: !TimeoutType
    , caGenesisFile :: !GenesisFile
    , caGenesisHash :: !Text
    , caSocketPaths :: ![SocketPath]
    , caConfigYaml :: !ConfigYamlFilePath
    , caSigningKeyFp :: !(Maybe SigningKeyFile)
    , caDelegationCertFp :: !(Maybe DelegationCertFile)
    }

parseSecurityParam :: Parser SecurityParam
parseSecurityParam =
    option (SecurityParam <$> auto) (
         long "security-param"
      <> short 'k'
      <> metavar "K"
      <> help "The security parameter"
    )

parseSlots :: Parser BlockNo
parseSlots =
    option ((fromIntegral :: Int -> BlockNo) <$> auto) (
         long "max-block-no"
      <> short 's'
      <> metavar "BlockNo"
      <> help "Finish after that many number of blocks"
    )

parseTimeout :: Parser Int
parseTimeout =
      option auto (
           long "timeout"
        <> short 't'
        <> metavar "Timeout"
        <> help "Timeout after given time in seconds."
      )

parseChairmanArgs :: Parser ChairmanArgs
parseChairmanArgs =
    ChairmanArgs
      <$> parseSecurityParam
      <*> optional parseSlots
      <*> optional parseTimeout
      <*> parseFlag' FailureTimeout SuccessTimeout
          "timeout-is-success" "Exit successfully on timeout."
      <*> (GenesisFile <$> parseGenesisPath)
      <*> parseGenesisHash
      <*> (some $ parseSocketPath "Path to a cardano-node socket")
      <*> (ConfigYamlFilePath <$> parseConfigFile)
      <*> (optional $ SigningKeyFile <$> parseSigningKey)
      <*> (optional $ DelegationCertFile <$> parseDelegationCert)

opts :: ParserInfo ChairmanArgs
opts = info (parseChairmanArgs <**> helper)
  ( fullDesc
  <> progDesc "Chairman Shelly application checks if Shelly nodes find consensus."
  <> header "Chairman sits in a room full of Shelley nodes, and checks if they are all behaving ...")

data Timeout = Timeout
  deriving Show

instance Exception Timeout
