{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Data.Text (pack)
import           Control.Applicative (some)
import           Control.Monad.Trans.Except.Extra (runExceptT)
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Exception (Exception)
import           Control.Tracer (stdoutTracer)

import           Ouroboros.Network.Block (BlockNo)

import           Options.Applicative
import           Cardano.Config.CommonCLI
import           Cardano.Config.Protocol ( ProtocolInstantiationError
                                         , SomeProtocol(..), fromProtocol)
import           Cardano.Config.Types (ConfigYamlFilePath(..), DelegationCertFile(..),
                                       NodeConfiguration(..), SigningKeyFile(..),
                                       SocketPath(..), parseNodeConfigurationFP)
import           Cardano.Common.Parsers
import           Cardano.Chairman (chairmanTest)

main :: IO ()
main = do
    ChairmanArgs { caMaxBlockNo
                 , caTimeout
                 , caSocketPaths
                 , caConfigYaml
                 , caSigningKeyFp
                 , caDelegationCertFp
                 } <- execParser opts

    nc <- liftIO $ parseNodeConfigurationFP caConfigYaml
    frmPtclRes <- runExceptT $ fromProtocol
                                 (ncNodeId nc)
                                 (ncNumCoreNodes nc)
                                 (Just $ ncGenesisFile nc)
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

    chairmanTest
      stdoutTracer
      p
      caMaxBlockNo
      caTimeout
      caSocketPaths

renderPtclInstantiationErr :: ProtocolInstantiationError -> Text
renderPtclInstantiationErr = pack . show

data TimeoutType
  = SuccessTimeout
  | FailureTimeout
  deriving (Eq, Show)

data ChairmanArgs = ChairmanArgs {
      -- | stop after seeing given block number
      caMaxBlockNo      :: !(Maybe BlockNo)
      -- | timeout after given number of seconds, this is useful in combination
      -- with 'caMaxBlockNo'.  The chairman will observe only for the given
      -- period of time and then error.
      --
      -- TODO: when we'll have timeouts for 'typed-protocols' we will be able to
      -- detect progress errors when running 'chain-sync' protocol and we will
      -- be able to remove this option
    , caTimeout     :: !(Maybe DiffTime)
    , caSocketPaths :: ![SocketPath]
    , caConfigYaml :: !ConfigYamlFilePath
    , caSigningKeyFp :: !(Maybe SigningKeyFile)
    , caDelegationCertFp :: !(Maybe DelegationCertFile)
    }

parseSlots :: Parser BlockNo
parseSlots =
    option ((fromIntegral :: Int -> BlockNo) <$> auto) (
         long "max-block-no"
      <> short 's'
      <> metavar "BlockNo"
      <> help "Finish after that many number of blocks"
    )

parseTimeout :: Parser DiffTime
parseTimeout =
      option ((fromIntegral :: Int -> DiffTime) <$> auto) (
           long "timeout"
        <> short 't'
        <> metavar "Timeout"
        <> help "Timeout after given time in seconds."
      )

parseChairmanArgs :: Parser ChairmanArgs
parseChairmanArgs =
    ChairmanArgs
      <$> optional parseSlots
      <*> optional parseTimeout
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
