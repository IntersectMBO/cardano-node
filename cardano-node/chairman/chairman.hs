{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Control.Applicative (some)
import           Control.Monad.Trans.Except.Extra (runExceptT)
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Tracer (stdoutTracer)

import           Ouroboros.Network.Block (BlockNo)

import           Options.Applicative
import           Cardano.Api.Protocol.Types
                   (SomeConsensusProtocol(..), mkConsensusProtocol,
                    renderProtocolInstantiationError)
import           Cardano.Api.Config.Types
                  (ConfigYamlFilePath(..), SocketPath(..),
                   parseNodeConfigurationFP)
import           Cardano.Api.Config.Parsers
import           Cardano.Chairman (chairmanTest)

main :: IO ()
main = do
    ChairmanArgs { caRunningTime
                 , caMinProgress
                 , caSocketPaths
                 , caConfigYaml
                 } <- execParser opts

    nc <- liftIO $ parseNodeConfigurationFP caConfigYaml
    frmPtclRes <- runExceptT $ mkConsensusProtocol nc Nothing

    SomeConsensusProtocol p <- case frmPtclRes of
                        Right p  -> pure p
                        Left err -> do putTextLn $ renderProtocolInstantiationError err
                                       exitFailure

    chairmanTest
      stdoutTracer
      p
      caRunningTime
      caMinProgress
      caSocketPaths

data ChairmanArgs = ChairmanArgs {
      -- | Stop the test after given number of seconds. The chairman will
      -- observe only for the given period of time, and check the consensus
      -- and progress conditions at the end.
      --
      caRunningTime :: !DiffTime
      -- | Expect this amount of progress (chain growth) by the end of the test.
    , caMinProgress :: !(Maybe BlockNo)
    , caSocketPaths :: ![SocketPath]
    , caConfigYaml :: !ConfigYamlFilePath
    }

parseRunningTime :: Parser DiffTime
parseRunningTime =
      option ((fromIntegral :: Int -> DiffTime) <$> auto) (
           long "timeout"
        <> short 't'
        <> metavar "Time"
        <> help "Run the chairman for this length of time in seconds."
      )

parseProgress :: Parser BlockNo
parseProgress =
    option ((fromIntegral :: Int -> BlockNo) <$> auto) (
         long "require-progress"
      <> short 'p'
      <> metavar "Blocks"
      <> help "Require this much chain-growth progress, in blocks."
    )

parseChairmanArgs :: Parser ChairmanArgs
parseChairmanArgs =
    ChairmanArgs
      <$> parseRunningTime
      <*> optional parseProgress
      <*> (some $ parseSocketPath "Path to a cardano-node socket")
      <*> (ConfigYamlFilePath <$> parseConfigFile)

opts :: ParserInfo ChairmanArgs
opts = info (parseChairmanArgs <**> helper)
  ( fullDesc
  <> progDesc "Chairman Shelley application is a CI tool which checks \
              \if Shelly nodes find consensus and do expected progress."
  <> header "Chairman sits in a room full of Shelley nodes, and checks \
            \if they are all behaving ...")
