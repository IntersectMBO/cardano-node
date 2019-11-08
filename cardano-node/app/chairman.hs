{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

import           Cardano.Prelude hiding (option)

import           Control.Applicative (some)
import           Control.Exception (Exception)
import           Control.Concurrent (threadDelay)
import           Options.Applicative

import           Cardano.Config.Presets (mainnetConfiguration)

import           Control.Tracer (stdoutTracer)

import           Ouroboros.Network.Block (BlockNo)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId)

import           Cardano.Config.CommonCLI
import           Cardano.Config.Protocol (Protocol, SomeProtocol(..), fromProtocol)
import           Cardano.Config.Types (CardanoConfiguration(..), ConfigYamlFilePath(..),
                                       NodeCLI(..), parseNodeConfiguration)
import           Cardano.Common.Parsers (nodeCliParser, parseCoreNodeId, parseProtocol)
import           Cardano.Chairman (runChairman)

main :: IO ()
main = do
    ChairmanArgs { caProtocol
                 , caCoreNodeIds
                 , caSecurityParam
                 , caMaxBlockNo
                 , caTimeout
                 , caCommonCLI
                 , caCommonCLIAdv
                 , caNodeCLI
                 } <- execParser opts

    cc <- case mkConfiguration mainnetConfiguration caCommonCLI caCommonCLIAdv of
      Left e -> throwIO e
      Right x -> pure x


    nc <- liftIO . parseNodeConfiguration . unConfigPath $ configFp caNodeCLI
    SomeProtocol p <- fromProtocol nc caNodeCLI caProtocol

    let run = runChairman p caCoreNodeIds
                          (NumCoreNodes $ length caCoreNodeIds)
                          caSecurityParam
                          caMaxBlockNo
                          (ccSocketDir cc)
                          stdoutTracer

    case caTimeout of
      Nothing      -> run
      Just timeout ->
        run
        `race_`
        do
          threadDelay (timeout * 1_000_000)
          throwIO Timeout


data ChairmanArgs = ChairmanArgs {
      caProtocol        :: !Protocol
    , caCoreNodeIds     :: ![CoreNodeId]
    , caSecurityParam   :: !SecurityParam
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
    , caCommonCLI       :: !CommonCLI
    , caCommonCLIAdv    :: !CommonCLIAdvanced
    , caNodeCLI         :: !(NodeCLI)
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
      <$> parseProtocol
      <*> some parseCoreNodeId
      <*> parseSecurityParam
      <*> optional parseSlots
      <*> optional parseTimeout
      <*> parseCommonCLI
      <*> parseCommonCLIAdvanced
      <*> nodeCliParser

opts :: ParserInfo ChairmanArgs
opts = info (parseChairmanArgs <**> helper)
  ( fullDesc
  <> progDesc "Chairman Shelly application checks if Shelly nodes find consensus."
  <> header "Chairman sits in a room full of Shelley nodes, and checks if they are all behaving ...")

data Timeout = Timeout
  deriving Show

instance Exception Timeout
