module Cardano.Node.Parsers
  ( loggingParser
  , parseCoreNodeId
  , parseProtocol
  ) where


import           Prelude (String, error, id)
import           Cardano.Prelude hiding (option)

import           Cardano.Common.Protocol (Protocol(..))
import           Cardano.Node.Features.Logging (LoggingCLIArguments(..))
import           Ouroboros.Consensus.NodeId (CoreNodeId(..))

import           Options.Applicative

-- Common command line parsers

parseCoreNodeId :: Parser CoreNodeId
parseCoreNodeId =
    option (fmap CoreNodeId auto) (
            long "core-node-id"
         <> short 'n'
         <> metavar "CORE-NODE-ID"
         <> help "The ID of the core node to which this client is connected."
    )

parseNodeId :: String -> Parser NodeId
parseNodeId desc =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help desc
    )

parseTopologyInfo :: String -> Parser TopologyInfo
parseTopologyInfo desc = TopologyInfo <$> parseNodeId desc <*> parseTopologyFile

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )

-- | The parser for the logging specific arguments.
loggingParser :: Parser LoggingCLIArguments
loggingParser = LoggingCLIArguments
    <$> strOption
        ( long "log-config"
       <> metavar "LOGCONFIG"
       <> help "Configuration file for logging"
       <> completer (bashCompleter "file")
        )

parseLovelace :: String -> String -> Parser Lovelace
parseLovelace optname desc =
  either (error . show) id . mkLovelace
  <$> parseIntegral optname desc

parseLovelacePortion :: String -> String -> Parser LovelacePortion
parseLovelacePortion optname desc =
  either (error . show) id . mkLovelacePortion
  <$> parseIntegral optname desc

parseFakeAvvmOptions :: Parser Genesis.FakeAvvmOptions
parseFakeAvvmOptions =
  Genesis.FakeAvvmOptions
  <$> parseIntegral        "avvm-entry-count"         "Number of AVVM addresses."
  <*> parseLovelace        "avvm-entry-balance"       "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
  <$> parseIntegral        "k"                        "The security parameter of the Ouroboros protocol."

parseProtocolMagicId :: String -> Parser ProtocolMagicId
parseProtocolMagicId arg =
  ProtocolMagicId
  <$> parseIntegral        arg                        "The magic number unique to any instance of Cardano."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated ()
  <$> parseProtocolMagicId "protocol-magic"

parseNetworkMagic :: Parser NetworkMagic
parseNetworkMagic = asum
    [ flag' NetworkMainOrStage $ mconcat [
          long "main-or-staging"
        , help ""
        ]
    , option (fmap NetworkTestnet auto) (
          long "testnet-magic"
       <> metavar "MAGIC"
       <> help "The testnet network magic, decibal"
        )
    ]
