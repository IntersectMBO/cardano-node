module Cardano.Node.Parsers
  ( loggingParser
  , parseCoreNodeId
  , parseProtocol
  ) where


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
-- | The parser for the logging specific arguments.
loggingParser :: Parser LoggingCLIArguments
loggingParser = LoggingCLIArguments
    <$> strOption
        ( long "log-config"
       <> metavar "LOGCONFIG"
       <> help "Configuration file for logging"
       <> completer (bashCompleter "file")
        )

parseProtocol :: Parser Protocol
parseProtocol = asum [
      flag' BFT $ mconcat [
          long "bft"
        , help "Use the BFT consensus algorithm"
        ]
    , flag' Praos $ mconcat [
          long "praos"
        , help "Use the Praos consensus algorithm"
        ]
    , flag' MockPBFT $ mconcat [
          long "mock-pbft"
        , help "Use the Permissive BFT consensus algorithm using a mock ledger"
        ]
    , flag' RealPBFT $ mconcat [
          long "real-pbft"
        , help "Use the Permissive BFT consensus algorithm using the real ledger"
        ]
    ]
