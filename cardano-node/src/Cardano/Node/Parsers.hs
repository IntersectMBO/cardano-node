module Cardano.Node.Parsers
  ( loggingParser
  , parseCoreNodeId
  , parseNodeId
  , parseProtocol
  , parseViewMode
  ) where


import           Cardano.Prelude hiding (option)

import           Cardano.Node.CLI (Protocol(..), ViewMode(..))
import           Cardano.Node.Features.Logging (LoggingCLIArguments(..))
import           Ouroboros.Consensus.NodeId (CoreNodeId(..), NodeId(..))

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
        )

parseNodeId :: Parser NodeId
parseNodeId =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node"
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

-- Optional flag for live view (with TUI graphics).
parseViewMode :: Parser ViewMode
parseViewMode =
    flag SimpleView LiveView $ mconcat
        [ long "live-view"
        , help "Live view with TUI."
        ]
