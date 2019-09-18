{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Prelude (String, read)

import qualified Data.IP as IP
import           Data.Semigroup ((<>))
import           Network.Socket (PortNumber)
import           Options.Applicative ( Parser, auto, flag, help, long
                                     , metavar, option, str, value
                                     )
import qualified Options.Applicative as Opt

import           Cardano.Config.Partial (PartialCardanoConfiguration (..))
import           Cardano.Config.Types (CardanoEnvironment (..))
import           Cardano.Config.Presets (mainnetConfiguration)
import           Cardano.Config.Logging (LoggingCLIArguments (..),
                                                createLoggingFeature
                                                )
import           Cardano.Node.Features.Node
import           Cardano.Prelude hiding (option)
import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..),)

import           Cardano.Config.CommonCLI
import           Cardano.Common.Help
import           Cardano.Common.Parsers
import           Cardano.Node.Run
import           Cardano.Node.Configuration.Topology (NodeAddress (..))
import           Cardano.Tracing.Tracers

main :: IO ()
main = do
    cli <- Opt.execParser opts

    (features, nodeLayer) <- initializeAllFeatures cli pcc env

    runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)

    where
      pcc :: PartialCardanoConfiguration
      pcc = mainnetConfiguration

      env :: CardanoEnvironment
      env = NoEnvironment

      cardanoApplication :: NodeLayer -> CardanoApplication
      cardanoApplication = CardanoApplication . nlRunNode

      opts :: Opt.ParserInfo CLI
      opts =
        Opt.info (cliParser
                  <**> helperBrief "help" "Show this help text" cliHelpMain
                  <**> helperBrief "help-tracing" "Show help for tracing options" cliHelpTracing
                  <**> helperBrief "help-advanced" "Show help for advanced options" cliHelpAdvanced)
          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )

      helperBrief :: String -> String -> String -> Parser (a -> a)
      helperBrief l d helpText = Opt.abortOption (Opt.InfoMsg helpText) $ mconcat
        [ Opt.long l
        , Opt.help d ]

      cliHelpMain :: String
      cliHelpMain = renderHelpDoc 80 $
        parserHelpHeader "cardano-node" cliParserMain
        <$$> ""
        <$$> parserHelpOptions cliParserMain

      cliHelpTracing :: String
      cliHelpTracing = renderHelpDoc 80 $
        "Additional tracing options:"
        <$$> ""
        <$$> parserHelpOptions cliTracingParser

      cliHelpAdvanced :: String
      cliHelpAdvanced = renderHelpDoc 80 $
        "Advanced options:"
        <$$> ""
        <$$> parserHelpOptions parseCommonCLIAdvanced

initializeAllFeatures
  :: CLI
  -> PartialCardanoConfiguration
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (CLI (CLIMain nodeCLI logCLI commonCLI) traceOpts commonCLIAdv)
                      partialConfig cardanoEnvironment = do
    -- TODO: we have to execute on our decision to implement the
    -- generalised options monoid (GOM), to serve the purposes of composition
    -- of the three config layers:  presets, config files and CLI.
    -- Currently we have a mish-mash (see createNodeFeature accepting both
    -- 'finalConfig' and nodeCli/traceCLI/advancedCLI). Yuck!
    --
    -- Considerations:
    -- 1. the CLI parser data structures must be grouped to accomodate help sectioning.
    -- 2. from #1 it follows we either switch all code users to the same structure, or
    --    we implement conversion (which will need to be maintained).
    -- 3. we want to enforce a single point where we go from GOM config layers to
    --    'CardanoConfiguration' -- so the users are not exposed to un-merged layers.
    --    This is probably the best place for this to happen.
    finalConfig <- case mkConfiguration partialConfig commonCLI commonCLIAdv of
      Left e -> throwIO e
      Right x -> pure x

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment finalConfig logCLI
    (nodeLayer   , nodeFeature)    <-
      createNodeFeature
        loggingLayer nodeCLI
        traceOpts
        cardanoEnvironment finalConfig

    pure ([ loggingFeature
          , nodeFeature
          ] :: [CardanoFeature]
         , nodeLayer)

-------------------------------------------------------------------------------
-- Parsers & Types
-------------------------------------------------------------------------------

data CLI = CLI !CLIMain !TraceOptions !CommonCLIAdvanced

data CLIMain = CLIMain !NodeArgs !LoggingCLIArguments !CommonCLI

-- | The product parser for all the CLI arguments.
cliParser :: Parser CLI
cliParser = CLI
  <$> cliParserMain
  <*> cliTracingParser
  <*> parseCommonCLIAdvanced

cliTracingParser :: Parser TraceOptions
cliTracingParser = parseTraceOptions Opt.hidden

cliParserMain :: Parser CLIMain
cliParserMain = CLIMain
  <$> parseNodeArgs
  <*> loggingParser
  <*> parseCommonCLI

parseNodeArgs :: Parser NodeArgs
parseNodeArgs =
  NodeArgs
    <$> parseTopologyInfo "PBFT node ID to assume."
    <*> parseNodeAddress
    <*> parseProtocol
    <*> parseViewMode

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

parseHostAddr :: Parser (Maybe IP.IP)
parseHostAddr =
    option (Just <$> read <$> str) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "Optionally limit node to one ipv6 or ipv4 address"
       <> value Nothing
    )

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

-- Optional flag for live view (with TUI graphics).
parseViewMode :: Parser ViewMode
parseViewMode =
    flag SimpleView LiveView $ mconcat
        [ long "live-view"
        , help "Live view with TUI."
        ]
