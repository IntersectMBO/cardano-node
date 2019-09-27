{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Prelude (String, read)

import           Data.Semigroup ((<>))
import qualified Data.IP as IP
import           Network.Socket (PortNumber)
import           Options.Applicative ( Parser, auto, flag, flag', help, long
                                     , metavar, option, str, switch
                                     )
import qualified Options.Applicative as Opt

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
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
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers'(..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus

import           Cardano.Config.CommonCLI
import           Cardano.Common.Help
import           Cardano.Common.Parsers
import           Cardano.Node.Run
import           Cardano.Node.Configuration.Topology (NodeAddress (..))
import           Cardano.Tracing.Tracers (ConsensusTraceOptions,  ProtocolTraceOptions, TraceOptions(..))

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
initializeAllFeatures (CLI (CLIMain nodeCLI logCLI commonCLI) traceCLI commonCLIAdv)
                      partialConfig cardanoEnvironment = do
    -- TODO: we have to execute on our decision to implement the
    -- generalised options monoid (GOM), to serve the purposes of composition
    -- of the three config layers:  presets, config files and CLI.
    -- Currently we have a mish-mash (see createNodeFeature accepting both
    -- 'finalConfig' and nodeCli/traceCLI/advancedCLI. Yuck!
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
    (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer nodeCLI traceCLI cardanoEnvironment finalConfig

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

cliParserMain :: Parser CLIMain
cliParserMain = CLIMain
  <$> parseNodeArgs
  <*> loggingParser
  <*> parseCommonCLI

cliTracingParser :: Parser TraceOptions
cliTracingParser = parseTraceOptions Opt.hidden

parseNodeArgs :: Parser NodeArgs
parseNodeArgs =
  NodeArgs
    <$> parseTopologyInfo "PBFT node ID to assume."
    <*> parseNodeAddress
    <*> parseProtocol
    <*> parseViewMode

parseTraceBlockFetchClient :: MParser Bool
parseTraceBlockFetchClient m =
    switch (
         long "trace-block-fetch-client"
      <> help "Trace BlockFetch client."
      <> m
    )

parseTraceBlockFetchServer :: MParser Bool
parseTraceBlockFetchServer m =
    switch (
         long "trace-block-fetch-server"
      <> help "Trace BlockFetch server."
      <> m
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

parseHostAddr :: Parser IP.IP
parseHostAddr =
    option (read <$> str) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "The ipv6 or ipv4 address"
    )

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

parseTraceOptions :: MParser TraceOptions
parseTraceOptions m = TraceOptions
  <$> parseTracingGlobal m
  <*> parseTracingVerbosity m
  <*> parseTraceChainDB m
  <*> parseConsensusTraceOptions m
  <*> parseProtocolTraceOptions m
  <*> parseTraceIpSubscription m
  <*> parseTraceDnsSubscription m
  <*> parseTraceDnsResolver m
  <*> parseTraceMux m

parseTracingGlobal :: MParser Bool
parseTracingGlobal m =
  switch ( long "tracing-off"
           <> help "Tracing globally turned off."
           <> m
         )

parseTracingVerbosity :: MParser TracingVerbosity
parseTracingVerbosity m = asum [
  flag' MinimalVerbosity (
      long "tracing-verbosity-minimal"
        <> help "Minimal level of the rendering of captured items"
        <> m)
    <|>
  flag' MaximalVerbosity ( 
      long "tracing-verbosity-maximal"
        <> help "Maximal level of the rendering of captured items"
        <> m)
    <|>
  flag NormalVerbosity NormalVerbosity (
      long "tracing-verbosity-normal"
        <> help "the default level of the rendering of captured items"
        <> m)
  ]

parseTraceChainDB :: MParser Bool
parseTraceChainDB m =
    switch (
         long "trace-chain-db"
      <> help "Verbose tracer of ChainDB."
      <> m
    )

parseConsensusTraceOptions :: (forall a b. Opt.Mod a b) -> Parser ConsensusTraceOptions
parseConsensusTraceOptions m = Consensus.Tracers
  <$> (Const <$> parseTraceChainSyncClient m)
  <*> (Const <$> parseTraceChainSyncHeaderServer m)
  <*> (Const <$> parseTraceChainSyncBlockServer m)
  <*> (Const <$> parseTraceBlockFetchDecisions m)
  <*> (Const <$> parseTraceBlockFetchClient m)
  <*> (Const <$> parseTraceBlockFetchServer m)
  <*> (Const <$> parseTraceTxInbound m)
  <*> (Const <$> parseTraceTxOutbound m)
  <*> (Const <$> parseTraceLocalTxSubmissionServer m)
  <*> (Const <$> parseTraceMempool m)
  <*> (Const <$> parseTraceForge m)

type MParser a = (forall b c. Opt.Mod b c) -> Parser a

parseTraceBlockFetchDecisions :: MParser Bool
parseTraceBlockFetchDecisions m =
    switch (
         long "trace-block-fetch-decisions"
      <> help "Trace BlockFetch decisions made by the BlockFetch client."
      <> m
    )

parseTraceChainSyncClient :: MParser Bool
parseTraceChainSyncClient m =
    switch (
         long "trace-chain-sync-client"
      <> help "Trace ChainSync client."
      <> m
    )

parseTraceChainSyncBlockServer :: MParser Bool
parseTraceChainSyncBlockServer m =
    switch (
         long "trace-chain-sync-block-server"
      <> help "Trace ChainSync server (blocks)."
      <> m
    )

parseTraceChainSyncHeaderServer :: MParser Bool
parseTraceChainSyncHeaderServer m =
    switch (
         long "trace-chain-sync-header-server"
      <> help "Trace ChainSync server (headers)."
      <> m
    )

parseTraceTxInbound :: MParser Bool
parseTraceTxInbound m =
    switch (
         long "trace-tx-inbound"
      <> help "Trace TxSubmission server (inbound transactions)."
      <> m
    )

parseTraceTxOutbound :: MParser Bool
parseTraceTxOutbound m =
    switch (
         long "trace-tx-outbound"
      <> help "Trace TxSubmission client (outbound transactions)."
      <> m
    )

parseTraceLocalTxSubmissionServer :: MParser Bool
parseTraceLocalTxSubmissionServer m =
    switch (
         long "trace-local-tx-submission-server"
      <> help "Trace local TxSubmission server."
      <> m
    )

parseTraceMempool :: MParser Bool
parseTraceMempool m =
    switch (
         long "trace-mempool"
      <> help "Trace mempool."
      <> m
    )

parseTraceForge :: MParser Bool
parseTraceForge m =
    switch (
         long "trace-forge"
      <> help "Trace block forging."
      <> m
    )

parseTraceChainSyncProtocol :: MParser Bool
parseTraceChainSyncProtocol m =
    switch (
         long "trace-chain-sync-protocol"
      <> help "Trace ChainSync protocol messages."
      <> m
    )

parseTraceBlockFetchProtocol :: MParser Bool
parseTraceBlockFetchProtocol m =
    switch (
         long "trace-block-fetch-protocol"
      <> help "Trace BlockFetch protocol messages."
      <> m
    )

parseTraceTxSubmissionProtocol :: MParser Bool
parseTraceTxSubmissionProtocol m =
    switch (
         long "trace-tx-submission-protocol"
      <> help "Trace TxSubmission protocol messages."
      <> m
    )

parseTraceLocalChainSyncProtocol :: MParser Bool
parseTraceLocalChainSyncProtocol m =
    switch (
         long "trace-local-chain-sync-protocol"
      <> help "Trace local ChainSync protocol messages."
      <> m
    )

parseTraceLocalTxSubmissionProtocol :: MParser Bool
parseTraceLocalTxSubmissionProtocol m =
    switch (
         long "trace-local-tx-submission-protocol"
      <> help "Trace local TxSubmission protocol messages."
      <> m
    )


parseProtocolTraceOptions :: MParser ProtocolTraceOptions
parseProtocolTraceOptions m = ProtocolTracers
  <$> (Const <$> parseTraceChainSyncProtocol m)
  <*> (Const <$> parseTraceBlockFetchProtocol m)
  <*> (Const <$> parseTraceTxSubmissionProtocol m)
  <*> (Const <$> parseTraceLocalChainSyncProtocol m)
  <*> (Const <$> parseTraceLocalTxSubmissionProtocol m)

parseTraceIpSubscription :: MParser Bool
parseTraceIpSubscription m =
    switch (
         long "trace-ip-subscription"
      <> help "Trace IP Subscription messages."
      <> m
    )

parseTraceDnsSubscription :: MParser Bool
parseTraceDnsSubscription m =
    switch (
         long "trace-dns-subscription"
      <> help "Trace DNS Subscription messages."
      <> m
    )

parseTraceDnsResolver :: MParser Bool
parseTraceDnsResolver m =
    switch (
         long "trace-dns-resolver"
      <> help "Trace DNS Resolver messages."
      <> m
    )

parseTraceMux :: MParser Bool
parseTraceMux m =
    switch (
         long "trace-mux"
      <> help "Trace Mux Events"
      <> m
    )

-- Optional flag for live view (with TUI graphics).
parseViewMode :: Parser ViewMode
parseViewMode =
    flag SimpleView LiveView $ mconcat
        [ long "live-view"
        , help "Live view with TUI."
        ]
