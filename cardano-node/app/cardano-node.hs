{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Prelude (read)

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
import           Cardano.Common.Parsers
import           Cardano.Node.Run
import           Cardano.Node.Configuration.Topology (NodeAddress (..))
import           Cardano.Tracing.Tracers (ConsensusTraceOptions,  ProtocolTraceOptions, TraceOptions(..))

main :: IO ()
main = do
    cliArgs <- Opt.customExecParser pref opts

    (features, nodeLayer) <- initializeAllFeatures cliArgs pcc env

    runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)

    where
      pcc :: PartialCardanoConfiguration
      pcc = mainnetConfiguration

      env :: CardanoEnvironment
      env = NoEnvironment

      cardanoApplication :: NodeLayer -> CardanoApplication
      cardanoApplication = CardanoApplication . nlRunNode

      pref :: Opt.ParserPrefs
      pref = Opt.prefs Opt.showHelpOnEmpty

      opts :: Opt.ParserInfo CLIArguments
      opts =
        Opt.info (commandLineParser <**> Opt.helper)
          ( Opt.fullDesc <>
            Opt.progDesc "Cardano node."
          )

initializeAllFeatures
  :: CLIArguments
  -> PartialCardanoConfiguration
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (CLIArguments logCLI nodeCLI commonCLI) partialConfig cardanoEnvironment = do
    finalConfig <- mkConfiguration partialConfig commonCLI

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment finalConfig logCLI
    (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer nodeCLI cardanoEnvironment finalConfig

    pure ([ loggingFeature
          , nodeFeature
          ] :: [CardanoFeature]
         , nodeLayer)

-------------------------------------------------------------------------------
-- Parsers & Types
-------------------------------------------------------------------------------

data CLIArguments = CLIArguments !LoggingCLIArguments !NodeArgs !CommonCLI

-- | The product parser for all the CLI arguments.
commandLineParser :: Parser CLIArguments
commandLineParser = CLIArguments
    <$> loggingParser
    <*> parseNodeArgs
    <*> parseCommonCLI

parseNodeArgs :: Parser NodeArgs
parseNodeArgs =
  NodeArgs
    <$> parseTopologyInfo "PBFT node ID to assume."
    <*> parseNodeAddress
    <*> parseProtocol
    <*> parseViewMode
    <*> parseTraceOptions

parseTraceBlockFetchClient :: Parser Bool
parseTraceBlockFetchClient  =
    switch (
         long "trace-block-fetch-client"
      <> help "Trace BlockFetch client."
    )

parseTraceBlockFetchServer :: Parser Bool
parseTraceBlockFetchServer  =
    switch (
         long "trace-block-fetch-server"
      <> help "Trace BlockFetch server."
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

parseTraceOptions :: Parser TraceOptions
parseTraceOptions = TraceOptions
  <$> parseTracingGlobal
  <*> parseTracingVerbosity
  <*> parseTraceChainDB
  <*> parseConsensusTraceOptions
  <*> parseProtocolTraceOptions
  <*> parseTraceIpSubscription
  <*> parseTraceDnsSubscription
  <*> parseTraceDnsResolver

parseTracingGlobal :: Parser Bool
parseTracingGlobal =
    switch (
         long "tracing-off"
      <> help "Tracing globally turned off."
    )

parseTracingVerbosity :: Parser TracingVerbosity
parseTracingVerbosity = asum [
    flag' MinimalVerbosity (long "tracing-verbosity-minimal"
            <> help "Minimal level of the rendering of captured items")
    <|>
    flag' MaximalVerbosity (long "tracing-verbosity-maximal"
            <> help "Maximal level of the rendering of captured items")
    <|>
    flag NormalVerbosity NormalVerbosity (long "tracing-verbosity-normal"
            <> help "the default level of the rendering of captured items")
    ]

parseTraceChainDB :: Parser Bool
parseTraceChainDB =
    switch (
         long "trace-chain-db"
      <> help "Verbose tracer of ChainDB."
    )

parseConsensusTraceOptions :: Parser ConsensusTraceOptions
parseConsensusTraceOptions = Consensus.Tracers
  <$> (Const <$> parseTraceChainSyncClient)
  <*> (Const <$> parseTraceChainSyncHeaderServer)
  <*> (Const <$> parseTraceChainSyncBlockServer)
  <*> (Const <$> parseTraceBlockFetchDecisions)
  <*> (Const <$> parseTraceBlockFetchClient)
  <*> (Const <$> parseTraceBlockFetchServer)
  <*> (Const <$> parseTraceTxInbound)
  <*> (Const <$> parseTraceTxOutbound)
  <*> (Const <$> parseTraceLocalTxSubmissionServer)
  <*> (Const <$> parseTraceMempool)
  <*> (Const <$> parseTraceForge)

parseTraceBlockFetchDecisions :: Parser Bool
parseTraceBlockFetchDecisions =
    switch (
         long "trace-block-fetch-decisions"
      <> help "Trace BlockFetch decisions made by the BlockFetch client."
    )

parseTraceChainSyncClient :: Parser Bool
parseTraceChainSyncClient  =
    switch (
         long "trace-chain-sync-client"
      <> help "Trace ChainSync client."
    )

parseTraceChainSyncBlockServer :: Parser Bool
parseTraceChainSyncBlockServer  =
    switch (
         long "trace-chain-sync-block-server"
      <> help "Trace ChainSync server (blocks)."
    )

parseTraceChainSyncHeaderServer :: Parser Bool
parseTraceChainSyncHeaderServer  =
    switch (
         long "trace-chain-sync-header-server"
      <> help "Trace ChainSync server (headers)."
    )

parseTraceTxInbound :: Parser Bool
parseTraceTxInbound =
    switch (
         long "trace-tx-inbound"
      <> help "Trace TxSubmission server (inbound transactions)."
    )

parseTraceTxOutbound :: Parser Bool
parseTraceTxOutbound =
    switch (
         long "trace-tx-outbound"
      <> help "Trace TxSubmission client (outbound transactions)."
    )

parseTraceLocalTxSubmissionServer :: Parser Bool
parseTraceLocalTxSubmissionServer =
    switch (
         long "trace-local-tx-submission-server"
      <> help "Trace local TxSubmission server."
    )

parseTraceMempool :: Parser Bool
parseTraceMempool =
    switch (
         long "trace-mempool"
      <> help "Trace mempool."
    )

parseTraceForge :: Parser Bool
parseTraceForge =
    switch (
         long "trace-forge"
      <> help "Trace block forging."
    )

parseTraceChainSyncProtocol :: Parser Bool
parseTraceChainSyncProtocol =
    switch (
         long "trace-chain-sync-protocol"
      <> help "Trace ChainSync protocol messages."
    )

parseTraceBlockFetchProtocol :: Parser Bool
parseTraceBlockFetchProtocol =
    switch (
         long "trace-block-fetch-protocol"
      <> help "Trace BlockFetch protocol messages."
    )

parseTraceTxSubmissionProtocol :: Parser Bool
parseTraceTxSubmissionProtocol =
    switch (
         long "trace-tx-submission-protocol"
      <> help "Trace TxSubmission protocol messages."
    )

parseTraceLocalChainSyncProtocol :: Parser Bool
parseTraceLocalChainSyncProtocol =
    switch (
         long "trace-local-chain-sync-protocol"
      <> help "Trace local ChainSync protocol messages."
    )

parseTraceLocalTxSubmissionProtocol :: Parser Bool
parseTraceLocalTxSubmissionProtocol =
    switch (
         long "trace-local-tx-submission-protocol"
      <> help "Trace local TxSubmission protocol messages."
    )


parseProtocolTraceOptions :: Parser ProtocolTraceOptions
parseProtocolTraceOptions = ProtocolTracers
  <$> (Const <$> parseTraceChainSyncProtocol)
  <*> (Const <$> parseTraceBlockFetchProtocol)
  <*> (Const <$> parseTraceTxSubmissionProtocol)
  <*> (Const <$> parseTraceLocalChainSyncProtocol)
  <*> (Const <$> parseTraceLocalTxSubmissionProtocol)

parseTraceIpSubscription :: Parser Bool
parseTraceIpSubscription =
    switch (
         long "trace-ip-subscription"
      <> help "Trace IP Subscription messages."
    )

parseTraceDnsSubscription :: Parser Bool
parseTraceDnsSubscription =
    switch (
         long "trace-dns-subscription"
      <> help "Trace DNS Subscription messages."
    )

parseTraceDnsResolver :: Parser Bool
parseTraceDnsResolver =
    switch (
         long "trace-dns-resolver"
      <> help "Trace DNS Resolver messages."
    )

-- Optional flag for live view (with TUI graphics).
parseViewMode :: Parser ViewMode
parseViewMode =
    flag SimpleView LiveView $ mconcat
        [ long "live-view"
        , help "Live view with TUI."
        ]
