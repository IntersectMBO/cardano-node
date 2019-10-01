{-# LANGUAGE RankNTypes           #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Common.Parsers
  ( loggingParser
  , parseCoreNodeId
  , parseProtocol
  , parseProtocolActual
  , parseProtocolAsCommand
  , parseTopologyInfo
  , parseTraceOptions
  ) where


import           Prelude (String)

import           Cardano.Prelude hiding (option)

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import           Cardano.Config.Logging (LoggingCLIArguments(..))
import           Ouroboros.Consensus.NodeId (NodeId(..), CoreNodeId(..))
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers'(..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus

import qualified Cardano.BM.Data.Severity as Log
import           Cardano.Config.Logging
import           Cardano.Common.Protocol
import           Cardano.Node.Configuration.Topology

-- Common command line parsers

parseCoreNodeId :: Parser CoreNodeId
parseCoreNodeId =
    option (fmap CoreNodeId auto) (
            long "core-node-id"
         <> metavar "CORE-NODE-ID"
         <> help "The ID of the core node to which this client is connected."
    )

parseNodeId :: String -> Parser NodeId
parseNodeId desc =
    option (fmap CoreId auto) (
            long "node-id"
         <> metavar "NODE-ID"
         <> help desc
    )

-- | Flag parser, that returns its argument on success.
fl :: a -> String -> String -> Parser a
fl val opt desc = flag' val $ mconcat [long opt, help desc]

parseProtocol :: Parser Protocol
parseProtocol = asum
  [ fl ByronLegacy "byron-legacy"
    "Byron/Ouroboros Classic suite of algorithms"

  , fl BFT "bft"
    "BFT consensus"

  , fl Praos "praos"
    "Praos consensus"

  , fl MockPBFT "mock-pbft"
    "Permissive BFT consensus with a mock ledger"

  , fl RealPBFT "real-pbft"
    "Permissive BFT consensus with a real ledger"
  ]

parseProtocolActual :: Parser Protocol
parseProtocolActual = asum
  [ fl ByronLegacy "byron-legacy"
    "Byron/Ouroboros Classic suite of algorithms"

  , fl RealPBFT "real-pbft"
    "Permissive BFT consensus with a real ledger"
  ]

parseProtocolAsCommand :: Parser Protocol
parseProtocolAsCommand = subparser $ mconcat
  [ commandGroup "System version"
  , metavar "SYSTEMVER"
  , cmd "byron-legacy" "Byron Legacy mode" $ pure ByronLegacy
  , cmd "bft"          "BFT mode"          $ pure BFT
  , cmd "praos"        "Praos mode"        $ pure Praos
  , cmd "mock-pbft"    "Mock PBFT mode"    $ pure MockPBFT
  , cmd "real-pbft"    "Real PBFT mode"    $ pure RealPBFT
  ]

  where
    cmd :: forall a. String -> String -> Parser a -> Mod CommandFields a
    cmd c desc p = command c $ info (p <**> helper) $ mconcat [ progDesc desc ]

parseTopologyInfo :: String -> Parser TopologyInfo
parseTopologyInfo desc = TopologyInfo <$> parseNodeId desc <*> parseTopologyFile

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )

-- | A parser disables logging if --log-config is not supplied.
loggingParser :: Parser LoggingCLIArguments
loggingParser =
  fromMaybe muteLoggingCLIArguments
    <$> optional parseLoggingCLIArgumentsInternal
  where
    parseLoggingCLIArgumentsInternal :: Parser LoggingCLIArguments
    parseLoggingCLIArgumentsInternal =
      LoggingCLIArguments
        <$> (Just
             <$> strOption
              ( long "log-config"
                <> metavar "LOGCONFIG"
                <> help "Configuration file for logging"
                <> completer (bashCompleter "file")))
        <*> option auto
         ( long "log-min-severity"
           <> metavar "SEVERITY"
           <> help "Limit logging to items with severity at least this severity"
           <> value Log.Info
           <> showDefault)
        <*> switch
         ( long "log-metrics"
           <> help "Log a number of metrics about this node")

    -- This is the value returned by the parser, when --log-config is omitted.
    muteLoggingCLIArguments :: LoggingCLIArguments
    muteLoggingCLIArguments =
      LoggingCLIArguments
      Nothing
      Log.Emergency
      False

-- | The parser for the logging specific arguments.
parseTraceOptions :: MParser TraceOptions
parseTraceOptions m = TraceOptions
  <$> parseTracingVerbosity m
  <*> parseTraceChainDB m
  <*> parseConsensusTraceOptions m
  <*> parseProtocolTraceOptions m
  <*> parseTraceIpSubscription m
  <*> parseTraceDnsSubscription m
  <*> parseTraceDnsResolver m
  <*> parseTraceMux m

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
