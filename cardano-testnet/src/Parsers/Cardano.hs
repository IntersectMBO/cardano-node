{-# LANGUAGE ScopedTypeVariables #-}

module Parsers.Cardano
  ( cmdCardano
  , cmdCreateEnv
  , parseNodeSpecs
  ) where

import           Cardano.Api (AnyShelleyBasedEra (..))

import           Cardano.CLI.EraBased.Common.Option hiding (pNetworkId)
import           Cardano.Prelude (readMaybe)

import           Prelude

import           Control.Applicative (optional, (<|>))
import           Control.Monad (unless)
import           Data.Default.Class (def)
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe
import           Data.Word (Word64)
import           Options.Applicative (CommandFields, Mod, Parser)
import qualified Options.Applicative as OA
import           Options.Applicative.Types (readerAsk)
import           Text.Parsec (char, many1, noneOf,
                     sepBy1, string, try, (<?>), parse, eof, notFollowedBy)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec

import           Testnet.Defaults (defaultEra)
import           Testnet.Start.Cardano
import           Testnet.Start.Types

data ModeOptions
  = ModeFromEnv TestnetEnvOptions
  | ModeNewEnv TestnetCreationOptions (Maybe FilePath)

optsTestnet :: Parser CardanoTestnetCliOptions
optsTestnet = mkCliOptions <$> pModeOptions <*> pRuntimeOptions
  where
    pModeOptions =
          ModeFromEnv <$> pFromEnv
      <|> ModeNewEnv <$> pCreationOptions <*> pScratchOutputDir
    mkCliOptions (ModeFromEnv envOpts) rt = StartFromEnv (StartFromEnvOptions envOpts rt)
    mkCliOptions (ModeNewEnv creation outDir) rt = NoUserProvidedEnv (NoUserProvidedEnvOptions creation outDir rt)

optsCreateTestnet :: Parser CardanoTestnetCreateEnvOptions
optsCreateTestnet = CardanoTestnetCreateEnvOptions
  <$> pCreationOptions
  <*> pEnvOutputDir

pFromEnv :: Parser TestnetEnvOptions
pFromEnv = TestnetEnvOptions
  <$> OA.strOption
      (  OA.long "node-env"
      <> OA.metavar "FILEPATH"
      <> OA.help "Path to the node's environment (which is generated otherwise). You can generate a default environment with the 'create-env' command, then modify it and pass it with this argument."
      )
  <*> pUpdateTimestamps

pCreationOptions :: Parser TestnetCreationOptions
pCreationOptions = TestnetCreationOptions
  <$> pTestnetNodesWithOptions
  <*> pure (AnyShelleyBasedEra defaultEra)
  <*> pMaxLovelaceSupply
  <*> pNumDReps
  <*> pGenesisOptions
  <*> pOnChainParams

pRuntimeOptions :: Parser TestnetRuntimeOptions
pRuntimeOptions = TestnetRuntimeOptions
  <$> pEnableNewEpochStateLogging
  <*> pEnableRpc
  <*> pKesSource

pScratchOutputDir :: Parser (Maybe FilePath)
pScratchOutputDir = optional $ OA.strOption
  (   OA.long "output-dir"
  <>  OA.help "Directory where to store files, sockets, and so on. It is created if it doesn't exist. If unset, a temporary directory is used."
  <>  OA.metavar "DIRECTORY"
  )

pNumDReps :: Parser NumDReps
pNumDReps = OA.option OA.auto
  (   OA.long "num-dreps"
  <>  OA.help "Number of delegate representatives (DReps) to generate."
  <>  OA.metavar "NUMBER"
  <>  OA.showDefault
  <>  OA.value 3
  )

pEnableNewEpochStateLogging :: Parser Bool
pEnableNewEpochStateLogging = OA.switch
  (   OA.long "enable-new-epoch-state-logging"
  <>  OA.help "Enable new epoch state logging to logs/ledger-epoch-state.log"
  <>  OA.showDefault
  )

pEnableRpc :: Parser RpcSupport
pEnableRpc = OA.flag RpcDisabled RpcEnabled
  (   OA.long "enable-grpc"
  <>  OA.help "[EXPERIMENTAL] Enable gRPC endpoint on all of testnet nodes. The listening socket file will be the same directory as node's N2C socket."
  <>  OA.showDefault
  )

pKesSource :: Parser PraosCredentialsSource
pKesSource = OA.flag UseKesKeyFile UseKesSocket
  (   OA.long "use-kes-agent"
  <>  OA.help "Get Praos block forging credentials from kes-agent via the default socket path"
  <>  OA.showDefault
  )

pTestnetNodesWithOptions :: Parser TestnetNodesWithOptions
pTestnetNodesWithOptions =
  pNodes <|> pNumPoolNodes <|> pure cardanoDefaultTestnetNodesWithOptions
  where
    pNumPoolNodes :: Parser TestnetNodesWithOptions
    pNumPoolNodes =
      (\num -> TestnetNodesWithOptions { optSpoNodes = defaultSpoOption :| L.replicate (num - 1) defaultSpoOption, optRelayNodes = [] }) <$>
        OA.option ensureAtLeastOne
          (   OA.long "num-pool-nodes"
          <>  OA.help "Number of pool nodes. Note this uses a default node configuration for all nodes."
          <>  OA.metavar "COUNT"
          )
    defaultSpoOption = NodeWithOptions Nothing []

    ensureAtLeastOne :: OA.ReadM Int
    ensureAtLeastOne = readerAsk >>= \arg ->
      case readMaybe arg of
        Just n | n >= 1 -> pure n
        _ -> fail "Need at least one SPO node to produce blocks, but got none."

    pNodes :: Parser TestnetNodesWithOptions
    pNodes = OA.option readNodeSpecs
      (   OA.long "nodes"
      <>  OA.help "Comma-separated node specifications. SPO nodes must come before relay nodes. \
                   \Each spec is a role (spo or relay) optionally followed by :node-bin=<path>. \
                   \If the path contains commas, colons, double quotes, or backslashes, wrap it \
                   \in double quotes and escape any literal double quotes as \\\" and backslashes \
                   \as \\\\ within. To prevent bash from consuming the double quotes, enclose the \
                   \whole argument in single quotes. \
                   \Examples: --nodes spo,spo:node-bin=/path/to/bin,relay,relay | \
                   \--nodes 'spo:node-bin=\"/path,with:commas\",relay'"
      <>  OA.metavar "SPEC[,SPEC...]"
      )

    readNodeSpecs :: OA.ReadM TestnetNodesWithOptions
    readNodeSpecs = readerAsk >>= either (fail . show) pure . parseNodeSpecs

-- | Parse a @--nodes@ argument string into 'TestnetNodesWithOptions'.
--
-- SPO nodes are required to appear before relay nodes because:
--
-- 1. The testnet configuration assigns node directories by position (node-spo1,
--    node-spo2, …, relay1, relay2, …). Allowing arbitrary ordering would require
--    maintaining a separate mapping between pool indices and node positions, which
--    much of the existing code does not expect.
--
-- 2. Silently reordering (e.g. turning @relay,spo@ into @spo,relay@) would
--    violate the user's expectations about which node gets which configuration.
--
-- 3. Requiring SPOs first lets us represent the result directly as a 'NonEmpty'
--    list of SPO nodes (guaranteeing at least one) plus a plain list of relays,
--    so the type itself makes an invalid configuration unrepresentable.
parseNodeSpecs :: String -> Either Parsec.ParseError TestnetNodesWithOptions
parseNodeSpecs = parse (nodeSpecsParser <* eof) "Error parsing node specifications"
  where
    nodeSpecsParser :: Parsec.Parser TestnetNodesWithOptions
    nodeSpecsParser = do
      specs <- nodeSpec `sepBy1` char ','
      let (spos, relays) = span (\(role, _) -> role == Spo) specs
      unless (all (\(role, _) -> role == Relay) relays) $
        fail "SPO nodes must come before relay nodes. Example: --nodes spo,spo,relay,relay"
      case map snd spos of
        [] -> fail "Need at least one SPO node to produce blocks."
        (s:ss) -> pure $ TestnetNodesWithOptions
          { optSpoNodes = s :| ss
          , optRelayNodes = map snd relays
          }

    nodeSpec :: Parsec.Parser (NodeRole, NodeWithOptions)
    nodeSpec = do
      role <- nodeRole
      bin <- optional $ char ':' *> nodeBinKV
      pure (role, NodeWithOptions bin [])

    nodeRole :: Parsec.Parser NodeRole
    nodeRole =
          Spo <$ try (string "spo" <* notFollowedBy (noneOf ",:\"\\"))
      <|> Relay <$ try (string "relay" <* notFollowedBy (noneOf ",:\"\\"))
      <?> "node role (\"spo\" or \"relay\")"

    nodeBinKV :: Parsec.Parser FilePath
    nodeBinKV = string "node-bin=" *> (quotedPath <|> unquotedPath) <?> "\"node-bin=<path>\", where <path> is the path to the node binary, optionally quoted if it contains special characters"

    quotedPath :: Parsec.Parser FilePath
    quotedPath = char '"' *> Parsec.many quotedChar <* char '"'
      where
        quotedChar = try (char '\\' *> (char '"' <|> char '\\')) <|> noneOf "\""

    unquotedPath :: Parsec.Parser FilePath
    unquotedPath = many1 (noneOf ",:\"\\")

data NodeRole = Spo | Relay deriving Eq

pOnChainParams :: Parser TestnetOnChainParams
pOnChainParams = fmap (fromMaybe DefaultParams) <$> optional $
  pCustomParamsFile <|> pMainnetParams

pCustomParamsFile :: Parser TestnetOnChainParams
pCustomParamsFile = OnChainParamsFile <$> OA.strOption
  (  OA.long "params-file"
  <> OA.help "File containing custom on-chain parameters in Blockfrost format:\nhttps://docs.blockfrost.io/#tag/cardano--epochs/GET/epochs/latest/parameters"
  <> OA.metavar "FILEPATH"
  )

pMainnetParams :: Parser TestnetOnChainParams
pMainnetParams = OA.flag' OnChainParamsMainnet
  (  OA.long "params-mainnet"
  <> OA.help "Use mainnet on-chain parameters"
  )

pUpdateTimestamps :: Parser UpdateTimestamps
pUpdateTimestamps =
  -- Default to UpdateTimestamps, because when using the two-step flow
  -- (cardano-testnet create-env → cardano-testnet cardano --node-env),
  -- genesis timestamps can become stale.
  -- See https://github.com/IntersectMBO/cardano-node/issues/6455
  OA.flag' UpdateTimestamps (OA.long "update-time" <> OA.help "Update the time stamps in genesis files to current date (default, kept for backward compatibility)" <> OA.internal)
  <|> OA.flag' DontUpdateTimestamps (OA.long "preserve-timestamps" <> OA.help "Do not update the time stamps in genesis files to current date.")
  <|> pure UpdateTimestamps

pEnvOutputDir :: Parser FilePath
pEnvOutputDir = OA.strOption
  (   OA.long "output"
  <>  OA.help "Directory where to create the sandbox environment."
  <>  OA.metavar "DIRECTORY"
  )

pGenesisOptions :: Parser GenesisOptions
pGenesisOptions =
  GenesisOptions
    <$> pNetworkId
    <*> pEpochLength
    <*> pSlotLength
    <*> pActiveSlotCoeffs
  where
    pEpochLength =
      OA.option OA.auto
        (   OA.long "epoch-length"
        <>  OA.help "Epoch length, in number of slots."
        <>  OA.metavar "SLOTS"
        <>  OA.showDefault
        <>  OA.value (genesisEpochLength def)
        )
    pSlotLength =
      OA.option OA.auto
        (   OA.long "slot-length"
        <>  OA.help "Slot length."
        <>  OA.metavar "SECONDS"
        <>  OA.showDefault
        <>  OA.value (genesisSlotLength def)
        )
    pActiveSlotCoeffs =
      OA.option OA.auto
        (   OA.long "active-slots-coeff"
        <>  OA.help "Active slots coefficient."
        <>  OA.metavar "DOUBLE"
        <>  OA.showDefault
        <>  OA.value (genesisActiveSlotsCoeff def)
        )

cmdCardano :: Mod CommandFields CardanoTestnetCliOptions
cmdCardano = command' "cardano" "Start a testnet and keep it running until stopped" optsTestnet

cmdCreateEnv :: Mod CommandFields CardanoTestnetCreateEnvOptions
cmdCreateEnv = command' "create-env" "Create a sandbox for Cardano testnet" optsCreateTestnet

pNetworkId :: Parser Int
pNetworkId =
  OA.option (bounded "TESTNET_MAGIC") $ mconcat
    [ OA.long "testnet-magic"
    , OA.metavar "INT"
    , OA.help "Specify a testnet magic id."
    , OA.showDefault
    , OA.value defaultTestnetMagic
    ]

pMaxLovelaceSupply :: Parser Word64
pMaxLovelaceSupply =
  OA.option OA.auto
      (   OA.long "max-lovelace-supply"
      <>  OA.help "Max lovelace supply that your testnet starts with."
      <>  OA.metavar "WORD64"
      <>  OA.showDefault
      <>  OA.value (creationMaxSupply def)
      )
