{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Control.Monad.Trans.Except.Extra (runExceptT)
import qualified Options.Applicative as Opt
import           Options.Applicative (Parser, ParserInfo, ParserPrefs,
                                      showHelpOnEmpty)
import           System.Exit (exitFailure)

import           Cardano.CLI.Parsers
import           Cardano.CLI.Run
import           Cardano.Common.TopHandler
import           Cardano.Common.Parsers
import           Cardano.Config.Logging (createLoggingFeatureCLI)
import           Cardano.Config.Partial (PartialCardanoConfiguration (..),
                                         PartialCore (..), PartialNode (..),
                                         mkCardanoConfiguration)
import           Cardano.Config.Presets (mainnetConfiguration)
import           Cardano.Config.Protocol (Protocol)
import           Cardano.Config.Types (CardanoEnvironment (..))
import           Cardano.Crypto (RequiresNetworkMagic(..))
import qualified Ouroboros.Consensus.BlockchainTime as Consensus

main :: IO ()
main = toplevelExceptionHandler $ do

  co <- Opt.customExecParser pref opts
  -- Initialize logging layer. Particularly, we need it for benchmarking (command 'generate-txs').
  let cardanoConfiguration :: PartialCardanoConfiguration
      cardanoConfiguration = mainnetConfiguration
      cardanoEnvironment :: CardanoEnvironment
      cardanoEnvironment = NoEnvironment

  cmdRes <- runExceptT $ do
    finalConfig <- withExceptT ConfigError $ ExceptT $ pure $
                     mkCardanoConfiguration $ cardanoConfiguration <> partialConfig co
    (loggingLayer, _loggingFeature) <- liftIO $
      createLoggingFeatureCLI cardanoEnvironment finalConfig
    runCommand finalConfig loggingLayer $ mainCommand co

  case cmdRes of
    Right _ -> pure ()
    Left err -> do putStrLn $ renderCliError err
                   exitFailure
  where
    pref :: ParserPrefs
    pref = Opt.prefs showHelpOnEmpty

    opts :: ParserInfo CLI
    opts =
      Opt.info (parseClient <**> Opt.helper)
        ( Opt.fullDesc
          <> Opt.header
          "cardano-cli - utility to support a variety of key\
          \ operations (genesis generation, migration,\
          \ pretty-printing..) for different system generations."
        )

    renderCliError :: CliError -> String
    renderCliError = show

data CLI = CLI
  { partialConfig :: PartialCardanoConfiguration
  , mainCommand :: ClientCommand
  }

parseClient :: Parser CLI
parseClient = do
  let pConfig = createPcc
                  <$> (parseProtocolByron <|> parseProtocolRealPBFT)
                  <*> parseDbPathLast
                  <*> parseGenesisPathLast
                  <*> parseGenesisHashLast
                  <*> parseDelegationCertLast
                  <*> parseSigningKeyLast
                  <*> parseSocketDirLast
                  <*> parsePbftSigThresholdLast
                  <*> parseRequiresNetworkMagicLast
                  <*> parseSlotLengthLast
                  <*> parseLogConfigFileLast
                  <*> parseLogMetricsLast

  CLI <$> pConfig <*> parseClientCommand
 where
  -- This merges the command line parsed values into one `PartialCardanoconfiguration`.
  createPcc
    :: Last Protocol
    -> Last FilePath
    -- ^ Db Path
    -> Last FilePath
    -- ^ Genesis Path
    -> Last Text
    -- ^ Genesis Hash
    -> Last FilePath
    -- ^ Delegation certificate
    -> Last FilePath
    -- ^ Signing Key
    -> Last FilePath
    -- ^ Socket dir
    -> Last Double
    -- ^ PBFT Signature Threshold
    -> Last RequiresNetworkMagic
    -> Last Consensus.SlotLength
    -> Last FilePath
    -- ^ Log Configuration Path
    -> Last Bool
    -- ^ Capture Log Metrics
    -> PartialCardanoConfiguration
  createPcc
    ptcl
    dbPath
    genPath
    genHash
    delCert
    sKey
    socketDir
    pbftSigThresh
    reqNetMagic
    slotLength
    logConfigFp
    logMetrics = mempty { pccDBPath = dbPath
                        , pccProtocol = ptcl
                        , pccSocketDir = socketDir
                        , pccLogConfig = logConfigFp
                        , pccLogMetrics = logMetrics
                        , pccCore = mempty { pcoGenesisFile = genPath
                                           , pcoGenesisHash = genHash
                                           , pcoStaticKeyDlgCertFile = delCert
                                           , pcoStaticKeySigningKeyFile = sKey
                                           , pcoPBftSigThd = pbftSigThresh
                                           , pcoRequiresNetworkMagic = reqNetMagic
                                           }
                        , pccNode = mempty { pnoSlotLength = slotLength }
                        }


parseClientCommand :: Parser ClientCommand
parseClientCommand =
  parseGenesisRelatedValues
    <|> parseKeyRelatedValues
    <|> parseDelegationRelatedValues
    <|> parseTxRelatedValues
