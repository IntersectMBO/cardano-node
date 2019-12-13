{-# LANGUAGE ApplicativeDo #-}

module Cardano.CLI.Parsers
  ( command'
  , parseLogConfigFileLast
  , parseDbPathLast
  , parseDelegationCertLast
  , parseGenesisHashLast
  , parseGenesisPathLast
  , parsePbftSigThresholdLast
  , parseRequiresNetworkMagicLast
  , parseRequiresNetworkMagic
  , parseSigningKeyLast
  , parseSlotLengthLast
  , parseSocketDirLast
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Options.Applicative as OA

import           Cardano.Config.CommonCLI
import qualified Ouroboros.Consensus.BlockchainTime  as Consensus
import           Cardano.Crypto (RequiresNetworkMagic(..))

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    OA.command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]

parseLogConfigFileLast :: Parser (Last FilePath)
parseLogConfigFileLast =
  lastStrOption
    ( long "log-config"
    <> metavar "LOGCONFIG"
    <> help "Configuration file for logging"
    <> completer (bashCompleter "file")
    )

parseDbPathLast :: Parser (Last FilePath)
parseDbPathLast =
  lastStrOption
    ( long "database-path"
        <> metavar "FILEPATH"
        <> help "Directory where the state is stored."
    )

parseDelegationCertLast :: Parser (Last FilePath)
parseDelegationCertLast =
  lastStrOption
    ( long "delegation-certificate"
        <> metavar "FILEPATH"
        <> help "Path to the delegation certificate."
    )

parseGenesisHashLast :: Parser (Last Text)
parseGenesisHashLast =
  lastStrOption
    ( long "genesis-hash"
        <> metavar "GENESIS-HASH"
        <> help "The genesis hash value."
    )

parseGenesisPathLast :: Parser (Last FilePath)
parseGenesisPathLast =
  lastStrOption
    ( long "genesis-file"
        <> metavar "FILEPATH"
        <> help "The filepath to the genesis file."
    )

parsePbftSigThresholdLast :: Parser (Last Double)
parsePbftSigThresholdLast =
  lastDoubleOption
    ( long "pbft-signature-threshold"
        <> metavar "DOUBLE"
        <> help "The PBFT signature threshold."
        <> hidden
    )

parseRequiresNetworkMagicLast :: Parser (Last RequiresNetworkMagic)
parseRequiresNetworkMagicLast =
  lastFlag RequiresNoMagic RequiresMagic
    ( long "require-network-magic"
        <> help "Require network magic in transactions."
        <> hidden
    )


parseRequiresNetworkMagic :: Parser RequiresNetworkMagic
parseRequiresNetworkMagic =
  flag RequiresNoMagic RequiresMagic
    ( long "require-network-magic"
        <> help "Require network magic in transactions."
        <> hidden
    )

parseSigningKeyLast :: Parser (Last FilePath)
parseSigningKeyLast =
  lastStrOption
    ( long "signing-key"
        <> metavar "FILEPATH"
        <> help "Path to the signing key."
    )

parseSlotLengthLast :: Parser (Last Consensus.SlotLength)
parseSlotLengthLast = do
  slotDurInteger <- lastAutoOption
                      ( long "slot-duration"
                          <> metavar "SECONDS"
                          <> help "The slot duration (seconds)"
                          <> hidden
                      )
  pure $ mkSlotLength <$> slotDurInteger
 where
  mkSlotLength :: Integer -> Consensus.SlotLength
  mkSlotLength sI = Consensus.slotLengthFromMillisec $ 1000 * sI

parseSocketDirLast :: Parser (Last FilePath)
parseSocketDirLast =
  lastStrOption
    ( long "socket-dir"
        <> metavar "FILEPATH"
        <> help "Directory with local sockets:\
                \  ${dir}/node-{core,relay}-${node-id}.socket"
    )
