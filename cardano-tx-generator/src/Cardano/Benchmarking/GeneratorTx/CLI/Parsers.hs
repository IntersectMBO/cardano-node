module Cardano.Benchmarking.GeneratorTx.CLI.Parsers
  ( GenerateTxs (..)
  , parseCommand
  ) where

import           Cardano.Prelude hiding (option)
import qualified Data.List.NonEmpty as NE
import           Prelude (String)
import           Options.Applicative
                    ( Parser
                    , bashCompleter, completer, help, long, metavar
                    , auto, option, strOption
                    )
import qualified Control.Arrow as Arr
import           Network.Socket (PortNumber)

import           Cardano.Config.Types
                    ( SigningKeyFile(..)
                    , DelegationCertFile(..)
                    , GenesisFile(..)
                    , SocketPath(..)
                    , NodeAddress(..)
                    , NodeHostAddress(..)
                    )
import           Cardano.Config.CommonCLI
                    ( parseDelegationCert
                    , parseGenesisPath
                    )

import           Cardano.Benchmarking.GeneratorTx
                    ( NumberOfTxs(..)
                    , NumberOfInputsPerTx(..)
                    , NumberOfOutputsPerTx(..)
                    , FeePerTx(..)
                    , TPSRate(..)
                    , TxAdditionalSize(..)
                    , ExplorerAPIEnpoint(..)
                    )

data GenerateTxs =
  GenerateTxs FilePath
              SigningKeyFile
              DelegationCertFile
              GenesisFile
              SocketPath
              (NonEmpty NodeAddress)
              NumberOfTxs
              NumberOfInputsPerTx
              NumberOfOutputsPerTx
              FeePerTx
              TPSRate
              (Maybe TxAdditionalSize)
              (Maybe ExplorerAPIEnpoint)
              [SigningKeyFile]

parseCommand :: Parser GenerateTxs
parseCommand =
  GenerateTxs
    <$> parseConfigFile
          "config"
          "Configuration file for the cardano-node"
    <*> parseSigningKeyFile
          "signing-key"
          "Signing key file."
    <*> (DelegationCertFile <$> parseDelegationCert)
    <*> (GenesisFile <$> parseGenesisPath)
    <*> parseSocketPath
          "socket-path"
          "Path to a cardano-node socket"
    <*> (NE.fromList <$> some (
            parseTargetNodeAddress
              "target-node"
              "host and port of the node transactions will be sent to."
          )
        )
    <*> parseNumberOfTxs
          "num-of-txs"
          "Number of transactions generator will create."
    <*> parseNumberOfInputsPerTx
          "inputs-per-tx"
          "Number of inputs in each of transactions."
    <*> parseNumberOfOutputsPerTx
          "outputs-per-tx"
          "Number of outputs in each of transactions."
    <*> parseFeePerTx
          "tx-fee"
          "Fee per transaction, in Lovelaces."
    <*> parseTPSRate
          "tps"
          "TPS (transaction per second) rate."
    <*> optional (
          parseTxAdditionalSize
            "add-tx-size"
            "Additional size of transaction, in bytes."
        )
    <*> optional (
          parseExplorerAPIEndpoint
            "submit-to-api"
            "Explorer's API endpoint to submit transaction."
        )
    <*> parseSigningKeysFiles
          "sig-key"
          "Path to signing key file, for genesis UTxO using by generator."

----------------------------------------------------------------

parseTargetNodeAddress :: String -> String -> Parser NodeAddress
parseTargetNodeAddress optname desc =
  option
    ( uncurry NodeAddress
      . Arr.first parseHostAddress
      . Arr.second parsePort
      <$> auto
    )
    $ long optname
      <> metavar "(HOST,PORT)"
      <> help desc

parseHostAddress :: String -> NodeHostAddress
parseHostAddress = NodeHostAddress . Just .
  maybe (panic "Bad host of target node") identity . readMaybe

parsePort :: Word16 -> PortNumber
parsePort = fromIntegral

parseNumberOfTxs :: String -> String -> Parser NumberOfTxs
parseNumberOfTxs opt desc = NumberOfTxs <$> parseIntegral opt desc

parseNumberOfInputsPerTx :: String -> String -> Parser NumberOfInputsPerTx
parseNumberOfInputsPerTx opt desc = NumberOfInputsPerTx <$> parseIntegral opt desc

parseNumberOfOutputsPerTx :: String -> String -> Parser NumberOfOutputsPerTx
parseNumberOfOutputsPerTx opt desc = NumberOfOutputsPerTx <$> parseIntegral opt desc

parseFeePerTx :: String -> String -> Parser FeePerTx
parseFeePerTx opt desc = FeePerTx <$> parseIntegral opt desc

parseTPSRate :: String -> String -> Parser TPSRate
parseTPSRate opt desc = TPSRate <$> parseIntegral opt desc

parseTxAdditionalSize :: String -> String -> Parser TxAdditionalSize
parseTxAdditionalSize opt desc = TxAdditionalSize <$> parseIntegral opt desc

parseExplorerAPIEndpoint :: String -> String -> Parser ExplorerAPIEnpoint
parseExplorerAPIEndpoint opt desc = ExplorerAPIEnpoint <$> parseUrl opt desc

parseSigningKeyFile :: String -> String -> Parser SigningKeyFile
parseSigningKeyFile opt desc = SigningKeyFile <$> parseFilePath opt desc

parseSigningKeysFiles :: String -> String -> Parser [SigningKeyFile]
parseSigningKeysFiles opt desc = some $ SigningKeyFile <$> parseFilePath opt desc

------------------------------------------------------------------

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc

parseUrl :: String -> String -> Parser String
parseUrl optname desc =
  strOption $ long optname <> metavar "URL" <> help desc

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption
    $ long optname
        <> metavar "FILEPATH"
        <> help desc
        <> completer (bashCompleter "file")

parseSocketPath :: String -> String -> Parser SocketPath
parseSocketPath optname desc =
  SocketFile <$> parseFilePath optname desc

parseConfigFile :: String -> String -> Parser FilePath
parseConfigFile = parseFilePath
