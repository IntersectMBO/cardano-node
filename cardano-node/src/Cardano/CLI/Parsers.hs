{-# LANGUAGE ApplicativeDo #-}

module Cardano.CLI.Parsers
  ( command'
  , parseBenchmarkingCommands
  , parseDelegationRelatedValues
  , parseGenesisParameters
  , parseGenesisRelatedValues
  , parseKeyRelatedValues
  , parseLocalNodeQueryValues
  , parseRequiresNetworkMagic
  , parseTxRelatedValues
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import qualified Control.Arrow
import qualified Data.List.NonEmpty as NE
import           Data.Text (pack)
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Network.Socket (PortNumber)
import           Options.Applicative as OA

import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Run
import           Cardano.Common.Parsers

import           Cardano.Binary (Annotated(..))
import           Cardano.Chain.Common
                   (Address(..), BlockCount(..), Lovelace
                   , NetworkMagic(..), decodeAddressBase58
                   , mkLovelace, rationalToLovelacePortion)
import           Cardano.Chain.Genesis (FakeAvvmOptions(..), TestnetBalanceOptions(..))
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.Chain.UTxO (TxId, TxIn(..), TxOut(..))
import           Cardano.Config.CommonCLI
import           Cardano.Config.Types
                   (ConfigYamlFilePath(..), DelegationCertFile(..), GenesisFile(..)
                   , NodeAddress(..), NodeHostAddress(..), SigningKeyFile(..))
import           Cardano.Crypto (RequiresNetworkMagic(..), decodeHash)
import           Cardano.Crypto.ProtocolMagic
                   (AProtocolMagic(..), ProtocolMagic
                   , ProtocolMagicId(..))


-- | See the rationale for cliParseBase58Address.
cliParseLovelace :: Word64 -> Lovelace
cliParseLovelace =
  either (panic . ("Bad Lovelace value: " <>) . show) identity
  . mkLovelace

-- | Here, we hope to get away with the usage of 'error' in a pure expression,
--   because the CLI-originated values are either used, in which case the error is
--   unavoidable rather early in the CLI tooling scenario (and especially so, if
--   the relevant command ADT constructor is strict, like with ClientCommand), or
--   they are ignored, in which case they are arguably irrelevant.
--   And we're getting a correct-by-construction value that doesn't need to be
--   scrutinised later, so that's an abstraction benefit as well.
cliParseBase58Address :: Text -> Address
cliParseBase58Address =
  either (panic . ("Bad Base58 address: " <>) . show) identity
  . decodeAddressBase58

cliParseHostAddress :: String -> NodeHostAddress
cliParseHostAddress = NodeHostAddress . Just .
  maybe (panic "Bad host of target node") identity . readMaybe

cliParsePort :: Word16 -> PortNumber
cliParsePort = fromIntegral

-- | See the rationale for cliParseBase58Address.
cliParseTxId :: String -> TxId
cliParseTxId =
  either (panic . ("Bad Lovelace value: " <>) . show) identity
  . decodeHash . pack

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    OA.command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]

parseAddress :: String -> String -> Parser Address
parseAddress opt desc =
  option (cliParseBase58Address <$> auto)
    $ long opt <> metavar "ADDR" <> help desc

parseBenchmarkingCommands :: Parser ClientCommand
parseBenchmarkingCommands =
  subparser $ mconcat
    [ commandGroup "Benchmarking related commands"
    , metavar "Benchmarking related commands"
    , command'
        "generate-txs"
        "Launch transactions generator."
        $ GenerateTxs
            <$> parseConfigFile
            <*> parseSigningKeyFile "signing-key" "Signing key file."
            <*> (DelegationCertFile <$> parseDelegationCert)
            <*> (GenesisFile <$> parseGenesisPath)
            <*> parseSocketPath "Path to a cardano-node socket"
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
     ]


parseCertificateFile :: String -> String -> Parser CertificateFile
parseCertificateFile opt desc = CertificateFile <$> parseFilePath opt desc

parseDelegationRelatedValues :: Parser ClientCommand
parseDelegationRelatedValues =
  subparser $ mconcat
    [ commandGroup "Delegation related commands"
    , metavar "Delegation related commands"
    , command'
        "issue-delegation-certificate"
        "Create a delegation certificate allowing the\
        \ delegator to sign blocks on behalf of the issuer"
        $ IssueDelegationCertificate
        <$> parseProtocol
        <*> parseProtocolMagicId "protocol-magic"
        <*> ( EpochNumber
                <$> parseIntegral
                      "since-epoch"
                      "The epoch from which the delegation is valid."
              )
        <*> parseSigningKeyFile
              "secret"
              "The issuer of the certificate, who delegates\
              \ their right to sign blocks."
        <*> parseVerificationKeyFile
              "delegate-key"
              "The delegate, who gains the right to sign block."
        <*> parseNewCertificateFile "certificate"
    , command'
        "check-delegation"
        "Verify that a given certificate constitutes a valid\
        \ delegation relationship between keys."
        $ CheckDelegation
            <$> parseProtocolMagicId "protocol-magic"
            <*> parseCertificateFile
                  "certificate"
                  "The certificate embodying delegation to verify."
            <*> parseVerificationKeyFile
                  "issuer-key"
                  "The genesis key that supposedly delegates."
            <*> parseVerificationKeyFile
                  "delegate-key"
                  "The operation verification key supposedly delegated to."
      ]


parseFakeAvvmOptions :: Parser FakeAvvmOptions
parseFakeAvvmOptions =
  FakeAvvmOptions
    <$> parseIntegral "avvm-entry-count" "Number of AVVM addresses."
    <*> parseLovelace "avvm-entry-balance" "AVVM address."

parseFeePerTx :: String -> String -> Parser FeePerTx
parseFeePerTx opt desc = FeePerTx <$> parseIntegral opt desc

-- | Values required to create genesis.
parseGenesisParameters :: Parser GenesisParameters
parseGenesisParameters =
  GenesisParameters
    <$> parseUTCTime
          "start-time"
          "Start time of the new cluster to be enshrined in the new genesis."
    <*> parseFilePath
          "protocol-parameters-file"
          "JSON file with protocol parameters."
    <*> parseK
    <*> parseProtocolMagic
    <*> parseTestnetBalanceOptions
    <*> parseFakeAvvmOptions
    <*> (rationalToLovelacePortion <$>
         parseFractionWithDefault
          "avvm-balance-factor"
          "AVVM balances will be multiplied by this factor (defaults to 1)."
          1)
    <*> optional
        ( parseIntegral
            "secret-seed"
            "Optionally specify the seed of generation."
        )

parseGenesisRelatedValues :: Parser ClientCommand
parseGenesisRelatedValues =
  subparser $ mconcat
    [ commandGroup "Genesis related commands"
    , metavar "Genesis related commands"
    , command' "genesis" "Create genesis."
      $ Genesis
          <$> parseNewDirectory
              "genesis-output-dir"
              "Non-existent directory where genesis JSON file and secrets shall be placed."
          <*> parseGenesisParameters
          <*> parseProtocol
    , command' "print-genesis-hash" "Compute hash of a genesis file."
        $ PrintGenesisHash
            <$> parseGenesisFile "genesis-json"
    ]

parseK :: Parser BlockCount
parseK =
  BlockCount
    <$> parseIntegral "k" "The security parameter of the Ouroboros protocol."

parseNewDirectory :: String -> String -> Parser NewDirectory
parseNewDirectory opt desc = NewDirectory <$> parseFilePath opt desc

-- | Values required to create keys and perform
-- transformation on keys.
parseKeyRelatedValues :: Parser ClientCommand
parseKeyRelatedValues =
  subparser $ mconcat
        [ commandGroup "Key related commands"
        , metavar "Key related commands"
        , command' "keygen" "Generate a signing key."
            $ Keygen
                <$> parseProtocol
                <*> parseNewSigningKeyFile "secret"
                <*> parseFlag' GetPassword EmptyPassword
                      "no-password"
                      "Disable password protection."
        , command'
            "to-verification"
            "Extract a verification key in its base64 form."
            $ ToVerification
                <$> parseProtocol
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key file to extract the verification part from."
                <*> parseNewVerificationKeyFile "to"
        , command'
            "signing-key-public"
            "Pretty-print a signing key's verification key (not a secret)."
            $ PrettySigningKeyPublic
                <$> parseProtocol
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key to pretty-print."
        , command'
            "signing-key-address"
            "Print address of a signing key."
            $ PrintSigningKeyAddress
                <$> parseProtocol
                <*> parseNetworkMagic
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key, whose address is to be printed."
        , command'
            "migrate-delegate-key-from"
            "Migrate a delegate key from an older version."
            $ MigrateDelegateKeyFrom
                <$> parseProtocol -- Old protocol
                <*> parseSigningKeyFile "from" "Signing key file to migrate."
                <*> parseProtocol -- New protocol
                <*> parseNewSigningKeyFile "to"
        ]
parseLocalNodeQueryValues :: Parser ClientCommand
parseLocalNodeQueryValues =
  subparser $ mconcat
        [ commandGroup "Local node related commands"
        , metavar "Local node related commands"
        , command' "get-tip" "Get the tip of your local node's blockchain"
            $ GetLocalNodeTip
                <$> (ConfigYamlFilePath <$> parseConfigFile)
                <*> parseGenesisFile "genesis-json"
                <*> parseSocketPath "Socket of target node"
        ]

parseLovelace :: String -> String -> Parser Lovelace
parseLovelace optname desc =
  either (panic . show) identity . mkLovelace
    <$> parseIntegral optname desc

parseFraction :: String -> String -> Parser Rational
parseFraction optname desc =
  option (toRational <$> readDouble) $
      long optname
   <> metavar "DOUBLE"
   <> help desc
  where

parseFractionWithDefault
  :: String
  -> String
  -> Double
  -> Parser Rational
parseFractionWithDefault optname desc w =
  toRational <$> ( option readDouble
                 $ long optname
                <> metavar "DOUBLE"
                <> help desc
                <> value w
                )

readDouble :: ReadM Double
readDouble = do
  f <- auto
  when (f < 0) $ readerError "fraction must be >= 0"
  when (f > 1) $ readerError "fraction must be <= 1"
  return f

parseNetworkMagic :: Parser NetworkMagic
parseNetworkMagic =
  asum [ flag' NetworkMainOrStage $ mconcat
           [ long "main-or-staging"
           , help ""
           ]
       , option (fmap NetworkTestnet auto)
           $ long "testnet-magic"
             <> metavar "MAGIC"
             <> help "The testnet network magic, decibal"
       ]

parseNewCertificateFile :: String -> Parser NewCertificateFile
parseNewCertificateFile opt =
  NewCertificateFile
    <$> parseFilePath opt "Non-existent file to write the certificate to."

parseNewSigningKeyFile :: String -> Parser NewSigningKeyFile
parseNewSigningKeyFile opt =
  NewSigningKeyFile
    <$> parseFilePath opt "Non-existent file to write the signing key to."

parseNewTxFile :: String -> Parser NewTxFile
parseNewTxFile opt =
  NewTxFile
    <$> parseFilePath opt "Non-existent file to write the signed transaction to."

parseNewVerificationKeyFile :: String -> Parser NewVerificationKeyFile
parseNewVerificationKeyFile opt =
  NewVerificationKeyFile
    <$> parseFilePath opt "Non-existent file to write the verification key to."

parseNumberOfInputsPerTx :: String -> String -> Parser NumberOfInputsPerTx
parseNumberOfInputsPerTx opt desc = NumberOfInputsPerTx <$> parseIntegral opt desc

parseNumberOfOutputsPerTx :: String -> String -> Parser NumberOfOutputsPerTx
parseNumberOfOutputsPerTx opt desc = NumberOfOutputsPerTx <$> parseIntegral opt desc

parseNumberOfTxs :: String -> String -> Parser NumberOfTxs
parseNumberOfTxs opt desc = NumberOfTxs <$> parseIntegral opt desc

parseProtocolMagicId :: String -> Parser ProtocolMagicId
parseProtocolMagicId arg =
  ProtocolMagicId
    <$> parseIntegral arg "The magic number unique to any instance of Cardano."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated ()
    <$> parseProtocolMagicId "protocol-magic"

parseRequiresNetworkMagic :: Parser RequiresNetworkMagic
parseRequiresNetworkMagic =
  flag RequiresNoMagic RequiresMagic
    ( long "require-network-magic"
        <> help "Require network magic in transactions."
        <> hidden
    )

parseSigningKeyFile :: String -> String -> Parser SigningKeyFile
parseSigningKeyFile opt desc = SigningKeyFile <$> parseFilePath opt desc

parseSigningKeysFiles :: String -> String -> Parser [SigningKeyFile]
parseSigningKeysFiles opt desc = some $ SigningKeyFile <$> parseFilePath opt desc

parseTargetNodeAddress :: String -> String -> Parser NodeAddress
parseTargetNodeAddress optname desc =
  option
    ( uncurry NodeAddress
      . Control.Arrow.first cliParseHostAddress
      . Control.Arrow.second cliParsePort
      <$> auto
    )
    $ long optname
      <> metavar "(HOST,PORT)"
      <> help desc

parseTestnetBalanceOptions :: Parser TestnetBalanceOptions
parseTestnetBalanceOptions =
  TestnetBalanceOptions
    <$> parseIntegral
          "n-poor-addresses"
          "Number of poor nodes (with small balance)."
    <*> parseIntegral
          "n-delegate-addresses"
          "Number of delegate nodes (with huge balance)."
    <*> parseLovelace
          "total-balance"
          "Total balance owned by these nodes."
    <*> parseFraction
          "delegate-share"
          "Portion of stake owned by all delegates together."

parseTPSRate :: String -> String -> Parser TPSRate
parseTPSRate opt desc = TPSRate <$> parseIntegral opt desc

parseTxAdditionalSize :: String -> String -> Parser TxAdditionalSize
parseTxAdditionalSize opt desc = TxAdditionalSize <$> parseIntegral opt desc

parseExplorerAPIEndpoint :: String -> String -> Parser ExplorerAPIEnpoint
parseExplorerAPIEndpoint opt desc = ExplorerAPIEnpoint <$> parseUrl opt desc

parseTxFile :: String -> Parser TxFile
parseTxFile opt =
  TxFile
    <$> parseFilePath opt "File containing the signed transaction."

parseTxIn :: Parser TxIn
parseTxIn =
  option
  ( uncurry TxInUtxo
    . Control.Arrow.first cliParseTxId
    <$> auto
  )
  $ long "txin"
    <> metavar "(TXID,INDEX)"
    <> help "Transaction input is a pair of an UTxO TxId and a zero-based output index."

parseTxOut :: Parser TxOut
parseTxOut =
  option
    ( uncurry TxOut
      . Control.Arrow.first cliParseBase58Address
      . Control.Arrow.second cliParseLovelace
      <$> auto
    )
    $ long "txout"
      <> metavar "ADDR:LOVELACE"
      <> help "Specify a transaction output, as a pair of an address and lovelace."

parseTxRelatedValues :: Parser ClientCommand
parseTxRelatedValues =
  subparser $ mconcat
    [ commandGroup "Transaction related commands"
    , metavar "Transaction related commands"
    , command'
        "submit-tx"
        "Submit a raw, signed transaction, in its on-wire representation."
        $ SubmitTx
            <$> parseTxFile "tx"
            <*> parseProtocol
            <*> (GenesisFile <$> parseGenesisPath)
            <*> parseSocketPath "Socket of target node"
    , command'
        "issue-genesis-utxo-expenditure"
        "Write a file with a signed transaction, spending genesis UTxO."
        $ SpendGenesisUTxO
            <$> parseProtocol
            <*> (GenesisFile <$> parseGenesisPath)
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> parseAddress
                  "rich-addr-from"
                  "Tx source: genesis UTxO richman address (non-HD)."
            <*> (NE.fromList <$> some parseTxOut)

    , command'
        "issue-utxo-expenditure"
        "Write a file with a signed transaction, spending normal UTxO."
        $ SpendUTxO
            <$> parseProtocol
            <*> (GenesisFile <$> parseGenesisPath)
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> (NE.fromList <$> some parseTxIn)
            <*> (NE.fromList <$> some parseTxOut)
      ]


parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
  option (posixSecondsToUTCTime . fromInteger <$> auto)
    $ long optname <> metavar "POSIXSECONDS" <> help desc

parseVerificationKeyFile :: String -> String -> Parser VerificationKeyFile
parseVerificationKeyFile opt desc = VerificationKeyFile <$> parseFilePath opt desc
