{-# LANGUAGE ApplicativeDo #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String, error, id)

import           Control.Arrow
import           Control.Monad.Trans.Except.Extra (runExceptT)
import qualified Data.List.NonEmpty as NE
import           Data.Text
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Options.Applicative as Opt
import           Options.Applicative (Parser, ParserInfo, ParserPrefs, auto,
                                      commandGroup, flag, flag', help, long,
                                      metavar, option, showHelpOnEmpty,
                                      strOption, subparser, value)

import           System.Exit (exitFailure)

import           Cardano.Binary (Annotated (..))
import           Cardano.Chain.Common
import           Cardano.Chain.Genesis
import           Cardano.Chain.Slotting
import           Cardano.Chain.UTxO
import           Cardano.Crypto ( AProtocolMagic(..)
                                , ProtocolMagic
                                , ProtocolMagicId(..)
                                , RequiresNetworkMagic(..)
                                , decodeHash)

import           Cardano.Config.CommonCLI
import           Cardano.Config.Types (CardanoEnvironment (..),
                                       CardanoConfiguration(..))
import           Cardano.Config.Partial (PartialCardanoConfiguration (..),
                                         PartialCore (..))
import           Cardano.Config.Presets (mainnetConfiguration)
import           Cardano.Config.Protocol (Protocol)

import           Cardano.Common.Parsers
import           Cardano.Config.Logging
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.Config.Logging (createLoggingFeature)
import           Cardano.CLI.Ops (decideCLIOps)
import           Cardano.CLI.Run

main :: IO ()
main = do
  co <- Opt.customExecParser pref opts

  -- Initialize logging layer. Particularly, we need it for benchmarking (command 'generate-txs').
  let cardanoConfiguration :: PartialCardanoConfiguration
      cardanoConfiguration = mainnetConfiguration
  let cardanoEnvironment   = NoEnvironment

  cmdRes <- runExceptT $ do
    let emptyCommonCli = CommonCLI (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)
    finalConfig <- withExceptT ConfigError $ ExceptT $ pure $
      mkConfiguration (cardanoConfiguration <> partialConfig co) emptyCommonCli (commonCliAdv co)
    ops <- liftIO $ decideCLIOps (ccProtocol finalConfig)
    (loggingLayer, _loggingFeature) <- liftIO $
      createLoggingFeature cardanoEnvironment (finalConfig {ccLogConfig = logConfigFile $ loggingCli co})
    (runCommand ops finalConfig loggingLayer (mainCommand co) :: ExceptT CliError IO ())
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
  , commonCliAdv :: CommonCLIAdvanced
  , loggingCli  :: LoggingCLIArguments
  }

parseClient :: Parser CLI
parseClient = do
  ptcl <- (parseProtocolByron <|> parseProtocolRealPBFT)
  cmd <- parseClientCommand
  dbPath <- parseDbPath
  genPath <- parseGenesisPath
  genHash <- parseGenesisHash
  delCert <- parseDelegationeCert
  sKey <- parseSigningKey
  socketDir <- parseSocketDir
  comCliAdv <- parseCommonCLIAdvanced
  logging <- loggingParser
  pure $ CLI (createPcc ptcl dbPath genPath genHash delCert sKey socketDir) cmd comCliAdv logging
 where
  -- This merges the command line parsed values into one `PartialCardanoconfiguration`.
  createPcc
    :: Last Protocol
    -> Last FilePath -- Db Path
    -> Last FilePath -- Genesis Path
    -> Last Text -- Genesis Hash
    -> Last FilePath -- Deleg cert
    -> Last FilePath -- Signing Key
    -> Last FilePath -- Socket dir
    -> PartialCardanoConfiguration
  createPcc
    ptcl
    dbPath
    genPath
    genHash
    delCert
    sKey
    socketDir = mempty { pccDBPath = dbPath
                       , pccProtocol = ptcl
                       , pccSocketDir = socketDir
                       , pccCore = mempty { pcoGenesisFile = genPath
                                          , pcoGenesisHash = genHash
                                          , pcoStaticKeyDlgCertFile = delCert
                                          , pcoStaticKeySigningKeyFile = sKey
                                          }
                       }


parseClientCommand :: Parser ClientCommand
parseClientCommand =
  parseGenesisRelatedValues
    <|> parseKeyRelatedValues
    <|> parseDelegationRelatedValues
    <|> parseTxRelatedValues

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
    <*> parseLovelacePortionWithDefault
          "avvm-balance-factor"
          "AVVM balances will be multiplied by this factor (defaults to 1)."
          1
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
    , command'
        "dump-hardcoded-genesis"
        "Write out a hard-coded genesis."
        $ DumpHardcodedGenesis
            <$> parseNewDirectory
                  "genesis-output-dir"
                  "Non-existent directory where the genesis artifacts are to be written."
    , command' "print-genesis-hash" "Compute hash of a genesis file."
        $ PrintGenesisHash
            <$> parseGenesisFile "genesis-json"
    ]


-- | Values required to create keys and perform
-- transformation on keys.
parseKeyRelatedValues :: Parser ClientCommand
parseKeyRelatedValues =
  subparser $ mconcat
        [ commandGroup "Key related commands"
        , metavar "Key related commands"
        , command' "keygen" "Generate a signing key."
            $ Keygen
                <$> parseNewSigningKeyFile "secret"
                <*> parseFlag' GetPassword EmptyPassword
                      "no-password"
                      "Disable password protection."
        , command'
            "to-verification"
            "Extract a verification key in its base64 form."
            $ ToVerification
                <$> parseSigningKeyFile
                      "secret"
                      "Signing key file to extract the verification part from."
                <*> parseNewVerificationKeyFile "to"
        , command'
            "signing-key-public"
            "Pretty-print a signing key's verification key (not a secret)."
            $ PrettySigningKeyPublic
                <$> parseSigningKeyFile
                      "secret"
                      "Signing key to pretty-print."
        , command'
            "signing-key-address"
            "Print address of a signing key."
            $ PrintSigningKeyAddress
                <$> parseNetworkMagic
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key, whose address is to be printed."
        , command'
            "migrate-delegate-key-from"
            "Migrate a delegate key from an older version."
            $ MigrateDelegateKeyFrom
                <$> parseProtocol
                <*> parseNewSigningKeyFile "to"
                <*> parseSigningKeyFile "from" "Signing key file to migrate."
        ]


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
        <$> parseProtocolMagicId "protocol-magic"
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


parseTxRelatedValues :: Parser ClientCommand
parseTxRelatedValues =
  subparser $ mconcat
    [ commandGroup "Transaction related commands"
    , metavar "Transaction related commands"
    , command'
        "submit-tx"
        "Submit a raw, signed transaction, in its on-wire representation."
        $ SubmitTx
            <$> parseTopologyInfo "PBFT node ID to submit Tx to."
            <*> parseTxFile "tx"
    , command'
        "issue-genesis-utxo-expenditure"
        "Write a file with a signed transaction, spending genesis UTxO."
        $ SpendGenesisUTxO
            <$> parseNewTxFile "tx"
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
            <$> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> (NE.fromList <$> some parseTxIn)
            <*> (NE.fromList <$> some parseTxOut)
    , command'
        "generate-txs"
        "Launch transactions generator."
        $ GenerateTxs
            <$> parseTopologyInfo
                  "PBFT node ID to submit generated Txs to."
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
            <*> parseSigningKeysFiles
                  "sig-key"
                  "Path to signing key file, for genesis UTxO using by generator."
      ]


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
    <*> parseLovelacePortion
          "delegate-share"
          "Portion of stake owned by all delegates together."
    <*> parseFlag
          "use-hd-addresses"
          "Whether generate plain addresses or with hd payload."

parseLovelace :: String -> String -> Parser Lovelace
parseLovelace optname desc =
  either (error . show) id . mkLovelace
    <$> parseIntegral optname desc

parseLovelacePortion :: String -> String -> Parser LovelacePortion
parseLovelacePortion optname desc =
  either (error . show) id . mkLovelacePortion
    <$> parseIntegral optname desc

parseLovelacePortionWithDefault
  :: String
  -> String
  -> Word64
  -> Parser LovelacePortion
parseLovelacePortionWithDefault optname desc w =
  either (error . show) id . mkLovelacePortion
    <$> parseIntegralWithDefault optname desc w

parseFakeAvvmOptions :: Parser FakeAvvmOptions
parseFakeAvvmOptions =
  FakeAvvmOptions
    <$> parseIntegral "avvm-entry-count" "Number of AVVM addresses."
    <*> parseLovelace "avvm-entry-balance" "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
    <$> parseIntegral "k" "The security parameter of the Ouroboros protocol."

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

parseProtocolMagicId :: String -> Parser ProtocolMagicId
parseProtocolMagicId arg =
  ProtocolMagicId
    <$> parseIntegral arg "The magic number unique to any instance of Cardano."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated ()
    <$> parseProtocolMagicId "protocol-magic"

parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
  option (posixSecondsToUTCTime . fromInteger <$> auto)
    $ long optname <> metavar "POSIXSECONDS" <> help desc


parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption $ long optname <> metavar "FILEPATH" <> help desc


parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc


parseIntegralWithDefault :: Integral a => String -> String -> a -> Parser a
parseIntegralWithDefault optname desc def = option (fromInteger <$> auto)
 $ long optname <> metavar "INT" <> help desc <> value def


parseFlag :: String -> String -> Parser Bool
parseFlag = parseFlag' False True

parseFlag' :: a -> a -> String -> String -> Parser a
parseFlag' def active optname desc =
  flag def active $ long optname <> help desc


-- | Here, we hope to get away with the usage of 'error' in a pure expression,
--   because the CLI-originated values are either used, in which case the error is
--   unavoidable rather early in the CLI tooling scenario (and especially so, if
--   the relevant command ADT constructor is strict, like with ClientCommand), or
--   they are ignored, in which case they are arguably irrelevant.
--   And we're getting a correct-by-construction value that doesn't need to be
--   scrutinised later, so that's an abstraction benefit as well.
cliParseBase58Address :: Text -> Address
cliParseBase58Address =
  either (error . ("Bad Base58 address: " <>) . show) id
  . fromCBORTextAddress

-- | See the rationale for cliParseBase58Address.
cliParseLovelace :: Word64 -> Lovelace
cliParseLovelace =
  either (error . ("Bad Lovelace value: " <>) . show) id
  . mkLovelace

-- | See the rationale for cliParseBase58Address.
cliParseTxId :: String -> TxId
cliParseTxId =
  either (error . ("Bad Lovelace value: " <>) . show) id
  . decodeHash . pack

parseAddress :: String -> String -> Parser Address
parseAddress opt desc =
  option (cliParseBase58Address <$> auto)
    $ long opt <> metavar "ADDR" <> help desc


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


parseGenesisFile :: String -> Parser GenesisFile
parseGenesisFile opt =
  GenesisFile <$> parseFilePath opt "Genesis JSON file."

parseNewDirectory :: String -> String -> Parser NewDirectory
parseNewDirectory opt desc = NewDirectory <$> parseFilePath opt desc

parseSigningKeyFile :: String -> String -> Parser SigningKeyFile
parseSigningKeyFile opt desc = SigningKeyFile <$> parseFilePath opt desc

parseNewSigningKeyFile :: String -> Parser NewSigningKeyFile
parseNewSigningKeyFile opt =
  NewSigningKeyFile
    <$> parseFilePath opt "Non-existent file to write the signing key to."

parseVerificationKeyFile :: String -> String -> Parser VerificationKeyFile
parseVerificationKeyFile opt desc = VerificationKeyFile <$> parseFilePath opt desc

parseNewVerificationKeyFile :: String -> Parser NewVerificationKeyFile
parseNewVerificationKeyFile opt =
  NewVerificationKeyFile
    <$> parseFilePath opt "Non-existent file to write the verification key to."

parseCertificateFile :: String -> String -> Parser CertificateFile
parseCertificateFile opt desc = CertificateFile <$> parseFilePath opt desc

parseNewCertificateFile :: String -> Parser NewCertificateFile
parseNewCertificateFile opt =
  NewCertificateFile
    <$> parseFilePath opt "Non-existent file to write the certificate to."

parseTxFile :: String -> Parser TxFile
parseTxFile opt =
  TxFile
    <$> parseFilePath opt "File containing the signed transaction."

parseNewTxFile :: String -> Parser NewTxFile
parseNewTxFile opt =
  NewTxFile
    <$> parseFilePath opt "Non-existent file to write the signed transaction to."

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

parseSigningKeysFiles :: String -> String -> Parser [SigningKeyFile]
parseSigningKeysFiles opt desc = many $ SigningKeyFile <$> parseFilePath opt desc
