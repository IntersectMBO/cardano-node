{-# LANGUAGE NamedFieldPuns #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String, error, id)

import           Cardano.Binary (Annotated (..))
import           Cardano.Chain.Common
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.CLI.Run (ClientCommand(..), SystemVersion(..), decideKeyMaterialOps, runCommand)
import           Cardano.Common.CommonCLI (command')
import           Cardano.Crypto (AProtocolMagic(..), ProtocolMagic, ProtocolMagicId(..), RequiresNetworkMagic(..))

import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Options.Applicative
import           Control.Exception.Safe (catchIO)
import           System.Exit (ExitCode(..), exitWith)

main :: IO ()
main = do
  CLI{mainCommand, systemVersion} <- execParser opts
  catchIO (runCommand (decideKeyMaterialOps systemVersion) mainCommand) $
    \err-> do
      hPutStrLn stderr ("Error:\n" <> show err :: String)
      exitWith $ ExitFailure 1

-- | Top level parser with info.
opts :: ParserInfo CLI
opts = info (parseClient <**> helper)
  ( fullDesc
    <> progDesc "Cardano genesis tool."
    <> header "Cardano genesis tool."
  )

{-------------------------------------------------------------------------------
  CLI parsers & Types
-------------------------------------------------------------------------------}

data CLI = CLI
  { systemVersion :: SystemVersion
  , mainCommand   :: ClientCommand
  }

parseClient :: Parser CLI
parseClient = CLI
    <$> parseSystemVersion
    <*> parseClientCommand

parseSystemVersion :: Parser SystemVersion
parseSystemVersion = subparser $ mconcat
  [ commandGroup "System version"
  , metavar "SYSTEMVER"
  , command' "byron-legacy" "Byron Legacy mode" $ pure ByronLegacy
  , command' "byron-pbft"   "Byron PBFT mode"   $ pure ByronPBFT
  ]

parseClientCommand :: Parser ClientCommand
parseClientCommand =
  subparser
  (mconcat
    [ commandGroup "Genesis"
    , command' "genesis"                        "Perform genesis." $
      Genesis
      <$> parseFilePath    "genesis-output-dir"       "A yet-absent directory where genesis JSON file along with secrets shall be placed."
      <*> parseUTCTime     "start-time"               "Start time of the new cluster to be enshrined in the new genesis."
      <*> parseFilePath    "protocol-parameters-file" "JSON file with protocol parameters."
      <*> parseK
      <*> parseProtocolMagic
      <*> parseTestnetBalanceOptions
      <*> parseFakeAvvmOptions
      <*> (LovelacePortion . fromInteger . fromMaybe 1 <$>
           (optional $
             parseIntegral  "avvm-balance-factor"      "AVVM balances will be multiplied by this factor (defaults to 1)."))
      <*> optional (parseIntegral    "secret-seed"              "Optionally specify the seed of generation.")
    , command' "dump-hardcoded-genesis"         "Write out a hard-coded genesis." $
      DumpHardcodedGenesis
      <$> parseFilePath    "genesis-output-dir"       "A yet-absent directory where genesis JSON file along with secrets shall be placed."
    , command' "print-genesis-hash"             "Compute hash of a genesis file." $
      PrintGenesisHash
      <$> parseFilePath    "genesis-json"             "Genesis JSON file to hash."
    ])
  <|> subparser
  (mconcat
    [ commandGroup "Keys"
    , command' "keygen"                         "Generate a signing key." $
      Keygen
      <$> parseFilePath    "secret"                   "Non-existent file to write the secret key to."
      <*> parseFlag        "no-password"              "Disable password protection."
    , command' "to-verification"                "Extract a verification key in its base64 form." $
      ToVerification
      <$> parseFilePath    "secret"                   "Secret key file to extract from."
      <*> parseFilePath    "to"                       "Non-existent file to write the base64-formatted verification key to."
    , command' "signing-key-public"             "Pretty-print a signing key's verification key (not a secret)." $
      PrettySigningKeyPublic
      <$> parseFilePath    "secret"                   "File name of the secret key to pretty-print."
    , command' "signing-key-address"            "Print address of a signing key." $
      PrintSigningKeyAddress
      <$> parseNetworkMagic
      <*> parseFilePath    "secret"                   "Secret key, whose address is to be printed."
    , command' "migrate-delegate-key-from"      "Migrate a delegate key from an older version." $
      MigrateDelegateKeyFrom
      <$> parseSystemVersion
      <*> parseFilePath    "to"                       "Output secret key file."
      <*> parseFilePath    "from"                     "Secret key file to migrate."
    ])
  <|> subparser
  (mconcat
    [ commandGroup "Delegation"
    , command' "redelegate"                     "Redelegate genesis authority to a different verification key." $
      Redelegate
      <$> parseProtocolMagicId "protocol-magic"
      <*> (EpochNumber <$>
            parseIntegral   "since-epoch"              "First epoch of effective delegation.")
      <*> parseFilePath    "secret"                   "The genesis key to redelegate from."
      <*> parseFilePath    "delegate-key"             "The operation verification key to delegate to."
      <*> parseFilePath    "certificate"              "Non-existent file to write the certificate to."
    , command' "check-delegation"               "Verify that a given certificate constitutes a valid delegation relationship betwen keys." $
      CheckDelegation
      <$> parseProtocolMagicId "protocol-magic"
      <*> parseFilePath    "certificate"              "The certificate embodying delegation to verify."
      <*> parseFilePath    "issuer-key"               "The genesis key that supposedly delegates."
      <*> parseFilePath    "delegate-key"             "The operation verification key supposedly delegated to."
    ])

parseTestnetBalanceOptions :: Parser Genesis.TestnetBalanceOptions
parseTestnetBalanceOptions =
  Genesis.TestnetBalanceOptions
  <$> parseIntegral        "n-poor-addresses"         "Number of poor nodes (with small balance)."
  <*> parseIntegral        "n-delegate-addresses"     "Number of delegate nodes (with huge balance)."
  <*> parseLovelace        "total-balance"            "Total balance owned by these nodes."
  <*> parseLovelacePortion "delegate-share"           "Portion of stake owned by all delegates together."
  <*> parseFlag            "use-hd-addresses"         "Whether generate plain addresses or with hd payload."

parseLovelace :: String -> String -> Parser Lovelace
parseLovelace optname desc =
  either (error . show) id . mkLovelace
  <$> parseIntegral optname desc

parseLovelacePortion :: String -> String -> Parser LovelacePortion
parseLovelacePortion optname desc =
  either (error . show) id . mkLovelacePortion
  <$> parseIntegral optname desc

parseFakeAvvmOptions :: Parser Genesis.FakeAvvmOptions
parseFakeAvvmOptions =
  Genesis.FakeAvvmOptions
  <$> parseIntegral        "avvm-entry-count"         "Number of AVVM addresses."
  <*> parseLovelace        "avvm-entry-balance"       "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
  <$> parseIntegral        "k"                        "The security parameter of the Ouroboros protocol."

parseProtocolMagicId :: String -> Parser ProtocolMagicId
parseProtocolMagicId arg =
  ProtocolMagicId
  <$> parseIntegral        arg                        "The magic number unique to any instance of Cardano."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated ()
  <$> parseProtocolMagicId "protocol-magic"

parseNetworkMagic :: Parser NetworkMagic
parseNetworkMagic = asum
    [ flag' NetworkMainOrStage $ mconcat [
          long "main-or-staging"
        , help ""
        ]
    , option (fmap NetworkTestnet auto) (
          long "testnet-magic"
       <> metavar "MAGIC"
       <> help "The testnet network magic, decibal"
        )
    ]

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
    strOption (
            long optname
         <> metavar "FILEPATH"
         <> help desc
    )

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc =
    option (fromInteger <$> auto) (
            long optname
         <> metavar "INT"
         <> help desc
    )

parseFlag :: String -> String -> Parser Bool
parseFlag optname desc =
    flag False True (
            long optname
         <> help desc
    )

parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
    option (posixSecondsToUTCTime . fromInteger <$> auto) (
            long optname
         <> metavar "POSIXSECONDS"
         <> help desc
    )
