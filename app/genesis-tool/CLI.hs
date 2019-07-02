{-# LANGUAGE GADTs #-}

module CLI (
    -- * CLI
    CLI(..)
  , Command(..)
  , parseCLI
  , KeyMaterialOps(..)
  , SystemVersion(..)
  ) where

import qualified Data.ByteString.Lazy as LB
import           Data.Foldable (asum)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Options.Applicative

import           Cardano.Binary (Annotated(..))
import           Cardano.Crypto.ProtocolMagic
import           Cardano.Crypto.Signing
import           Cardano.Chain.Common
import           Cardano.Chain.Genesis

import           NodeLib

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

data CLI = CLI
  { systemVersion :: SystemVersion
  , mainCommand   :: Command
  }

data SystemVersion
  = ByronLegacy
  | ByronPBFT
  deriving Show

data Command
  = Genesis
    !FilePath
    !UTCTime
    !FilePath
    !BlockCount
    !ProtocolMagic
    !TestnetBalanceOptions
    !FakeAvvmOptions
    !LovelacePortion
    !Integer
  | PrettySecretKeyPublicHash
    !FilePath

data KeyMaterialOps m
  = KeyMaterialOps
  { kmoSerialiseGenesisKey       :: SigningKey  -> m LB.ByteString
  , kmoSerialiseDelegateKey      :: SigningKey  -> m LB.ByteString
  , kmoSerialisePoorKey          :: PoorSecret  -> m LB.ByteString
  , kmoSerialiseGenesis          :: GenesisData -> m LB.ByteString
  , kmoDeserialiseDelegateKey    :: LB.ByteString -> SigningKey
  }

{-------------------------------------------------------------------------------
  CLI parsers.
-------------------------------------------------------------------------------}

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseSystemVersion
    <*> parseCommand

parseSystemVersion :: Parser SystemVersion
parseSystemVersion = asum
  [ flag' ByronLegacy $ mconcat [ long "byron-legacy" ]
  , flag' ByronPBFT   $ mconcat [ long "byron-pbft" ]
  ]

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "genesis"                        "Perform genesis." $
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
      <*> parseIntegral    "secret-seed"              "Optionally specify the seed of generation."
  , command' "pretty-secret-key-public-hash"  "Print a hash of secret key's public key (not a secret)." $
      PrettySecretKeyPublicHash
      <$> parseFilePath    "secret"                   "File name of the secret key to pretty-print."
  ]

parseTestnetBalanceOptions :: Parser TestnetBalanceOptions
parseTestnetBalanceOptions =
  TestnetBalanceOptions
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

parseFakeAvvmOptions :: Parser FakeAvvmOptions
parseFakeAvvmOptions =
  FakeAvvmOptions
  <$> parseIntegral        "avvm-entry-count"         "Number of AVVM addresses."
  <*> parseLovelace        "avvm-entry-balance"       "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
  <$> parseIntegral        "k"                        "The security parameter of the Ouroboros protocol."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated () . ProtocolMagicId
  <$> parseIntegral        "protocol-magic"           "The magic number unique to any instance of Cardano."

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
