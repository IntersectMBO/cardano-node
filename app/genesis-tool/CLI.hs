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
import           Data.Maybe (fromMaybe)
import           Data.Time (UTCTime)
import           Options.Applicative

import           Cardano.Crypto.ProtocolMagic
import           Cardano.Crypto.Signing
import           Cardano.Chain.Common
import           Cardano.Chain.Delegation
import           Cardano.Chain.Genesis

import           Cardano.Node.CLI

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
  | PrettySecretKeyPublic
    !FilePath
  | MigrateDelegateKeyFrom
    !SystemVersion
    !FilePath
    !FilePath
  | DumpHardcodedGenesis
    !FilePath

data KeyMaterialOps m
  = KeyMaterialOps
  { kmoSerialiseGenesisKey       :: SigningKey  -> m LB.ByteString
  , kmoSerialiseDelegateKey      :: SigningKey  -> m LB.ByteString
  , kmoSerialisePoorKey          :: PoorSecret  -> m LB.ByteString
  , kmoSerialiseGenesis          :: GenesisData -> m LB.ByteString
  , kmoSerialiseDelegationCert   :: Certificate -> m LB.ByteString
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
parseSystemVersion = subparser $ mconcat
  [ commandGroup "System version"
  , metavar "SYSTEMVER"
  , command' "byron-legacy" "Byron Legacy mode" $ pure ByronLegacy
  , command' "byron-pbft"   "Byron PBFT mode"   $ pure ByronPBFT
  ]

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
  [ command' "genesis"                        "Perform genesis." $
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
  , command' "pretty-secret-key-public"       "Pretty-print a secret key's public key (not a secret)." $
      PrettySecretKeyPublic
      <$> parseFilePath    "secret"                   "File name of the secret key to pretty-print."
  , command' "migrate-delegate-key-from"      "Migrate a delegate key from an older version." $
      MigrateDelegateKeyFrom
      <$> parseSystemVersion
      <*> parseFilePath    "to"                       "Output secret key file."
      <*> parseFilePath    "from"                     "Secret key file to migrate."
  , command' "dump-hardcoded-genesis"         "Write out a hard-coded genesis." $
      DumpHardcodedGenesis
      <$> parseFilePath    "genesis-output-dir"       "A yet-absent directory where genesis JSON file along with secrets shall be placed."
  ]
