{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.GenesisTool.CLI (
    -- * CLI
    CLI(..)
  , Command(..)
  , parseCLI
  , SystemVersion(..)
  ) where

import           Cardano.Prelude

import           Data.Maybe (fromMaybe)
import           Data.Time (UTCTime)
import           Options.Applicative

import           Cardano.Crypto.ProtocolMagic
import           Cardano.Chain.Common
import           Cardano.Chain.Genesis
import           Cardano.Chain.Slotting

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
    !(Maybe Integer)
  | PrettySigningKeyPublic
    !FilePath
  | MigrateDelegateKeyFrom
    !SystemVersion
    !FilePath
    !FilePath
  | DumpHardcodedGenesis
    !FilePath
  | PrintGenesisHash
    !FilePath
  | PrintSigningKeyAddress
    !NetworkMagic -- TODO:  consider deprecation in favor of ProtocolMagicId,
                  --        once Byron is out of the picture.
    !FilePath
  | Keygen
    !FilePath
    !Bool
  | ToVerification
    !FilePath
    !FilePath
  | Redelegate
    !ProtocolMagicId
    !EpochNumber
    !FilePath
    !FilePath
    !FilePath
  | CheckDelegation
    !ProtocolMagicId
    !FilePath
    !FilePath
    !FilePath

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
      <*> optional (parseIntegral    "secret-seed"              "Optionally specify the seed of generation.")
  , command' "signing-key-public"             "Pretty-print a signing key's verification key (not a secret)." $
      PrettySigningKeyPublic
      <$> parseFilePath    "secret"                   "File name of the secret key to pretty-print."
  , command' "migrate-delegate-key-from"      "Migrate a delegate key from an older version." $
      MigrateDelegateKeyFrom
      <$> parseSystemVersion
      <*> parseFilePath    "to"                       "Output secret key file."
      <*> parseFilePath    "from"                     "Secret key file to migrate."
  , command' "dump-hardcoded-genesis"         "Write out a hard-coded genesis." $
      DumpHardcodedGenesis
      <$> parseFilePath    "genesis-output-dir"       "A yet-absent directory where genesis JSON file along with secrets shall be placed."
  , command' "print-genesis-hash"             "Compute hash of a genesis file." $
      PrintGenesisHash
      <$> parseFilePath    "genesis-json"             "Genesis JSON file to hash."
  , command' "signing-key-address"            "Print address of a signing key." $
      PrintSigningKeyAddress
      <$> parseNetworkMagic
      <*> parseFilePath    "secret"                   "Secret key, whose address is to be printed."
  , command' "keygen"                         "Generate a signing key." $
      Keygen
      <$> parseFilePath    "secret"                   "Non-existent file to write the secret key to."
      <*> parseFlag        "no-password"              "Disable password protection."
  , command' "to-verification"                "Extract a verification key in its base64 form." $
      ToVerification
      <$> parseFilePath    "secret"                   "Secret key file to extract from."
      <*> parseFilePath    "to"                       "Non-existent file to write the base64-formatted verification key to."
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
  ]
