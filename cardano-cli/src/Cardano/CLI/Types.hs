{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Types
  ( CBORObject (..)
  , CertificateFile (..)
  , GenesisFile (..)
  , OutputFormat (..)
  , QueryFilter (..)
  , SigningKeyFile (..)
  , SocketPath (..)
  , StakePoolVerificationKeyHashOrFile (..)
  , ScriptFile (..)
  , UpdateProposalFile (..)
  , VerificationKeyFile (..)
  ) where

import           Cardano.Prelude

import           Data.Aeson
import qualified Data.Text as Text

import qualified Cardano.Chain.Slotting as Byron

import           Cardano.Api.Typed

-- | Specify what the CBOR file is
-- i.e a block, a tx, etc
data CBORObject = CBORBlockByron Byron.EpochSlots
                | CBORDelegationCertificateByron
                | CBORTxByron
                | CBORUpdateProposalByron
                | CBORVoteByron
                deriving Show

-- Encompasses stake certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile { unCertificateFile :: FilePath }
                          deriving newtype (Eq, Show)

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> (Text.pack $ show invalid)

-- | The desired output format.
data OutputFormat
  = OutputFormatHex
  | OutputFormatBech32
  deriving (Eq, Show)

-- | UTxO query filtering options.
data QueryFilter
  = FilterByAddress !(Set (Address Shelley))
  | NoFilter
  deriving (Eq, Show)

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

newtype SocketPath = SocketPath { unSocketPath :: FilePath }

-- | Either a stake pool verification key hash or verification key file.
data StakePoolVerificationKeyHashOrFile
  = StakePoolVerificationKeyHash !(Hash StakePoolKey)
  | StakePoolVerificationKeyFile !VerificationKeyFile
  deriving (Eq, Show)

newtype UpdateProposalFile = UpdateProposalFile { unUpdateProposalFile :: FilePath }
                             deriving newtype (Eq, Show)

newtype VerificationKeyFile
  = VerificationKeyFile FilePath
  deriving (Eq, Show)

newtype ScriptFile = ScriptFile { unScriptFile :: FilePath }
                     deriving (Eq, Show)
