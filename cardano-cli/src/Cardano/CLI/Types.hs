{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Types
  ( CertificateFile (..)
  , NodeAddress (..)
  , NodeHostAddress (..)
  , QueryFilter (..)
  , SigningKeyFile (..)
  , SocketPath (..)
  , StakePoolVerificationKeyHashOrFile (..)
  , UpdateProposalFile (..)
  , VerificationKeyFile (..)
  ) where

import           Cardano.Prelude

import           Data.IP (IP)
import           Network.Socket (PortNumber)

import           Cardano.Api.Typed

-- Encompasses stake certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile { unCertificateFile :: FilePath }
                          deriving newtype (Eq, Show)

-- | IPv4 address with a port number.
data NodeAddress = NodeAddress
  { naHostAddress :: !NodeHostAddress
  , naPort :: !PortNumber
  } deriving (Eq, Ord, Show)

-- Embedding a Maybe inside a newtype is somewhat icky but this seems to work
-- and removing the Maybe breaks the functionality in a subtle way that is difficult
-- to diagnose.
newtype NodeHostAddress
  = NodeHostAddress { unNodeHostAddress :: Maybe IP }
  deriving newtype Show
  deriving (Eq, Ord)

-- | UTxO query filtering options.
data QueryFilter
  = FilterByAddress !(Set (Address Shelley))
  | NoFilter
  deriving (Eq, Show)

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile ::  FilePath }
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
