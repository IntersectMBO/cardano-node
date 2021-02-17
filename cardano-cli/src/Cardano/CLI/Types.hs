{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Types
  ( CBORObject (..)
  , CertificateFile (..)
  , Datum (..)
  , ExecutionUnits(..)
  , GenesisFile (..)
  , ScriptBundle(..)
  , NonNativeScriptFile(..)
  , PlutusScriptType(..)
  , OutputFormat (..)
  , PlutusTag (..)
  , PlutusScriptRequirements(..)
  , ProtocolParamsFile( ..)
  , QueryFilter (..)
  , Redeemer (..)
  , SigningKeyFile (..)
  , SigningKeyOrScriptFile (..)
  , SocketPath (..)
  , ScriptFile (..)
  , TxInAnyEra (..)
  , TxOutAnyEra (..)
  , UpdateProposalFile (..)
  , VerificationKeyFile (..)
  ) where


import           Cardano.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import qualified Cardano.Chain.Slotting as Byron

import           Cardano.Api

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
  parseJSON (Aeson.String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> Text.pack (show invalid)

-- | The desired output format.
data OutputFormat
  = OutputFormatHex
  | OutputFormatBech32
  deriving (Eq, Show)

-- | UTxO query filtering options.
data QueryFilter
  = FilterByAddress !(Set AddressAny)
  | NoFilter
  deriving (Eq, Show)

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

newtype SocketPath = SocketPath { unSocketPath :: FilePath }

newtype UpdateProposalFile = UpdateProposalFile { unUpdateProposalFile :: FilePath }
                             deriving newtype (Eq, Show)

newtype VerificationKeyFile
  = VerificationKeyFile { unVerificationKeyFile :: FilePath }
  deriving (Eq, Show)

newtype ScriptFile = ScriptFile { unScriptFile :: FilePath }
                     deriving (Eq, Show)

data SigningKeyOrScriptFile = ScriptFileForWitness FilePath
                            | SigningKeyFileForWitness FilePath
                            deriving (Eq, Show)

-- | A TxOut value that is the superset of possibilities for any era: any
-- address type and allowing multi-asset values. This is used as the type for
-- values passed on the command line. It can be converted into the
-- era-dependent 'TxOutValue' type.
--
data TxOutAnyEra = TxOutAnyEra AddressAny Value
  deriving (Eq, Show)

data TxInAnyEra = TxInAnyEra TxId TxIx PlutusTag
  deriving Show

newtype ProtocolParamsFile = ProtocolParamsFile FilePath
  deriving (Show, Eq)

data PlutusTag = IsPlutusFee | IsNotPlutusFee
  deriving Show

-- Optional Datum when spending from a
-- Plutus script locked UTxO
newtype Datum = Datum { unDatum :: FilePath } deriving Show

newtype Redeemer = Redeemer { unRedeemer :: FilePath } deriving Show

-- ScriptBundle can be any script type (native, plutus etc) and version.
data ScriptBundle
  = ScriptBundle
      { scriptBundleFile :: ScriptFile
        -- ^ Filepath of the script
      , scriptBundlePlutus :: Maybe PlutusScriptRequirements
        -- ^ Plutus script additional requirements.
      } deriving Show

data PlutusScriptRequirements
  = PlutusScriptRequirements
      { plutusScriptType :: PlutusScriptType
        -- ^ What the Plutus script will do
      , plutusScriptExecutionUnits :: ExecutionUnits
        -- ^ Arbitrary execution unit in which we measure the cost of scripts.
      , plutusScriptTxIns :: [TxInAnyEra]
        -- ^ Script fees
      , plutusScriptRedeemers :: [Redeemer]
      , plutusScriptDatum :: Maybe Datum
      } deriving Show

-- | The different types of Plutus scripts
--and what they do.
data PlutusScriptType
  = Spending TxInAnyEra
    -- ^ Validates spending a script-locked UTxO
  | Minting PolicyId
    -- ^ Validates minting new tokens
  | Rewarding StakeAddress
    -- ^ Validates certificate transactions
  | Certifying FilePath
    -- ^ Validates withdrawl from a reward account
  deriving Show


newtype NonNativeScriptFile = NonNativeScriptFile { unNonNativeScriptFile :: FilePath }

-- | Arbitrary execution unit in which we measure the cost of scripts.
data ExecutionUnits = ExecutionUnits
                        -- ^ Memory
                        Word64
                        -- ^ Steps
                        Word64
                      deriving Show

instance Semigroup ExecutionUnits where
  ExecutionUnits a c <> ExecutionUnits b d = ExecutionUnits (a + b) (c + d)

instance Monoid ExecutionUnits where
  mempty = ExecutionUnits 0 0
