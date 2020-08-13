
-- | Shelley CLI command types
module Cardano.CLI.Shelley.Commands
  ( -- * CLI command types
    ShelleyCommand (..)
  , AddressCmd (..)
  , StakeAddressCmd (..)
  , KeyCmd (..)
  , TransactionCmd (..)
  , NodeCmd (..)
  , PoolCmd (..)
  , QueryCmd (..)
  , GovernanceCmd (..)
  , GenesisCmd (..)
  , TextViewCmd (..)

    -- * CLI flag types
  , AddressKeyType (..)
  , ByronKeyType (..)
  , ByronKeyFormat (..)
  , GenesisDir (..)
  , TxInCount (..)
  , TxOutCount (..)
  , TxShelleyWitnessCount (..)
  , TxByronWitnessCount (..)
  , SomeKeyFile (..)
  , OpCertCounterFile (..)
  , OutputFile (..)
  , ProtocolParamsFile (..)
  , WitnessFile (..)
  , TxBodyFile (..)
  , TxFile (..)
  , VerificationKeyBase64 (..)
  , GenesisKeyFile (..)
  , MetaDataFile (..)
  , PoolId (..)
  , PoolMetaDataFile (..)
  , PrivKeyFile (..)
  , BlockId (..)
  ) where

import           Data.Text (Text)
import           Prelude

import           Cardano.Api.Protocol (Protocol)
import           Cardano.Api.Typed hiding (PoolId)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import           Cardano.CLI.Types

import           Shelley.Spec.Ledger.TxData (MIRPot)

--
-- Shelley CLI command data types
--

-- | All the CLI subcommands under \"shelley\".
--
data ShelleyCommand
  = AddressCmd      AddressCmd
  | StakeAddressCmd StakeAddressCmd
  | KeyCmd          KeyCmd
  | TransactionCmd  TransactionCmd
  | NodeCmd         NodeCmd
  | PoolCmd         PoolCmd
  | QueryCmd        QueryCmd
  | GovernanceCmd   GovernanceCmd
  | GenesisCmd      GenesisCmd
  | TextViewCmd     TextViewCmd
  deriving (Eq, Show)


data AddressCmd
  = AddressKeyGen AddressKeyType VerificationKeyFile SigningKeyFile
  | AddressKeyHash VerificationKeyFile (Maybe OutputFile)
  | AddressBuild VerificationKeyFile (Maybe VerificationKeyFile) NetworkId (Maybe OutputFile)
  | AddressBuildMultiSig  --TODO
  | AddressBuildScript ScriptFile NetworkId (Maybe OutputFile)
  | AddressInfo Text (Maybe OutputFile)
  deriving (Eq, Show)

data StakeAddressCmd
  = StakeAddressKeyGen VerificationKeyFile SigningKeyFile
  | StakeAddressKeyHash VerificationKeyFile (Maybe OutputFile)
  | StakeAddressBuild VerificationKeyFile NetworkId (Maybe OutputFile)
  | StakeKeyRegistrationCert VerificationKeyFile OutputFile
  | StakeKeyDelegationCert VerificationKeyFile StakePoolVerificationKeyHashOrFile OutputFile
  | StakeKeyDeRegistrationCert VerificationKeyFile OutputFile
  deriving (Eq, Show)

data KeyCmd
  = KeyGetVerificationKey SigningKeyFile VerificationKeyFile
  | KeyNonExtendedKey  VerificationKeyFile VerificationKeyFile
  | KeyConvertByronKey (Maybe Text) ByronKeyType SomeKeyFile OutputFile
  | KeyConvertByronGenesisVKey VerificationKeyBase64 OutputFile
  | KeyConvertITNStakeKey SomeKeyFile OutputFile
  | KeyConvertITNExtendedToStakeKey SomeKeyFile OutputFile
  | KeyConvertITNBip32ToStakeKey SomeKeyFile OutputFile
  deriving (Eq, Show)

data TransactionCmd
  = TxBuildRaw
      [TxIn]
      [TxOut Shelley]
      SlotNo
      Lovelace
      [CertificateFile]
      [(StakeAddress, Lovelace)]
      [MetaDataFile]
      (Maybe UpdateProposalFile)
      TxBodyFile
  | TxSign TxBodyFile [SigningKeyFile] (Maybe NetworkId) TxFile
  | TxWitness TxBodyFile SigningKeyFile (Maybe NetworkId) OutputFile
  | TxSignWitness TxBodyFile [WitnessFile] OutputFile
  | TxSubmit Protocol NetworkId FilePath
  | TxCalculateMinFee
      TxBodyFile
      (Maybe NetworkId)
      ProtocolParamsFile
      TxInCount
      TxOutCount
      TxShelleyWitnessCount
      TxByronWitnessCount
  | TxGetTxId TxBodyFile
  deriving (Eq, Show)


data NodeCmd
  = NodeKeyGenCold VerificationKeyFile SigningKeyFile OpCertCounterFile
  | NodeKeyGenKES  VerificationKeyFile SigningKeyFile
  | NodeKeyGenVRF  VerificationKeyFile SigningKeyFile
  | NodeKeyHashVRF  VerificationKeyFile (Maybe OutputFile)
  | NodeNewCounter  VerificationKeyFile Word OpCertCounterFile
  | NodeIssueOpCert VerificationKeyFile SigningKeyFile OpCertCounterFile
                    KESPeriod OutputFile
  deriving (Eq, Show)

data PoolCmd
  = PoolRegistrationCert
      VerificationKeyFile
      -- ^ Stake pool verification key.
      VerificationKeyFile
      -- ^ VRF Verification key.
      Lovelace
      -- ^ Pool pledge.
      Lovelace
      -- ^ Pool cost.
      Rational
      -- ^ Pool margin.
      VerificationKeyFile
      -- ^ Reward account verification staking key.
      [VerificationKeyFile]
      -- ^ Pool owner verification staking key(s).
      [StakePoolRelay]
      -- ^ Stake pool relays.
      (Maybe StakePoolMetadataReference)
      -- ^ Stake pool metadata.
      NetworkId
      OutputFile
  | PoolRetirementCert
      VerificationKeyFile
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool. --TODO: Double check this
      OutputFile
  | PoolGetId VerificationKeyFile OutputFormat
  | PoolMetaDataHash PoolMetaDataFile (Maybe OutputFile)
  deriving (Eq, Show)


data QueryCmd =
    QueryProtocolParameters Protocol NetworkId (Maybe OutputFile)
  | QueryTip Protocol NetworkId (Maybe OutputFile)
  | QueryStakeDistribution Protocol NetworkId (Maybe OutputFile)
  | QueryStakeAddressInfo Protocol StakeAddress NetworkId (Maybe OutputFile)
  | QueryUTxO Protocol QueryFilter NetworkId (Maybe OutputFile)
  | QueryLedgerState Protocol NetworkId (Maybe OutputFile)
  deriving (Eq, Show)

data GovernanceCmd
  = GovernanceMIRCertificate MIRPot [VerificationKeyFile] [Lovelace] OutputFile
  | GovernanceUpdateProposal OutputFile EpochNo
                             [VerificationKeyFile]
                             ProtocolParametersUpdate
  deriving (Eq, Show)


data TextViewCmd
  = TextViewInfo !FilePath (Maybe OutputFile)
  deriving (Eq, Show)

data GenesisCmd
  = GenesisCreate GenesisDir Word Word (Maybe SystemStart) (Maybe Lovelace) NetworkId
  | GenesisKeyGenGenesis VerificationKeyFile SigningKeyFile
  | GenesisKeyGenDelegate VerificationKeyFile SigningKeyFile OpCertCounterFile
  | GenesisKeyGenUTxO VerificationKeyFile SigningKeyFile
  | GenesisCmdKeyHash VerificationKeyFile
  | GenesisVerKey VerificationKeyFile SigningKeyFile
  | GenesisTxIn VerificationKeyFile NetworkId (Maybe OutputFile)
  | GenesisAddr VerificationKeyFile NetworkId (Maybe OutputFile)
  | GenesisHashFile GenesisFile
  deriving (Eq, Show)

--
-- Shelley CLI flag/option data types
--

newtype ProtocolParamsFile
  = ProtocolParamsFile FilePath
  deriving (Show, Eq)

newtype TxInCount
  = TxInCount Int
  deriving (Eq, Show)

newtype TxOutCount
  = TxOutCount Int
  deriving (Eq, Show)

newtype TxShelleyWitnessCount
  = TxShelleyWitnessCount Int
  deriving (Eq, Show)

newtype TxByronWitnessCount
  = TxByronWitnessCount Int
  deriving (Eq, Show)

newtype BlockId
  = BlockId String -- Probably not a String
  deriving (Eq, Show)

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving (Eq, Show)

data MetaDataFile = MetaDataFileJSON FilePath
                  | MetaDataFileCBOR FilePath

  deriving (Eq, Show)

newtype OutputFile
  = OutputFile FilePath
  deriving (Eq, Show)

newtype PoolId
  = PoolId String -- Probably not a String
  deriving (Eq, Show)

newtype PoolMetaDataFile = PoolMetaDataFile
  { unPoolMetaDataFile :: FilePath }
  deriving (Eq, Show)

newtype GenesisDir
  = GenesisDir FilePath
  deriving (Eq, Show)

-- | Either a verification or signing key, used for conversions and other
-- commands that make sense for both.
--
data SomeKeyFile
  = AVerificationKeyFile VerificationKeyFile
  | ASigningKeyFile SigningKeyFile
  deriving (Eq, Show)

data AddressKeyType
  = AddressKeyShelley
  | AddressKeyShelleyExtended
  | AddressKeyByron
  deriving (Eq, Show)

data ByronKeyType
  = ByronPaymentKey  ByronKeyFormat
  | ByronGenesisKey  ByronKeyFormat
  | ByronDelegateKey ByronKeyFormat
  deriving (Eq, Show)

data ByronKeyFormat = NonLegacyByronKeyFormat
                    | LegacyByronKeyFormat
  deriving (Eq, Show)

newtype OpCertCounterFile
  = OpCertCounterFile FilePath
  deriving (Eq, Show)

newtype PrivKeyFile
  = PrivKeyFile FilePath
  deriving (Eq, Show)

newtype WitnessFile
  = WitnessFile FilePath
  deriving (Eq, Show)

newtype TxBodyFile
  = TxBodyFile FilePath
  deriving (Eq, Show)

newtype TxFile
  = TxFile FilePath
  deriving (Eq, Show)

-- | A raw verification key given in Base64, and decoded into a ByteString.
newtype VerificationKeyBase64
  = VerificationKeyBase64 String
  deriving (Eq, Show)
