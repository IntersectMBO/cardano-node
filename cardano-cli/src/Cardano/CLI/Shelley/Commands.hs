{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , renderShelleyCommand

    -- * CLI flag types
  , AddressKeyType (..)
  , ByronKeyType (..)
  , ByronKeyFormat (..)
  , CardanoAddressKeyType (..)
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
  , WitnessSigningData (..)
  ) where

import           Data.Text (Text)
import           Prelude

import           Cardano.Api.MetaData
import           Cardano.Api.Protocol (Protocol)
import           Cardano.Api.Typed hiding (PoolId)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import           Cardano.CLI.Shelley.Key (VerificationKeyOrHashOrFile)
import           Cardano.CLI.Types

import           Shelley.Spec.Ledger.TxBody (MIRPot)

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

renderShelleyCommand :: ShelleyCommand -> Text
renderShelleyCommand sc =
  case sc of
    AddressCmd cmd -> renderAddressCmd cmd
    StakeAddressCmd cmd -> renderStakeAddressCmd cmd
    KeyCmd cmd -> renderKeyCmd cmd
    TransactionCmd cmd -> renderTransactionCmd cmd
    NodeCmd cmd -> renderNodeCmd cmd
    PoolCmd cmd -> renderPoolCmd cmd
    QueryCmd cmd -> renderQueryCmd cmd
    GovernanceCmd cmd -> renderGovernanceCmd cmd
    GenesisCmd cmd -> renderGenesisCmd cmd
    TextViewCmd cmd -> renderTextViewCmd cmd

data AddressCmd
  = AddressKeyGen AddressKeyType VerificationKeyFile SigningKeyFile
  | AddressKeyHash VerificationKeyFile (Maybe OutputFile)
  | AddressBuild VerificationKeyFile (Maybe VerificationKeyFile) NetworkId (Maybe OutputFile)
  | AddressBuildMultiSig ScriptFile NetworkId (Maybe OutputFile)
  | AddressInfo Text (Maybe OutputFile)
  deriving (Eq, Show)


renderAddressCmd :: AddressCmd -> Text
renderAddressCmd cmd =
  case cmd of
    AddressKeyGen {} -> "address key-gen"
    AddressKeyHash {} -> "address key-hash"
    AddressBuild {} -> "address build"
    AddressBuildMultiSig {} -> "address build-script"
    AddressInfo {} -> "address info"

data StakeAddressCmd
  = StakeAddressKeyGen VerificationKeyFile SigningKeyFile
  | StakeAddressKeyHash VerificationKeyFile (Maybe OutputFile)
  | StakeAddressBuild VerificationKeyFile NetworkId (Maybe OutputFile)
  | StakeKeyRegistrationCert VerificationKeyFile OutputFile
  | StakeKeyDelegationCert VerificationKeyFile StakePoolVerificationKeyHashOrFile OutputFile
  | StakeKeyDeRegistrationCert VerificationKeyFile OutputFile
  deriving (Eq, Show)

renderStakeAddressCmd :: StakeAddressCmd -> Text
renderStakeAddressCmd cmd =
  case cmd of
    StakeAddressKeyGen {} -> "stake-address key-gen"
    StakeAddressKeyHash {} -> "stake-address key-hash"
    StakeAddressBuild {} -> "stake-address build"
    StakeKeyRegistrationCert {} -> "stake-address registration-certificate"
    StakeKeyDelegationCert {} -> "stake-address delegation-certificate"
    StakeKeyDeRegistrationCert {} -> "stake-address deregistration-certificate"

data KeyCmd
  = KeyGetVerificationKey SigningKeyFile VerificationKeyFile
  | KeyNonExtendedKey  VerificationKeyFile VerificationKeyFile
  | KeyConvertByronKey (Maybe Text) ByronKeyType SomeKeyFile OutputFile
  | KeyConvertByronGenesisVKey VerificationKeyBase64 OutputFile
  | KeyConvertITNStakeKey SomeKeyFile OutputFile
  | KeyConvertITNExtendedToStakeKey SomeKeyFile OutputFile
  | KeyConvertITNBip32ToStakeKey SomeKeyFile OutputFile
  | KeyConvertCardanoAddressSigningKey CardanoAddressKeyType SigningKeyFile OutputFile
  deriving (Eq, Show)

renderKeyCmd :: KeyCmd -> Text
renderKeyCmd cmd =
  case cmd of
    KeyGetVerificationKey {} -> "key verification-key"
    KeyNonExtendedKey {} -> "key non-extended-key"
    KeyConvertByronKey {} -> "key convert-byron-key"
    KeyConvertByronGenesisVKey {} -> "key convert-byron-genesis-key"
    KeyConvertITNStakeKey {} -> "key convert-itn-key"
    KeyConvertITNExtendedToStakeKey {} -> "key convert-itn-extended-key"
    KeyConvertITNBip32ToStakeKey {} -> "key convert-itn-bip32-key"
    KeyConvertCardanoAddressSigningKey {} -> "key convert-cardano-address-signing-key"

data TransactionCmd
  = TxBuildRaw
      [TxIn]
      [TxOut Shelley]
      SlotNo
      Lovelace
      [CertificateFile]
      [(StakeAddress, Lovelace)]
      TxMetadataJsonSchema
      [MetaDataFile]
      (Maybe UpdateProposalFile)
      TxBodyFile
  | TxSign TxBodyFile [WitnessSigningData] (Maybe NetworkId) TxFile
  | TxCreateWitness TxBodyFile WitnessSigningData (Maybe NetworkId) OutputFile
  | TxAssembleTxBodyWitness TxBodyFile [WitnessFile] OutputFile
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

renderTransactionCmd :: TransactionCmd -> Text
renderTransactionCmd cmd =
  case cmd of
    TxBuildRaw {} -> "transaction build-raw"
    TxSign {} -> "transaction sign"
    TxCreateWitness {} -> "transaction witness"
    TxAssembleTxBodyWitness {} -> "transaction sign-witness"
    TxSubmit {} -> "transaction submit"
    TxCalculateMinFee {} -> "transaction calculate-min-fee"
    TxGetTxId {} -> "transaction txid"

data NodeCmd
  = NodeKeyGenCold VerificationKeyFile SigningKeyFile OpCertCounterFile
  | NodeKeyGenKES  VerificationKeyFile SigningKeyFile
  | NodeKeyGenVRF  VerificationKeyFile SigningKeyFile
  | NodeKeyHashVRF  VerificationKeyFile (Maybe OutputFile)
  | NodeNewCounter  VerificationKeyFile Word OpCertCounterFile
  | NodeIssueOpCert VerificationKeyFile SigningKeyFile OpCertCounterFile
                    KESPeriod OutputFile
  deriving (Eq, Show)

renderNodeCmd :: NodeCmd -> Text
renderNodeCmd cmd = do
  case cmd of
    NodeKeyGenCold {} -> "node key-gen"
    NodeKeyGenKES {} -> "node key-gen-KES"
    NodeKeyGenVRF {} -> "node key-gen-VRF"
    NodeKeyHashVRF {} -> "node key-hash-VRF"
    NodeNewCounter {} -> "node new-counter"
    NodeIssueOpCert{} -> "node issue-op-cert"


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
      -- ^ Epoch in which to retire the stake pool.
      OutputFile
  | PoolGetId VerificationKeyFile OutputFormat
  | PoolMetaDataHash PoolMetaDataFile (Maybe OutputFile)
  deriving (Eq, Show)

renderPoolCmd :: PoolCmd -> Text
renderPoolCmd cmd =
  case cmd of
    PoolRegistrationCert {} -> "stake-pool registration-certificate"
    PoolRetirementCert {} -> "stake-pool deregistration-certificate"
    PoolGetId {} -> "stake-pool id"
    PoolMetaDataHash {} -> "stake-pool metadata-hash"

data QueryCmd =
    QueryProtocolParameters Protocol NetworkId (Maybe OutputFile)
  | QueryTip Protocol NetworkId (Maybe OutputFile)
  | QueryStakeDistribution Protocol NetworkId (Maybe OutputFile)
  | QueryStakeAddressInfo Protocol StakeAddress NetworkId (Maybe OutputFile)
  | QueryUTxO Protocol QueryFilter NetworkId (Maybe OutputFile)
  | QueryLedgerState Protocol NetworkId (Maybe OutputFile)
  deriving (Eq, Show)

renderQueryCmd :: QueryCmd -> Text
renderQueryCmd cmd =
  case cmd of
    QueryProtocolParameters {} -> "query protocol-parameters "
    QueryTip {} -> "query tip"
    QueryStakeDistribution {} -> "query stake-distribution"
    QueryStakeAddressInfo {} -> "query stake-address-info"
    QueryUTxO {} -> "query utxo"
    QueryLedgerState {} -> "query ledger-state"

data GovernanceCmd
  = GovernanceMIRCertificate MIRPot [VerificationKeyFile] [Lovelace] OutputFile
  | GovernanceGenesisKeyDelegationCertificate
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      OutputFile
  | GovernanceUpdateProposal OutputFile EpochNo
                             [VerificationKeyFile]
                             ProtocolParametersUpdate
  deriving (Eq, Show)

renderGovernanceCmd :: GovernanceCmd -> Text
renderGovernanceCmd cmd =
  case cmd of
    GovernanceGenesisKeyDelegationCertificate {} -> "governance create-genesis-key-delegation-certificate"
    GovernanceMIRCertificate {} -> "governance create-mir-certificate"
    GovernanceUpdateProposal {} -> "governance create-update-proposal"

data TextViewCmd
  = TextViewInfo !FilePath (Maybe OutputFile)
  deriving (Eq, Show)


renderTextViewCmd :: TextViewCmd -> Text
renderTextViewCmd (TextViewInfo _ _) = "text-view decode-cbor"

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

renderGenesisCmd :: GenesisCmd -> Text
renderGenesisCmd cmd =
  case cmd of
    GenesisCreate {} -> "genesis create"
    GenesisKeyGenGenesis {} -> "genesis key-gen-genesis"
    GenesisKeyGenDelegate {} -> "genesis key-gen-delegate"
    GenesisKeyGenUTxO {} -> "genesis key-gen-utxo"
    GenesisCmdKeyHash {} -> "genesis key-hash"
    GenesisVerKey {} -> "genesis get-ver-key"
    GenesisTxIn {} -> "genesis initial-txin"
    GenesisAddr {} -> "genesis initial-addr"
    GenesisHashFile {} -> "genesis hash"

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

-- | The type of @cardano-address@ key.
data CardanoAddressKeyType
  = CardanoAddressShelleyPaymentKey
  | CardanoAddressShelleyStakeKey
  | CardanoAddressIcarusPaymentKey
  | CardanoAddressByronPaymentKey
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

-- | Data required to construct a witness.
data WitnessSigningData
  = KeyWitnessSigningData
      !SigningKeyFile
      -- ^ Path to a file that should contain a signing key.
      !(Maybe (Address Byron))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.
  | ScriptWitnessSigningData !ScriptFile
  deriving (Eq, Show)
