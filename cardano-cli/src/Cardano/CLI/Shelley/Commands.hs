
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
  | AddressBuildMultiSig  --TODO
  | AddressInfo Text (Maybe OutputFile)
  deriving (Eq, Show)


renderAddressCmd :: AddressCmd -> Text
renderAddressCmd cmd =
  case cmd of
    (AddressKeyGen _ _ _) -> "address key-gen"
    (AddressKeyHash _ _) -> "address key-hash"
    (AddressBuild _ _ _ _) -> "address build"
    (AddressBuildMultiSig) -> "address build-multisig"
    (AddressInfo _ _) -> "address info"

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
    StakeAddressKeyGen _ _ -> "stake-address key-gen"
    StakeAddressKeyHash _ _ -> "stake-address key-hash"
    StakeAddressBuild _ _ _ -> "stake-address build"
    StakeKeyRegistrationCert _ _ -> "stake-address registration-certificate"
    StakeKeyDelegationCert _ _ _ -> "stake-address delegation-certificate"
    StakeKeyDeRegistrationCert _ _ -> "stake-address deregistration-certificate"

data KeyCmd
  = KeyGetVerificationKey SigningKeyFile VerificationKeyFile
  | KeyNonExtendedKey  VerificationKeyFile VerificationKeyFile
  | KeyConvertByronKey (Maybe Text) ByronKeyType SomeKeyFile OutputFile
  | KeyConvertByronGenesisVKey VerificationKeyBase64 OutputFile
  | KeyConvertITNStakeKey SomeKeyFile OutputFile
  | KeyConvertITNExtendedToStakeKey SomeKeyFile OutputFile
  | KeyConvertITNBip32ToStakeKey SomeKeyFile OutputFile
  deriving (Eq, Show)

renderKeyCmd :: KeyCmd -> Text
renderKeyCmd cmd =
  case cmd of
    KeyGetVerificationKey _ _ -> "key verification-key"
    KeyNonExtendedKey  _ _ -> "key non-extended-key"
    KeyConvertByronKey _ _ _ _ -> "key convert-byron-key"
    KeyConvertByronGenesisVKey _ _ -> "key convert-byron-genesis-key"
    KeyConvertITNStakeKey _ _ -> "key convert-itn-key"
    KeyConvertITNExtendedToStakeKey _ _ -> "key convert-itn-extended-key"
    KeyConvertITNBip32ToStakeKey _ _ -> "key convert-itn-bip32-key"

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

renderTransactionCmd :: TransactionCmd -> Text
renderTransactionCmd cmd =
  case cmd of
    TxBuildRaw _ _ _ _ _ _ _ _ _ -> "transaction build-raw"
    TxSign _ _ _ _ -> "transaction sign"
    TxWitness _ _ _ _ -> "transaction witness"
    TxSignWitness _ _ _ -> "transaction sign-witness"
    TxSubmit _ _ _ -> "transaction submit"
    TxCalculateMinFee _ _ _ _ _ _ _ -> "transaction calculate-min-fee"
    TxGetTxId _ -> "transaction txid"

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
    NodeKeyGenCold _ _ _ -> "node key-gen"
    NodeKeyGenKES  _ _ -> "node key-gen-KES"
    NodeKeyGenVRF  _ _ -> "node key-gen-VRF"
    NodeKeyHashVRF  _ _ -> "node key-hash-VRF"
    NodeNewCounter  _ _ _ -> "node new-counter"
    NodeIssueOpCert _ _ _ _ _ -> "node issue-op-cert"


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

renderPoolCmd :: PoolCmd -> Text
renderPoolCmd cmd =
  case cmd of
    PoolRegistrationCert _ _ _ _ _ _ _ _ _ _ _ -> "stake-pool registration-certificate"
    PoolRetirementCert _ _ _ -> "stake-pool deregistration-certificate"
    PoolGetId _ _ -> "stake-pool id"
    PoolMetaDataHash _ _ -> "stake-pool metadata-hash"

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
    QueryProtocolParameters _ _ _ -> "query protocol-parameters "
    QueryTip _ _ _ -> "query tip"
    QueryStakeDistribution _ _ _ -> "query stake-distribution"
    QueryStakeAddressInfo _ _ _ _ -> "query stake-address-info"
    QueryUTxO _ _ _ _ -> "query utxo"
    QueryLedgerState _ _ _ -> "query ledger-state"

data GovernanceCmd
  = GovernanceMIRCertificate MIRPot [VerificationKeyFile] [Lovelace] OutputFile
  | GovernanceUpdateProposal OutputFile EpochNo
                             [VerificationKeyFile]
                             ProtocolParametersUpdate
  deriving (Eq, Show)

renderGovernanceCmd :: GovernanceCmd -> Text
renderGovernanceCmd cmd =
  case cmd of
    GovernanceMIRCertificate _ _ _ _ -> "governance create-mir-certificate"
    GovernanceUpdateProposal _ _ _ _ -> "governance create-update-proposal"

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
    GenesisCreate _ _ _ _ _ _-> "genesis create"
    GenesisKeyGenGenesis _ _ -> "genesis key-gen-genesis"
    GenesisKeyGenDelegate _ _ _ -> "genesis key-gen-delegate"
    GenesisKeyGenUTxO _ _ -> "genesis key-gen-utxo"
    GenesisCmdKeyHash _ -> "genesis key-hash"
    GenesisVerKey _ _ -> "genesis get-ver-key"
    GenesisTxIn _ _ _ -> "genesis initial-txin"
    GenesisAddr _ _ _ -> "genesis initial-addr"
    GenesisHashFile _ -> "genesis hash"

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
