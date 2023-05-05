{-# LANGUAGE DataKinds #-}
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
  , OpCertCounter
  , TxInCount (..)
  , TxOutCount (..)
  , TxShelleyWitnessCount (..)
  , TxByronWitnessCount (..)
  , SomeKeyFile (..)
  , OpCertCounterFile
  , ProtocolParamsFile (..)
  , WitnessFile (..)
  , TxFile
  , InputTxBodyOrTxFile (..)
  , VerificationKeyBase64 (..)
  , GenesisKeyFile (..)
  , MetadataFile (..)
  , PrivKeyFile (..)
  , BlockId (..)
  , WitnessSigningData (..)
  , ColdVerificationKeyOrFile (..)
  ) where

import           Prelude

import           Cardano.Api.Shelley

import           Data.Text (Text)

import           Cardano.CLI.Shelley.Key (DelegationTarget, PaymentVerifier, StakeIdentifier,
                   StakeVerifier, VerificationKeyOrFile, VerificationKeyOrHashOrFile,
                   VerificationKeyTextOrFile)
import           Cardano.CLI.Types

import           Cardano.Chain.Common (BlockCount)
import           Cardano.Ledger.Shelley.TxBody (MIRPot)
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
  = AddressKeyGen AddressKeyType (VerificationKeyFile Out) (SigningKeyFile Out)
  | AddressKeyHash VerificationKeyTextOrFile (Maybe (File () Out))
  | AddressBuild
      PaymentVerifier
      (Maybe StakeIdentifier)
      NetworkId
      (Maybe (File () Out))
  | AddressInfo Text (Maybe (File () Out))
  deriving Show


renderAddressCmd :: AddressCmd -> Text
renderAddressCmd cmd =
  case cmd of
    AddressKeyGen {} -> "address key-gen"
    AddressKeyHash {} -> "address key-hash"
    AddressBuild {} -> "address build"
    AddressInfo {} -> "address info"

data StakeAddressCmd
  = StakeAddressKeyGen (VerificationKeyFile Out) (SigningKeyFile Out)
  | StakeAddressKeyHash (VerificationKeyOrFile StakeKey) (Maybe (File () Out))
  | StakeAddressBuild StakeVerifier NetworkId (Maybe (File () Out))
  | StakeRegistrationCert StakeIdentifier (File () Out)
  | StakeCredentialDelegationCert
      StakeIdentifier
      DelegationTarget
      (File () Out)
  | StakeCredentialDeRegistrationCert StakeIdentifier (File () Out)
  deriving Show

renderStakeAddressCmd :: StakeAddressCmd -> Text
renderStakeAddressCmd cmd =
  case cmd of
    StakeAddressKeyGen {} -> "stake-address key-gen"
    StakeAddressKeyHash {} -> "stake-address key-hash"
    StakeAddressBuild {} -> "stake-address build"
    StakeRegistrationCert {} -> "stake-address registration-certificate"
    StakeCredentialDelegationCert {} -> "stake-address delegation-certificate"
    StakeCredentialDeRegistrationCert {} -> "stake-address deregistration-certificate"

data KeyCmd
  = KeyGetVerificationKey (SigningKeyFile In) (VerificationKeyFile Out)
  | KeyNonExtendedKey  (VerificationKeyFile In) (VerificationKeyFile Out)
  | KeyConvertByronKey (Maybe Text) ByronKeyType (SomeKeyFile In) (File () Out)
  | KeyConvertByronGenesisVKey VerificationKeyBase64 (File () Out)
  | KeyConvertITNStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertITNExtendedToStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertITNBip32ToStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertCardanoAddressSigningKey CardanoAddressKeyType (SigningKeyFile In) (File () Out)
  deriving Show

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
      AnyCardanoEra
      (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
      [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
      -- ^ Transaction inputs with optional spending scripts
      [TxIn]
      -- ^ Read only reference inputs
      [TxIn]
      -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
      (Maybe TxOutAnyEra)
      -- ^ Return collateral
      (Maybe Lovelace)
      -- ^ Total collateral
      [RequiredSigner]
      -- ^ Required signers
      [TxOutAnyEra]
      (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      (Maybe Lovelace)
      -- ^ Tx fee
      [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxiliary scripts
      [MetadataFile]
      (Maybe ProtocolParamsFile)
      (Maybe UpdateProposalFile)
      (TxBodyFile Out)

    -- | Like 'TxBuildRaw' but without the fee, and with a change output.
  | TxBuild
      SocketPath
      AnyCardanoEra
      AnyConsensusModeParams
      NetworkId
      (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
      (Maybe Word)
      -- ^ Override the required number of tx witnesses
      [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
      -- ^ Transaction inputs with optional spending scripts
      [TxIn]
      -- ^ Read only reference inputs
      [RequiredSigner]
      -- ^ Required signers
      [TxIn]
      -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
      (Maybe TxOutAnyEra)
      -- ^ Return collateral
      (Maybe Lovelace)
      -- ^ Total collateral
      [TxOutAnyEra]
      -- ^ Normal outputs
      TxOutChangeAddress
      -- ^ A change output
      (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Withdrawals with potential script witness
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxiliary scripts
      [MetadataFile]
      (Maybe ProtocolParamsFile)
      (Maybe UpdateProposalFile)
      TxBuildOutputOptions
  | TxSign InputTxBodyOrTxFile [WitnessSigningData] (Maybe NetworkId) (TxFile Out)
  | TxCreateWitness (TxBodyFile In) WitnessSigningData (Maybe NetworkId) (File () Out)
  | TxAssembleTxBodyWitness (TxBodyFile In) [WitnessFile] (File () Out)
  | TxSubmit SocketPath AnyConsensusModeParams NetworkId FilePath
  | TxMintedPolicyId ScriptFile
  | TxCalculateMinFee
      (TxBodyFile In)
      NetworkId
      ProtocolParamsFile
      TxInCount
      TxOutCount
      TxShelleyWitnessCount
      TxByronWitnessCount
  | TxCalculateMinRequiredUTxO
      AnyCardanoEra
      ProtocolParamsFile
      TxOutAnyEra
  | TxHashScriptData
      ScriptDataOrFile
  | TxGetTxId InputTxBodyOrTxFile
  | TxView InputTxBodyOrTxFile

data InputTxBodyOrTxFile = InputTxBodyFile (TxBodyFile In) | InputTxFile (TxFile In)
  deriving Show

renderTransactionCmd :: TransactionCmd -> Text
renderTransactionCmd cmd =
  case cmd of
    TxBuild {} -> "transaction build"
    TxBuildRaw {} -> "transaction build-raw"
    TxSign {} -> "transaction sign"
    TxCreateWitness {} -> "transaction witness"
    TxAssembleTxBodyWitness {} -> "transaction sign-witness"
    TxSubmit {} -> "transaction submit"
    TxMintedPolicyId {} -> "transaction policyid"
    TxCalculateMinFee {} -> "transaction calculate-min-fee"
    TxCalculateMinRequiredUTxO {} -> "transaction calculate-min-value"
    TxHashScriptData {} -> "transaction hash-script-data"
    TxGetTxId {} -> "transaction txid"
    TxView {} -> "transaction view"

data NodeCmd
  = NodeKeyGenCold (VerificationKeyFile Out) (SigningKeyFile Out) (OpCertCounterFile Out)
  | NodeKeyGenKES  (VerificationKeyFile Out) (SigningKeyFile Out)
  | NodeKeyGenVRF  (VerificationKeyFile Out) (SigningKeyFile Out)
  | NodeKeyHashVRF  (VerificationKeyOrFile VrfKey) (Maybe (File () Out))
  | NodeNewCounter ColdVerificationKeyOrFile Word (OpCertCounterFile InOut)
  | NodeIssueOpCert (VerificationKeyOrFile KesKey) (SigningKeyFile In) (OpCertCounterFile InOut)
                    KESPeriod (File () Out)
  deriving Show

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
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      (VerificationKeyOrFile VrfKey)
      -- ^ VRF Verification key.
      Lovelace
      -- ^ Pool pledge.
      Lovelace
      -- ^ Pool cost.
      Rational
      -- ^ Pool margin.
      (VerificationKeyOrFile StakeKey)
      -- ^ Reward account verification staking key.
      [VerificationKeyOrFile StakeKey]
      -- ^ Pool owner verification staking key(s).
      [StakePoolRelay]
      -- ^ Stake pool relays.
      (Maybe StakePoolMetadataReference)
      -- ^ Stake pool metadata.
      NetworkId
      (File () Out)
  | PoolRetirementCert
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool.
      (File Certificate Out)
  | PoolGetId (VerificationKeyOrFile StakePoolKey) OutputFormat
  | PoolMetadataHash (File StakePoolMetadata In) (Maybe (File () Out))
  deriving Show

renderPoolCmd :: PoolCmd -> Text
renderPoolCmd cmd =
  case cmd of
    PoolRegistrationCert {} -> "stake-pool registration-certificate"
    PoolRetirementCert {} -> "stake-pool deregistration-certificate"
    PoolGetId {} -> "stake-pool id"
    PoolMetadataHash {} -> "stake-pool metadata-hash"

data QueryCmd =
    QueryLeadershipSchedule
      SocketPath
      AnyConsensusModeParams
      NetworkId
      GenesisFile
      (VerificationKeyOrHashOrFile StakePoolKey)
      (SigningKeyFile In)
      EpochLeadershipSchedule
      (Maybe (File () Out))
  | QueryProtocolParameters' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryTip SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryStakePools' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryStakeDistribution' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryStakeAddressInfo SocketPath AnyConsensusModeParams StakeAddress NetworkId (Maybe (File () Out))
  | QueryUTxO' SocketPath AnyConsensusModeParams QueryUTxOFilter NetworkId (Maybe (File () Out))
  | QueryDebugLedgerState' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryProtocolState' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryStakeSnapshot'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (AllOrOnly [Hash StakePoolKey])
      (Maybe (File () Out))
  | QueryKesPeriodInfo
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (File () In)
      -- ^ Node operational certificate
      (Maybe (File () Out))
  | QueryPoolState' SocketPath AnyConsensusModeParams NetworkId [Hash StakePoolKey]
  | QueryTxMempool SocketPath AnyConsensusModeParams NetworkId TxMempoolQuery (Maybe (File () Out))
  deriving Show

renderQueryCmd :: QueryCmd -> Text
renderQueryCmd cmd =
  case cmd of
    QueryLeadershipSchedule {} -> "query leadership-schedule"
    QueryProtocolParameters' {} -> "query protocol-parameters "
    QueryTip {} -> "query tip"
    QueryStakePools' {} -> "query stake-pools"
    QueryStakeDistribution' {} -> "query stake-distribution"
    QueryStakeAddressInfo {} -> "query stake-address-info"
    QueryUTxO' {} -> "query utxo"
    QueryDebugLedgerState' {} -> "query ledger-state"
    QueryProtocolState' {} -> "query protocol-state"
    QueryStakeSnapshot' {} -> "query stake-snapshot"
    QueryKesPeriodInfo {} -> "query kes-period-info"
    QueryPoolState' {} -> "query pool-state"
    QueryTxMempool _ _ _ query _ -> "query tx-mempool" <> renderTxMempoolQuery query
  where
    renderTxMempoolQuery query =
      case query of
        TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
        TxMempoolQueryNextTx -> "next-tx"
        TxMempoolQueryInfo -> "info"


data GovernanceCmd
  = GovernanceMIRPayStakeAddressesCertificate
      MIRPot
      [StakeAddress]
      [Lovelace]
      (File () Out)
  | GovernanceMIRTransfer Lovelace (File () Out) TransferDirection
  | GovernanceGenesisKeyDelegationCertificate
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      (File () Out)
  | GovernanceUpdateProposal (File () Out) EpochNo
                             [VerificationKeyFile In]
                             ProtocolParametersUpdate
                             (Maybe FilePath)
  | GovernanceCreatePoll
      Text -- Prompt
      [Text] -- Choices
      (Maybe Word) -- Nonce
      (File GovernancePoll Out)
  | GovernanceAnswerPoll
      (File GovernancePoll In) -- Poll file
      (Maybe Word) -- Answer index
      (Maybe (File () Out)) -- Tx file
  | GovernanceVerifyPoll
      (File GovernancePoll In) -- Poll file
      (File (Tx ()) In) -- Tx file
      (Maybe (File () Out)) -- Tx file
  deriving Show

renderGovernanceCmd :: GovernanceCmd -> Text
renderGovernanceCmd cmd =
  case cmd of
    GovernanceGenesisKeyDelegationCertificate {} -> "governance create-genesis-key-delegation-certificate"
    GovernanceMIRPayStakeAddressesCertificate {} -> "governance create-mir-certificate stake-addresses"
    GovernanceMIRTransfer _ _ TransferToTreasury -> "governance create-mir-certificate transfer-to-treasury"
    GovernanceMIRTransfer _ _ TransferToReserves -> "governance create-mir-certificate transfer-to-reserves"
    GovernanceUpdateProposal {} -> "governance create-update-proposal"
    GovernanceCreatePoll{} -> "governance create-poll"
    GovernanceAnswerPoll{} -> "governance answer-poll"
    GovernanceVerifyPoll{} -> "governance verify-poll"

data TextViewCmd
  = TextViewInfo !FilePath (Maybe (File () Out))
  deriving Show


renderTextViewCmd :: TextViewCmd -> Text
renderTextViewCmd (TextViewInfo _ _) = "text-view decode-cbor"

data GenesisCmd
  = GenesisCreate GenesisDir Word Word (Maybe SystemStart) (Maybe Lovelace) NetworkId
  | GenesisCreateCardano GenesisDir Word Word (Maybe SystemStart) (Maybe Lovelace) BlockCount Word Rational NetworkId FilePath FilePath FilePath FilePath (Maybe FilePath)
  | GenesisCreateStaked
      GenesisDir
      Word
      Word
      Word
      Word
      (Maybe SystemStart)
      (Maybe Lovelace)
      Lovelace
      NetworkId
      Word
      Word
      Word
      (Maybe FilePath) -- ^ Relay specification filepath
  | GenesisKeyGenGenesis (VerificationKeyFile Out) (SigningKeyFile Out)
  | GenesisKeyGenDelegate (VerificationKeyFile Out) (SigningKeyFile Out) (OpCertCounterFile Out)
  | GenesisKeyGenUTxO (VerificationKeyFile Out) (SigningKeyFile Out)
  | GenesisCmdKeyHash (VerificationKeyFile In)
  | GenesisVerKey (VerificationKeyFile Out) (SigningKeyFile In)
  | GenesisTxIn (VerificationKeyFile In) NetworkId (Maybe (File () Out))
  | GenesisAddr (VerificationKeyFile In) NetworkId (Maybe (File () Out))
  | GenesisHashFile GenesisFile
  deriving Show

renderGenesisCmd :: GenesisCmd -> Text
renderGenesisCmd cmd =
  case cmd of
    GenesisCreate {} -> "genesis create"
    GenesisCreateCardano {} -> "genesis create-cardano"
    GenesisCreateStaked {} -> "genesis create-staked"
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
  deriving Show

newtype TxOutCount
  = TxOutCount Int
  deriving Show

newtype TxShelleyWitnessCount
  = TxShelleyWitnessCount Int
  deriving Show

newtype TxByronWitnessCount
  = TxByronWitnessCount Int
  deriving Show

newtype BlockId
  = BlockId String -- Probably not a String
  deriving Show

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving Show

data MetadataFile = MetadataFileJSON (File () In)
                  | MetadataFileCBOR (File () In)

  deriving Show

newtype GenesisDir
  = GenesisDir FilePath
  deriving Show

-- | Either a verification or signing key, used for conversions and other
-- commands that make sense for both.
--
data SomeKeyFile direction
  = AVerificationKeyFile (VerificationKeyFile direction)
  | ASigningKeyFile (SigningKeyFile direction)
  deriving Show

data AddressKeyType
  = AddressKeyShelley
  | AddressKeyShelleyExtended
  | AddressKeyByron
  deriving Show

data ByronKeyType
  = ByronPaymentKey  ByronKeyFormat
  | ByronGenesisKey  ByronKeyFormat
  | ByronDelegateKey ByronKeyFormat
  deriving Show

data ByronKeyFormat = NonLegacyByronKeyFormat
                    | LegacyByronKeyFormat
  deriving Show

-- | The type of @cardano-address@ key.
data CardanoAddressKeyType
  = CardanoAddressShelleyPaymentKey
  | CardanoAddressShelleyStakeKey
  | CardanoAddressIcarusPaymentKey
  | CardanoAddressByronPaymentKey
  deriving Show

data OpCertCounter

type OpCertCounterFile = File OpCertCounter

newtype PrivKeyFile
  = PrivKeyFile FilePath
  deriving Show

newtype WitnessFile
  = WitnessFile FilePath
  deriving Show

-- | A raw verification key given in Base64, and decoded into a ByteString.
newtype VerificationKeyBase64
  = VerificationKeyBase64 String
  deriving Show

-- | Data required to construct a witness.
data WitnessSigningData
  = KeyWitnessSigningData
      !(SigningKeyFile In)
      -- ^ Path to a file that should contain a signing key.
      !(Maybe (Address ByronAddr))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.
  deriving Show

-- | Either a stake pool verification key, genesis delegate verification key,
-- or a path to a cold verification key file.
--
-- Note that a "cold verification key" refers to either a stake pool or
-- genesis delegate verification key.
--
-- TODO: A genesis delegate extended key should also be valid here.
data ColdVerificationKeyOrFile
  = ColdStakePoolVerificationKey !(VerificationKey StakePoolKey)
  | ColdGenesisDelegateVerificationKey !(VerificationKey GenesisDelegateKey)
  | ColdVerificationKeyFile !(VerificationKeyFile In)
  deriving Show
