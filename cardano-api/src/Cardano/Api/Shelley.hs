-- | This module provides a library interface that is intended to be
-- the complete API for Shelley covering everything, including
-- exposing constructors for the lower level types.
--

module Cardano.Api.Shelley
  ( module Cardano.API,
    -- * Era
    Shelley,
    HasTypeProxy(..),
    AsType(AsShelleyAddress,
           AsShelleyTxBody,
           AsShelleyTx,
           AsShelleyWitness),

    -- * Cryptographic key interface
    -- $keys
    VerificationKey(..),

    -- * Payment addresses
    -- | Constructing and inspecting Shelley payment addresses
    Address(ShelleyAddress),
    NetworkId(Mainnet, Testnet),

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody(ShelleyTxBody),
    TxId(TxId),
    TxIn(TxIn),
    TxOut(TxOut),
    TxIx(TxIx),
    Lovelace(Lovelace),
    SlotNo(SlotNo),

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(ShelleyTx),

    -- ** Incremental signing and separate witnesses
    Witness
      ( ShelleyBootstrapWitness
      , ShelleyKeyWitness
      , ShelleyScriptWitness
      ),
    ShelleyWitnessSigningKey
      ( WitnessPaymentKey
      , WitnessPaymentExtendedKey
      , WitnessStakeKey
      , WitnessStakeExtendedKey
      , WitnessStakePoolKey
      , WitnessGenesisKey
      , WitnessGenesisExtendedKey
      , WitnessGenesisDelegateKey
      , WitnessGenesisDelegateExtendedKey
      ),
    ShelleySigningKey,
    getShelleyKeyWitnessVerificationKey,
    makeShelleySignature,
    toShelleySigningKey,

    -- *** Reading one of several key types
    FromSomeType(..),

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    TxMetadata
      ( TxMetadata
      , TxMetadataShelley
      ),
    toShelleyMetaData,
    fromShelleyMetaData,

    -- * Protocol parameter updates
    UpdateProposal(UpdateProposal),
    ProtocolParametersUpdate(ProtocolParametersUpdate),
    protocolUpdateProtocolVersion,
    protocolUpdateDecentralization,
    protocolUpdateExtraPraosEntropy,
    protocolUpdateMaxBlockHeaderSize,
    protocolUpdateMaxBlockBodySize,
    protocolUpdateMaxTxSize,
    protocolUpdateTxFeeFixed,
    protocolUpdateTxFeePerByte,
    protocolUpdateMinUTxOValue,
    protocolUpdateStakeAddressDeposit,
    protocolUpdateStakePoolDeposit,
    protocolUpdateMinPoolCost,
    protocolUpdatePoolRetireMaxEpoch,
    protocolUpdateStakePoolTargetNum,
    protocolUpdatePoolPledgeInfluence,
    protocolUpdateMonetaryExpansion,
    protocolUpdateTreasuryCut,
    EpochNo(..),
    NetworkMagic(..),

    -- * Scripts
    -- | Both 'PaymentCredential's and 'StakeCredential's can use scripts.
    -- Shelley supports multi-signatures via scripts.
    Script(ShelleyScript),
    SimpleScript(..),
    parseScript,
    parseScriptAny,
    parseScriptAll,
    parseScriptAtLeast,
    parseScriptSig,

    -- * Certificates
    Certificate (Certificate),

    -- ** Operational certificates
    OperationalCertificate(OperationalCertificate),
    OperationalCertificateIssueCounter(OperationalCertificateIssueCounter),
    OperationalCertIssueError(..),

    -- * Stake Pool
    StakePoolMetadata(StakePoolMetadata),
    stakePoolName,
    stakePoolDescription,
    stakePoolTicker,
    stakePoolHomepage,
    StakePoolMetadataReference(StakePoolMetadataReference),
    stakePoolMetadataURL,
    stakePoolMetadataHash,
    StakePoolParameters(StakePoolParameters),
    stakePoolId,
    stakePoolVRF,
    stakePoolCost,
    stakePoolMargin,
    stakePoolRewardAccount,
    stakePoolPledge,
    stakePoolOwners,
    stakePoolRelays,
    stakePoolMetadata,
    StakePoolRelay
      ( StakePoolRelayIp
      , StakePoolRelayDnsARecord
      , StakePoolRelayDnsSrvRecord
      ),
    toShelleyPoolParams,

    -- ** Stake pool operator's keys
    StakePoolKey,
    PoolId,

    -- ** KES keys
    KesKey,

    -- ** VRF keys
    VrfKey,

    -- ** Low level protocol interaction with a Cardano node
    LocalNodeConnectInfo(LocalNodeConnectInfo),
    ShelleyMode,
    CardanoMode,
    NodeConsensusMode
      ( ShelleyMode
      , CardanoMode
      ),
    LocalNodeClientProtocols(LocalNodeClientProtocols),
    withNodeProtocolClient,

  ) where

import           Cardano.API
import           Cardano.Api.Typed


-- For the deprecated functions below
import           Prelude
import           Data.Word
import           Data.Map (Map)
import qualified Shelley.Spec.Ledger.MetaData as Shelley

{-# DEPRECATED toShelleyMetaData
    "Use the 'TxMetadata' and 'TxMetadataShelley' constructors" #-}
toShelleyMetaData :: Map Word64 TxMetadataValue -> Shelley.MetaData
toShelleyMetaData = (\(TxMetadataShelley m) -> m) . TxMetadata

{-# DEPRECATED fromShelleyMetaData
    "Use the 'TxMetadata' and 'TxMetadataShelley' constructors" #-}
fromShelleyMetaData :: Shelley.MetaData -> Map Word64 TxMetadataValue
fromShelleyMetaData = (\(TxMetadata m) -> m) . TxMetadataShelley

