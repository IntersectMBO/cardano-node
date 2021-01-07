-- | This module provides a library interface that is intended to be
-- the complete API for Shelley covering everything, including
-- exposing constructors for the lower level types.
--

module Cardano.Api.Shelley
  ( module Cardano.Api,

    -- * Cryptographic key interface
    -- $keys
    VerificationKey(..),
    SigningKey(..),

    -- * Payment addresses
    -- | Constructing and inspecting Shelley payment addresses
    Address(ShelleyAddress),
    toShelleyAddr,
    fromShelleyAddr,
    toShelleyStakeCredential,
    fromShelleyStakeCredential,
    NetworkId(Mainnet, Testnet),

    -- * Stake addresses
    PaymentCredential(..),
    StakeAddress(..),
    StakeAddressReference(..),
    StakeCredential(..),
    toShelleyStakeAddr,
    fromShelleyStakeAddr,
    fromShelleyStakeReference,
    fromShelleyPaymentCredential,

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody(ShelleyTxBody),
    TxId(TxId),
    toShelleyTxId,
    fromShelleyTxId,
    TxIn(TxIn),
    toShelleyTxIn,
    fromShelleyTxIn,
    TxOut(TxOut),
    toShelleyTxOut,
    fromShelleyTxOut,
    TxIx(TxIx),
    Lovelace(Lovelace),
    toShelleyLovelace,
    fromShelleyLovelace,
    toMaryValue,
    fromMaryValue,
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
    toShelleyMetadata,
    fromShelleyMetadata,

    -- * Protocol parameter updates
    EpochNo(..),
    NetworkMagic(..),

    -- * Scripts
    toShelleyScript,
    toShelleyMultiSig,
    fromShelleyMultiSig,
    toAllegraTimelock,
    fromAllegraTimelock,
    toShelleyScriptHash,
    fromShelleyScriptHash,

    -- * Certificates
    Certificate (..),
    toShelleyCertificate,
    fromShelleyCertificate,

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

    -- ** Shelley based eras
    ShelleyLedgerEra,

    -- ** Conversions
    --TODO: arrange not to export these
    toShelleyNetwork,

  ) where

import           Cardano.Api
import           Cardano.Api.Address
import           Cardano.Api.TxBody
import           Cardano.Api.Typed
import           Cardano.Api.Value
