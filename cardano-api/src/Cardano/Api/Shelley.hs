-- | This module provides a library interface that is intended to be
-- the complete API for Shelley covering everything, including
-- exposing constructors for the lower level types.
--

module Cardano.Api.Shelley
  ( module Cardano.Api,

    -- * Genesis
    ShelleyGenesis(..),
    shelleyGenesisDefaults,

    -- * Cryptographic key interface
    -- $keys
    Key(..),
    VerificationKey(..),
    SigningKey(..),

    -- * Hashes
    Hash(..),

    -- * Payment addresses
    -- | Constructing and inspecting Shelley payment addresses
    Address(ShelleyAddress),
    toShelleyAddr,
    fromShelleyAddr,
    fromShelleyAddrIsSbe,
    fromShelleyAddrToAny,
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
    getTxIdShelley,
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
    calcMinimumDeposit,

    -- * Arbitrary signing
    signArbitraryBytesKes,

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(ShelleyTx),

    -- ** Incremental signing and separate witnesses
    KeyWitness
      ( ShelleyBootstrapWitness
      , ShelleyKeyWitness
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
    ShelleySigningKey(..),
    getShelleyKeyWitnessVerificationKey,
    getTxBodyAndWitnesses,
    makeShelleySignature,
    toShelleySigningKey,

    -- * Blocks
    fromConsensusBlock,
    toConsensusBlock,
    fromConsensusTip,
    fromConsensusPointInMode,
    toConsensusPointInMode,
    toConsensusPointHF,

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    toShelleyMetadata,
    fromShelleyMetadata,
    toShelleyMetadatum,
    fromShelleyMetadatum,

    -- * Protocol parameters
    ProtocolParameters(..),
    checkProtocolParameters,
    ProtocolParametersError(..),

    -- * Scripts
    fromShelleyBasedScript,
    toShelleyScript,
    toShelleyMultiSig,
    fromShelleyMultiSig,
    toAllegraTimelock,
    fromAllegraTimelock,
    toShelleyScriptHash,
    fromShelleyScriptHash,
    PlutusScript(..),
    PlutusScriptOrReferenceInput(..),
    SimpleScriptOrReferenceInput(..),
    toPlutusData,
    fromPlutusData,
    toAlonzoData,
    fromAlonzoData,
    toAlonzoPrices,
    fromAlonzoPrices,
    toAlonzoExUnits,
    fromAlonzoExUnits,
    toAlonzoRdmrPtr,
    fromAlonzoRdmrPtr,
    scriptDataFromJsonDetailedSchema,
    scriptDataToJsonDetailedSchema,
    calculateExecutionUnitsLovelace,

    -- * Reference Scripts
    ReferenceScript(..),
    ReferenceTxInsScriptsInlineDatumsSupportedInEra(..),
    refInsScriptsAndInlineDatsSupportedInEra,
    refScriptToShelleyScript,

    -- * Certificates
    Certificate (..),
    toShelleyCertificate,
    fromShelleyCertificate,

    -- ** Operational certificates
    OperationalCertificate(OperationalCertificate),
    OperationalCertificateIssueCounter(..),
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
    EpochNo(..),

    -- ** Stake pool operator's keys
    StakePoolKey,
    PoolId,

    -- ** KES keys
    KesKey,
    KESPeriod(..),

    -- ** VRF keys
    VrfKey,

    -- ** Low level protocol interaction with a Cardano node
    LocalNodeConnectInfo(LocalNodeConnectInfo),
    ShelleyMode,
    ConsensusMode
      ( ByronMode
      , ShelleyMode
      ),
    LocalNodeClientProtocols(LocalNodeClientProtocols),

    -- ** Shelley based eras
    ShelleyLedgerEra,


    -- ** Local State Query
    DebugLedgerState(..),
    decodeDebugLedgerState,
    ProtocolState(..),
    decodeProtocolState,
    SerialisedDebugLedgerState(..),
    CurrentEpochState(..),
    SerialisedCurrentEpochState(..),
    decodeCurrentEpochState,

    PoolState(..),
    SerialisedPoolState(..),
    decodePoolState,

    PoolDistribution(..),
    SerialisedPoolDistribution(..),
    decodePoolDistribution,

    StakeSnapshot(..),
    SerialisedStakeSnapshots(..),
    decodeStakeSnapshot,

    UTxO(..),
    AcquiringFailure(..),
    SystemStart(..),


    -- ** Various calculations
    LeadershipError(..),
    currentEpochEligibleLeadershipSlots,
    nextEpochEligibleLeadershipSlots,

    -- ** Conversions
    shelleyPayAddrToPlutusPubKHash,
    toConsensusGenTx,
    fromAlonzoCostModels,
    --TODO: arrange not to export these
    toShelleyNetwork,

  ) where

import           Cardano.Api
import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Genesis
import           Cardano.Api.InMode
import           Cardano.Api.IPC
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.LedgerState
import           Cardano.Api.NetworkId
import           Cardano.Api.OperationalCertificate
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Script
import           Cardano.Api.ScriptData
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Tx
import           Cardano.Api.TxBody
import           Cardano.Api.TxMetadata
import           Cardano.Api.Value
