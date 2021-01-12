{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | This module provides a library interface for interacting with Cardano as
-- a user of the system.
--
-- It is intended to be used to write tools and
--
-- In the interest of simplicity it glosses over some details of the system.
-- Most simple tools should be able to work just using this interface,
-- however you can go deeper and expose the types from the underlying libraries
-- using "Cardano.Api.Byron" or "Cardano.Api.Shelley".
--
module Cardano.Api.Typed (
    -- * Eras
    ByronEra,
    ShelleyEra,
    AllegraEra,
    MaryEra,
    CardanoEra(..),
    IsCardanoEra(..),
    AnyCardanoEra(..),
    anyCardanoEra,
    InAnyCardanoEra(..),

    -- ** Shelley-based eras
    ShelleyBasedEra(..),
    IsShelleyBasedEra(..),
    InAnyShelleyBasedEra(..),
    ShelleyLedgerEra,
    CardanoEraStyle(..),
    cardanoEraStyle,
    -- ** Deprecated
    Byron,
    Shelley,
    Allegra,
    Mary,
    -- * Type tags
    HasTypeProxy(..),
    AsType(..),
    -- * Cryptographic key interface
    -- $keys
    Key,
    VerificationKey(..),
    SigningKey(..),
    getVerificationKey,
    verificationKeyHash,
    castVerificationKey,
    castSigningKey,

    -- ** Generating keys
    generateSigningKey,
    deterministicSigningKey,
    deterministicSigningKeySeedSize,
    Crypto.Seed,
    Crypto.mkSeedFromBytes,
    Crypto.readSeedFromSystemEntropy,

    -- ** Hashes
    -- | In Cardano most keys are identified by their hash, and hashes are
    -- used in many other places.
    Hash(..),
    castHash,

    -- * Payment addresses
    -- | Constructing and inspecting normal payment addresses
    Address(..),
    ByronAddr,
    ShelleyAddr,
    NetworkId(..),
    -- ** Byron addresses
    makeByronAddress,
    ByronKey,
    ByronKeyLegacy,

    -- ** Shelley addresses
    makeShelleyAddress,
    PaymentCredential(..),
    StakeAddressReference(..),
    PaymentKey,
    PaymentExtendedKey,

    -- ** Addresses in any era
    AddressAny(..),

    -- ** Addresses in specific eras
    AddressInEra(..),
    AddressTypeInEra(..),
    byronAddressInEra,
    shelleyAddressInEra,
    anyAddressInShelleyBasedEra,
    anyAddressInEra,
    toAddressAny,
    makeByronAddressInEra,
    makeShelleyAddressInEra,

    -- * Stake addresses
    -- | Constructing and inspecting stake addresses
    StakeAddress(..),
    StakeCredential(..),
    makeStakeAddress,
    StakeKey,
    StakeExtendedKey,

    -- * Currency values
    -- ** Ada \/ Lovelace
    Lovelace(..),

    -- ** Multi-asset values
    Quantity(..),
    PolicyId(..),
    AssetName(..),
    AssetId(..),
    Value,
    selectAsset,
    valueFromList,
    valueToList,
    filterValue,
    negateValue,

    -- ** Ada \/ Lovelace within multi-asset values
    quantityToLovelace,
    lovelaceToQuantity,
    selectLovelace,
    lovelaceToValue,
    valueToLovelace,

    -- ** Alternative nested representation
    ValueNestedRep(..),
    ValueNestedBundle(..),
    valueToNestedRep,
    valueFromNestedRep,

    -- * Building transactions
    -- | Constructing and inspecting transactions

    -- ** Transaction bodies
    TxBody(..),
    makeTransactionBody,
    TxBodyContent(..),
    TxBodyError(..),

    -- ** Transitional utils
    makeByronTransaction,
    makeShelleyTransaction,

    -- ** Transaction Ids
    TxId(..),
    getTxId,

    -- ** Transaction inputs
    TxIn(..),
    TxIx(..),

    -- ** Transaction outputs
    TxOut(..),
    TxOutValue(..),

    -- ** Other transaction body types
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    SlotNo(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxWithdrawals(..),
    TxCertificates(..),
    Certificate(..),
    toShelleyCertificate,
    fromShelleyCertificate,
    TxUpdateProposal(..),
    TxMintValue(..),

    -- ** Era-dependent transaction body features
    MultiAssetSupportedInEra(..),
    OnlyAdaSupportedInEra(..),
    TxFeesExplicitInEra(..),
    TxFeesImplicitInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    ValidityLowerBoundSupportedInEra(..),
    TxMetadataSupportedInEra(..),
    AuxScriptsSupportedInEra(..),
    WithdrawalsSupportedInEra(..),
    CertificatesSupportedInEra(..),
    UpdateProposalSupportedInEra(..),

    -- ** Feature availability functions
    multiAssetSupportedInEra,
    txFeesExplicitInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    validityLowerBoundSupportedInEra,
    txMetadataSupportedInEra,
    auxScriptsSupportedInEra,
    withdrawalsSupportedInEra,
    certificatesSupportedInEra,
    updateProposalSupportedInEra,

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(..),
    getTxBody,
    getTxWitnesses,

    -- ** Signing in one go
    ShelleySigningKey,
    toShelleySigningKey,
    signByronTransaction,
    signShelleyTransaction,
    -- ** Incremental signing and separate witnesses
    makeSignedTransaction,
    Witness(..),
    makeByronKeyWitness,
    ShelleyWitnessSigningKey(..),
    makeShelleyKeyWitness,
    WitnessNetworkIdOrByronAddress (..),
    makeShelleyBootstrapWitness,
    makeScriptWitness,
    makeShelleySignature,
    getShelleyKeyWitnessVerificationKey,

    -- * Fee calculation
    transactionFee,
    estimateTransactionFee,

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    TxMetadata(..),
    toShelleyMetadata,
    fromShelleyMetadata,

    -- ** Constructing metadata
    TxMetadataValue(..),
    makeTransactionMetadata,

    -- ** Validating metadata
    validateTxMetadata,
    TxMetadataRangeError (..),

    -- ** Converstion to\/from JSON
    TxMetadataJsonSchema (..),
    metadataFromJson,
    metadataToJson,
    metadataValueToJsonNoSchema,
    TxMetadataJsonError (..),
    TxMetadataJsonSchemaError (..),

    -- * Registering stake address and delegating
    -- | Certificates that are embedded in transactions for registering and
    -- unregistering stake address, and for setting the stake pool delegation
    -- choice for a stake address.
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressDelegationCertificate,

    -- * Registering stake pools
    -- | Certificates that are embedded in transactions for registering and
    -- retiring stake pools. This includes updating the stake pool parameters.
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters(..),
    StakePoolRelay(..),
    StakePoolMetadataReference(..),

    -- ** Stake pool off-chain metadata
    StakePoolMetadata(..),
    validateAndHashStakePoolMetadata,
    StakePoolMetadataValidationError(..),

    -- * Scripts
    -- | Both 'PaymentCredential's and 'StakeCredential's can use scripts.

    -- ** Script languages
    SimpleScriptV1,
    SimpleScriptV2,
    ScriptLanguage(..),
    SimpleScriptVersion(..),
    PlutusScriptVersion,
    AnyScriptLanguage(..),
    IsScriptLanguage(..),
    IsSimpleScriptLanguage(..),

    -- ** Scripts in a specific language
    Script(..),

    -- ** Scripts in any language
    ScriptInAnyLang(..),
    toScriptInAnyLang,

    -- ** Scripts in a specific era
    ScriptInEra(..),
    toScriptInEra,
    eraOfScriptInEra,

    -- *** Languages supported in each era
    ScriptLanguageInEra(..),
    scriptLanguageSupportedInEra,
    languageOfScriptLanguageInEra,
    eraOfScriptLanguageInEra,

    -- ** Simple scripts
    -- | Making multi-signature and time-lock scripts.
    SimpleScript(..),
    TimeLocksSupported(..),
    timeLocksSupported,
    adjustSimpleScriptVersion,

    -- ** Script addresses
    -- | Making addresses from scripts.
    ScriptHash,
    hashScript,

    -- ** Internal conversion functions
    toShelleyScript,
    toShelleyMultiSig,
    fromShelleyMultiSig,
    toAllegraTimelock,
    fromAllegraTimelock,
    toShelleyScriptHash,
    fromShelleyScriptHash,

    -- * Serialisation
    -- | Support for serialising data in JSON, CBOR and text files.

    -- ** CBOR
    SerialiseAsCBOR,
    ToCBOR,
    FromCBOR,
    serialiseToCBOR,
    deserialiseFromCBOR,

    -- ** JSON
    ToJSON,
    FromJSON,
    serialiseToJSON,
    deserialiseFromJSON,
    JsonDecodeError(..),
    readFileJSON,
    writeFileJSON,

    -- ** Bech32
    SerialiseAsBech32,
    serialiseToBech32,
    deserialiseFromBech32,
    deserialiseAnyOfFromBech32,
    Bech32DecodeError(..),

    -- ** Addresses
    -- | Address serialisation is (sadly) special
    SerialiseAddress,
    serialiseAddress,
    deserialiseAddress,

    -- ** Raw binary
    -- | Some types have a natural raw binary format.
    SerialiseAsRawBytes,
    serialiseToRawBytes,
    deserialiseFromRawBytes,
    serialiseToRawBytesHex,
    deserialiseFromRawBytesHex,

    -- ** Text envelope
    -- | Support for a envelope file format with text headers and a hex-encoded
    -- binary payload.
    HasTextEnvelope(..),
    TextEnvelope(..),
    TextEnvelopeType,
    TextEnvelopeDescr,
    textEnvelopeRawCBOR,
    serialiseToTextEnvelope,
    deserialiseFromTextEnvelope,
    TextEnvelopeError(..),
    readFileTextEnvelope,
    writeFileTextEnvelope,
    writeFileTextEnvelopeWithOwnerPermissions,
    readTextEnvelopeFromFile,
    readTextEnvelopeOfTypeFromFile,

    -- *** Reading one of several key types
    FromSomeType(..),
    deserialiseFromTextEnvelopeAnyOf,
    readFileTextEnvelopeAnyOf,

    -- * Errors
    Error(..),
    throwErrorAsException,
    FileError(..),

    -- * Node interaction
    -- | Operations that involve talking to a local Cardano node.

    -- ** Queries
    -- ** Submitting transactions

    -- ** Low level protocol interaction with a Cardano node
    connectToLocalNode,
    LocalNodeConnectInfo(..),
    ByronMode,
    ShelleyMode,
    CardanoMode,
    NodeConsensusMode(..),
    LocalNodeClientProtocols(..),
    nullLocalNodeClientProtocols,
    withNodeProtocolClient,
--  connectToRemoteNode,

    -- *** Chain sync protocol
    ChainSyncClient(..),

    -- *** Local tx submission
    LocalTxSubmissionClient(..),
    submitTxToNodeLocal,

    -- *** Local state query
    LocalStateQueryClient(..),
    queryNodeLocalState,

    -- * Node operation
    -- | Support for the steps needed to operate a node, including the
    -- operator's offline keys, operational KES and VRF keys, and operational
    -- certificates.

    -- ** Stake pool operator's keys
    StakePoolKey,
    PoolId,

    -- ** KES keys
    KesKey,

    -- ** VRF keys
    VrfKey,

    -- ** Operational certificates
    OperationalCertificate(..),
    OperationalCertificateIssueCounter(..),
    Shelley.KESPeriod(..),
    OperationalCertIssueError(..),
    issueOperationalCertificate,

    -- * Genesis file
    -- | Types and functions needed to inspect or create a genesis file.
    GenesisKey,
    GenesisExtendedKey,
    GenesisDelegateKey,
    GenesisDelegateExtendedKey,
    GenesisUTxOKey,
    genesisUTxOPseudoTxIn,

    -- * Special transactions
    -- | There are various additional things that can be embedded in a
    -- transaction for special operations.
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,

    -- ** Protocol parameter updates
    UpdateProposal(..),
    ProtocolParametersUpdate(..),
    EpochNo(..),
    NetworkMagic(..),
    makeShelleyUpdateProposal,
    PraosNonce,
    makePraosNonce,

    -- ** Conversions
    --TODO: arrange not to export these
    fromByronTxIn,
    toByronLovelace,
    toByronNetworkMagic,
    toByronProtocolMagicId,
    toByronRequiresNetworkMagic,
    toShelleyNetwork,
    toShelleyPoolParams,
    toShelleyStakeAddr,
    toNetworkMagic,

    Shelley.Addr(..),
    Shelley.Coin(..),
    EpochSize(..),
    Shelley.GenDelegPair(..),
    Shelley.KeyRole (..),
    Shelley.KeyHash(..),
    Shelley.PParams'(..),
    Shelley.PParamsUpdate,
    Shelley.VerKeyVRF,
    StandardShelley,
    StandardAllegra,
    StandardMary,
    Shelley.emptyPParams,
    Shelley.truncateUnitInterval,
    emptyGenesisStaking,
    secondsToNominalDiffTime
  ) where

import           Prelude

import           Data.Void (Void)

import qualified Data.ByteString.Lazy as LBS

import qualified Data.Map.Strict as Map

import           Control.Concurrent.STM
import           Control.Tracer (nullTracer)

--
-- Common types, consensus, network
--
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

-- TODO: it'd be nice if the network imports needed were a bit more coherent
import           Ouroboros.Network.Block (Point, Tip)
import           Ouroboros.Network.Mux (MuxMode (InitiatorMode), MuxPeer (..),
                     RunMiniProtocol (InitiatorProtocolOnly))
import           Ouroboros.Network.NodeToClient (NetworkConnectTracers (..),
                     NodeToClientProtocols (..), NodeToClientVersionData (..), chainSyncPeerNull,
                     connectTo, foldMapVersions, localSnocket, localStateQueryPeerNull,
                     localTxSubmissionPeerNull, versionedNodeToClientProtocols, withIOManager)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

-- TODO: it'd be nice if the consensus imports needed were a bit more coherent
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Cardano (ProtocolClient, protocolClientInfo)
import           Ouroboros.Consensus.Ledger.Query (Query, ShowQuery)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import           Ouroboros.Consensus.Network.NodeToClient (Codecs' (..), clientCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion,
                     SupportedNetworkProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints)
import           Ouroboros.Consensus.Shelley.Node (emptyGenesisStaking)
import           Ouroboros.Consensus.Util.Time (secondsToNominalDiffTime)

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC)
import           Ouroboros.Consensus.Cardano.ShelleyHFC (ShelleyBlockHFC)

--
-- Crypto API used by consensus and Shelley (and should be used by Byron)
--
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Seed as Crypto

--
-- Byron imports
--
import qualified Cardano.Chain.Slotting as Byron

--
-- Shelley imports
--
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardMary, StandardShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.OCert as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley

--
-- Other config and common types
--
import           Cardano.Api.Protocol.Byron (mkNodeClientProtocolByron)
import           Cardano.Api.Protocol.Cardano (mkNodeClientProtocolCardano)
import           Cardano.Api.Protocol.Shelley (mkNodeClientProtocolShelley)

import           Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import           Ouroboros.Network.Protocol.LocalStateQuery.Client as StateQuery
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client as TxSubmission

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Fees
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Key
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysPraos
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.OperationalCertificate
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
import           Cardano.Api.SerialiseBech32
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Tx
import           Cardano.Api.TxBody
import           Cardano.Api.TxMetadata
import           Cardano.Api.Value


{- HLINT ignore "Redundant flip" -}

-- ----------------------------------------------------------------------------
-- Keys key keys!
--



-- $keys
-- Cardano has lots of cryptographic keys used for lots of different purposes.
-- Some keys have different representations, but most are just using keys in
-- different roles.
--
-- To allow for the different representations and to avoid mistakes we
-- distinguish the key /role/. These are type level distinctions, so each of
-- these roles is a type level tag.
--


-- ----------------------------------------------------------------------------
-- Node IPC protocols
--

data ByronMode
data ShelleyMode
data CardanoMode

data LocalNodeConnectInfo mode block =
     LocalNodeConnectInfo {
       localNodeSocketPath    :: FilePath,
       localNodeNetworkId     :: NetworkId,
       localNodeConsensusMode :: NodeConsensusMode mode block
     }

data NodeConsensusMode mode block where

     ByronMode
       :: Byron.EpochSlots
       -> NodeConsensusMode ByronMode ByronBlockHFC

     ShelleyMode
       :: NodeConsensusMode ShelleyMode (ShelleyBlockHFC StandardShelley)

     CardanoMode
       :: Byron.EpochSlots
       -> NodeConsensusMode CardanoMode (CardanoBlock StandardCrypto)


withNodeProtocolClient
  :: NodeConsensusMode mode block
  -> (   (SerialiseNodeToClientConstraints block,
          SupportedNetworkProtocolVersion block)
      => ProtocolClient block (BlockProtocol block) -> a)
  -> a
withNodeProtocolClient (ByronMode epochSlots) f =
    f (mkNodeClientProtocolByron epochSlots)

withNodeProtocolClient ShelleyMode f = f mkNodeClientProtocolShelley

withNodeProtocolClient (CardanoMode epochSlots) f =
    f (mkNodeClientProtocolCardano epochSlots)

data LocalNodeClientProtocols block =
     LocalNodeClientProtocols {
       localChainSyncClient
         :: Maybe (ChainSyncClient
                    block
                    (Point block)
                    (Tip block)
                    IO ())

     , localTxSubmissionClient
         :: Maybe (LocalTxSubmissionClient
                    (GenTx block)
                    (ApplyTxErr block)
                    IO ())

     , localStateQueryClient
         :: Maybe (LocalStateQueryClient
                    block
                    (Point block)
                    (Query block)
                    IO ())
     }

nullLocalNodeClientProtocols :: LocalNodeClientProtocols block
nullLocalNodeClientProtocols =
    LocalNodeClientProtocols {
      localChainSyncClient    = Nothing,
      localTxSubmissionClient = Nothing,
      localStateQueryClient   = Nothing
    }


-- | Establish a connection to a node and execute the given set of protocol
-- handlers.
--
connectToLocalNode :: forall mode block.
                      (ShowProxy block, ShowProxy (ApplyTxErr block),
                       ShowProxy (Query block), ShowProxy (GenTx block),
                       ShowQuery (Query block))
                       --TODO: too many constraints! we should pass
                       -- a single protocol to run, not all of them, until we
                       -- have the more flexible interface to run any combo
                   => LocalNodeConnectInfo mode block
                   -> LocalNodeClientProtocols block
                   -> IO ()
connectToLocalNode LocalNodeConnectInfo {
                     localNodeSocketPath    = path,
                     localNodeNetworkId     = network,
                     localNodeConsensusMode = mode
                   } clientptcls =
    withIOManager $ \iomgr ->
      withNodeProtocolClient mode $ \ptcl ->
      connectTo
        (localSnocket iomgr path)
        NetworkConnectTracers {
          nctMuxTracer       = nullTracer,
          nctHandshakeTracer = nullTracer
        }
        (foldMapVersions
            (\(version, blockVersion) ->
                versionedNodeToClientProtocols
                     version
                     NodeToClientVersionData {
                       networkMagic = toNetworkMagic network
                     }
                     (\_conn _runOrStop -> protocols ptcl blockVersion))
            (Map.toList (supportedNodeToClientVersions proxy)))
        path
  where
    proxy :: Proxy block
    proxy = Proxy

    protocols :: SerialiseNodeToClientConstraints block
              => ProtocolClient block (BlockProtocol block)
              -> BlockNodeToClientVersion block
              -> NodeToClientProtocols InitiatorMode LBS.ByteString IO () Void
    protocols ptcl clientVersion =
      let Codecs {
              cChainSyncCodec
            , cTxSubmissionCodec
            , cStateQueryCodec
            } = clientCodecs (pClientInfoCodecConfig (protocolClientInfo ptcl))
                             clientVersion

          LocalNodeClientProtocols {
            localChainSyncClient,
            localTxSubmissionClient,
            localStateQueryClient
          } = clientptcls

       in NodeToClientProtocols {
            localChainSyncProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cChainSyncCodec
                  (maybe chainSyncPeerNull
                         chainSyncClientPeer
                         localChainSyncClient)

          , localTxSubmissionProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cTxSubmissionCodec
                  (maybe localTxSubmissionPeerNull
                         localTxSubmissionClientPeer
                         localTxSubmissionClient)

          , localStateQueryProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cStateQueryCodec
                  (maybe localStateQueryPeerNull
                         localStateQueryClientPeer
                         localStateQueryClient)
          }


--TODO: change this query to be just a protocol client handler to be used with
-- connectToLocalNode. This would involve changing connectToLocalNode to be
-- able to return protocol handler results properly.

-- | Establish a connection to a node and execute a single query using the
-- local state query protocol.
--
queryNodeLocalState :: forall mode block result.
                       (ShowProxy block, ShowProxy (ApplyTxErr block),
                        ShowProxy (Query block), ShowProxy (GenTx block),
                        ShowQuery (Query block))
                    => LocalNodeConnectInfo mode block
                    -> (Point block, Query block result)
                    -> IO (Either AcquireFailure result)
queryNodeLocalState connctInfo pointAndQuery = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      nullLocalNodeClientProtocols {
        localStateQueryClient =
          Just (localStateQuerySingle resultVar pointAndQuery)
      }
    atomically (takeTMVar resultVar)
  where
    localStateQuerySingle
      :: TMVar (Either AcquireFailure result)
      -> (Point block, Query block result)
      -> LocalStateQueryClient block (Point block) (Query block) IO ()
    localStateQuerySingle resultVar (point, query) =
      LocalStateQueryClient $ pure $
        SendMsgAcquire point $
        ClientStAcquiring {
          recvMsgAcquired =
            SendMsgQuery query $
            ClientStQuerying {
              recvMsgResult = \result -> do
                --TODO: return the result via the SendMsgDone rather than
                -- writing into an mvar
                atomically $ putTMVar resultVar (Right result)
                pure $ SendMsgRelease $
                  pure $ StateQuery.SendMsgDone ()
            }
        , recvMsgFailure = \failure -> do
            --TODO: return the result via the SendMsgDone rather than
            -- writing into an mvar
            atomically $ putTMVar resultVar (Left failure)
            pure $ StateQuery.SendMsgDone ()
        }

submitTxToNodeLocal :: forall mode block.
                       (ShowProxy block, ShowProxy (ApplyTxErr block),
                        ShowProxy (Query block), ShowProxy (GenTx block),
                        ShowQuery (Query block))
                    => LocalNodeConnectInfo mode block
                    -> GenTx block
                    -> IO (SubmitResult (ApplyTxErr block))
submitTxToNodeLocal connctInfo tx = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      nullLocalNodeClientProtocols {
        localTxSubmissionClient =
          Just (localTxSubmissionClientSingle resultVar)
      }
    atomically (takeTMVar resultVar)
  where
    localTxSubmissionClientSingle
      :: TMVar (SubmitResult (ApplyTxErr block))
      -> LocalTxSubmissionClient (GenTx block) (ApplyTxErr block) IO ()
    localTxSubmissionClientSingle resultVar =
        LocalTxSubmissionClient $
        pure $ SendMsgSubmitTx tx $ \result -> do
        atomically $ putTMVar resultVar result
        pure (TxSubmission.SendMsgDone ())
