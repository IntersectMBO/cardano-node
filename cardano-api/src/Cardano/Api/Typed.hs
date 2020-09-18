{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

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
    Byron,
    Shelley,
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
    NetworkId(..),
    -- * Byron addresses
    makeByronAddress,
    ByronKey,
    -- * Shelley addresses
    makeShelleyAddress,
    PaymentCredential(..),
    StakeAddressReference(..),
    PaymentKey,
    PaymentExtendedKey,

    -- * Stake addresses
    -- | Constructing and inspecting stake addresses
    StakeAddress(..),
    StakeCredential(..),
    makeStakeAddress,
    StakeKey,
    StakeExtendedKey,

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody(..),
    TxId(..),
    getTxId,
    TxIn(..),
    TxOut(..),
    TxIx(..),
    TTL,
    TxFee,
    Lovelace(..),
    makeByronTransaction,
    makeShelleyTransaction,
    SlotNo(..),
    TxExtraContent(..),
    txExtraContentEmpty,
    Certificate(..),

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(..),
    getTxBody,
    getTxWitnesses,

    -- ** Signing in one go
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
    makeShelleyScriptWitness,

    -- * Fee calculation
    transactionFee,
    estimateTransactionFee,

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    TxMetadata (TxMetadata, TxMetadataShelley),
    TxMetadataValue(..),
    makeTransactionMetadata,

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
    -- Shelley supports multi-signatures via scripts.
    Script(..),

    -- ** Script addresses
    -- | Making addresses from scripts.
    scriptHash,

    -- ** Multi-signature scripts
    -- | Making multi-signature scripts.
    MultiSigScript(..),
    makeMultiSigScript,

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
    TextEnvelope,
    TextEnvelopeType,
    TextEnvelopeDescr,
    TextEnvelopeError,
    serialiseToTextEnvelope,
    deserialiseFromTextEnvelope,
    readFileTextEnvelope,
    writeFileTextEnvelope,
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

    -- ** Conversions
    --TODO: arrange not to export these
    toByronNetworkMagic,
    toByronProtocolMagicId,
    toByronRequiresNetworkMagic,
    toShelleyNetwork,
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
    Shelley.emptyPParams,
    Shelley.truncateUnitInterval,
    emptyGenesisStaking,
    secondsToNominalDiffTime
  ) where

import           Prelude

import           Data.Aeson.Encode.Pretty (encodePretty')
import           Data.Bifunctor (first)
import           Data.Kind (Constraint)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Proxy (Proxy (..))
import           Data.Scientific (toBoundedInteger)
import           Data.String (IsString (fromString))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Void (Void)
import           Data.Word
import           Numeric.Natural

import           Data.IP (IPv4, IPv6)
import           Network.Socket (PortNumber)
import qualified Network.URI as URI


import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Data.Map.Lazy as Map.Lazy
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Codec.Binary.Bech32 as Bech32

import           Control.Applicative
import           Control.Monad
--import Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Control.Exception (Exception (..), IOException, throwIO)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.Trans.Except.Extra
import           Control.Tracer (nullTracer)

import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import           Data.Kind (Type)

--
-- Common types, consensus, network
--
import           Cardano.Binary (Annotated (..), FromCBOR (fromCBOR), ToCBOR (toCBOR), reAnnotate,
                     recoverBytes)
import qualified Cardano.Binary as CBOR
import qualified Cardano.Prelude as CBOR (cborError)
import qualified Shelley.Spec.Ledger.Serialization as CBOR (CBORGroup (..), decodeNullMaybe,
                     encodeNullMaybe)

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

-- TODO: it'd be nice if the network imports needed were a bit more coherent
import           Ouroboros.Network.Block (Point, Tip)
import           Ouroboros.Network.Magic (NetworkMagic (..))
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
import           Ouroboros.Consensus.Ledger.Abstract (Query, ShowQuery)
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
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.KES.Class as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Crypto.Util as Crypto
import qualified Cardano.Crypto.VRF.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD

--
-- Byron imports
--
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Crypto.ProtocolMagic as Byron
import qualified Cardano.Crypto.Signing as Byron

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Slotting as Byron
import qualified Cardano.Chain.UTxO as Byron


--
-- Shelley imports
--
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto, StandardShelley)

import qualified Cardano.Ledger.Crypto as Shelley (DSIGN, KES, VRF)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Address.Bootstrap as Shelley
import           Shelley.Spec.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.MetaData as Shelley
import qualified Shelley.Spec.Ledger.OCert as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

-- Types we will re-export as-is
import           Shelley.Spec.Ledger.TxBody (MIRPot (..))

-- TODO: replace the above with
--import qualified Cardano.Api.Byron   as Byron
--import qualified Cardano.Api.Shelley as Shelley

--
-- Other config and common types
--
import           Cardano.Api.Protocol.Byron (mkNodeClientProtocolByron)
import           Cardano.Api.Protocol.Cardano (mkNodeClientProtocolCardano)
import           Cardano.Api.Protocol.Shelley (mkNodeClientProtocolShelley)
import qualified Cardano.Api.TextView as TextView

import           Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import           Ouroboros.Network.Protocol.LocalStateQuery.Client as StateQuery
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client as TxSubmission

{- HLINT ignore "Redundant flip" -}

-- ----------------------------------------------------------------------------
-- Cardano eras, sometimes we have to distinguish them
--

-- | A type used as a tag to distinguish the Byron era.
data Byron

-- | A type used as a tag to distinguish the Shelley era.
data Shelley


class HasTypeProxy t where
  -- | A family of singleton types used in this API to indicate which type to
  -- use where it would otherwise be ambiguous or merely unclear.
  --
  -- Values of this type are passed to
  --
  data AsType t

  proxyToAsType :: Proxy t -> AsType t


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

-- | An interface for cryptographic keys used for signatures with a 'SigningKey'
-- and a 'VerificationKey' key.
--
-- This interface does not provide actual signing or verifying functions since
-- this API is concerned with the management of keys: generating and
-- serialising.
--
class (Eq (VerificationKey keyrole),
       Show (VerificationKey keyrole),
       SerialiseAsRawBytes (Hash keyrole),
       HasTextEnvelope (VerificationKey keyrole),
       HasTextEnvelope (SigningKey keyrole))
    => Key keyrole where

    -- | The type of cryptographic verification key, for each key role.
    data VerificationKey keyrole :: Type

    -- | The type of cryptographic signing key, for each key role.
    data SigningKey keyrole :: Type

    -- | Get the corresponding verification key from a signing key.
    getVerificationKey :: SigningKey keyrole -> VerificationKey keyrole

    -- | Generate a 'SigningKey' deterministically, given a 'Crypto.Seed'. The
    -- required size of the seed is given by 'deterministicSigningKeySeedSize'.
    --
    deterministicSigningKey :: AsType keyrole -> Crypto.Seed -> SigningKey keyrole
    deterministicSigningKeySeedSize :: AsType keyrole -> Word

    verificationKeyHash :: VerificationKey keyrole -> Hash keyrole


-- | Generate a 'SigningKey' using a seed from operating system entropy.
--
generateSigningKey :: Key keyrole => AsType keyrole -> IO (SigningKey keyrole)
generateSigningKey keytype = do
    seed <- Crypto.readSeedFromSystemEntropy seedSize
    return $! deterministicSigningKey keytype seed
  where
    seedSize = deterministicSigningKeySeedSize keytype


-- | Some key roles share the same representation and it is sometimes
-- legitimate to change the role of a key.
--
class CastVerificationKeyRole keyroleA keyroleB where

    -- | Change the role of a 'VerificationKey', if the representation permits.
    castVerificationKey :: VerificationKey keyroleA -> VerificationKey keyroleB

class CastSigningKeyRole keyroleA keyroleB where

    -- | Change the role of a 'SigningKey', if the representation permits.
    castSigningKey :: SigningKey keyroleA -> SigningKey keyroleB


data family Hash keyrole :: Type

class CastHash keyroleA keyroleB where

    castHash :: Hash keyroleA -> Hash keyroleB


-- ----------------------------------------------------------------------------
-- Addresses
--

data Address era where

     -- | Byron addresses are valid in both the Byron and Shelley era.
     --
     ByronAddress
       :: Byron.Address
       -> Address era

     -- | Shelley addresses are only valid in the Shelley era.
     --
     ShelleyAddress
       :: Shelley.Network
       -> Shelley.PaymentCredential StandardShelley
       -> Shelley.StakeReference    StandardShelley
       -> Address Shelley

deriving instance Eq (Address Byron)
deriving instance Ord (Address Byron)
deriving instance Show (Address Byron)

deriving instance Eq (Address Shelley)
deriving instance Ord (Address Shelley)
deriving instance Show (Address Shelley)

data StakeAddress where

     StakeAddress
       :: Shelley.Network
       -> Shelley.StakeCredential StandardShelley
       -> StakeAddress
  deriving (Eq, Ord, Show)

data NetworkId
       = Mainnet
       | Testnet !NetworkMagic
  deriving (Eq, Show)

data PaymentCredential
       = PaymentCredentialByKey    (Hash PaymentKey)
       | PaymentCredentialByScript (Hash Script)
  deriving (Eq, Show)

data StakeCredential
       = StakeCredentialByKey    (Hash StakeKey)
       | StakeCredentialByScript (Hash Script)
  deriving (Eq, Show)

data StakeAddressReference
       = StakeAddressByValue   StakeCredential
       | StakeAddressByPointer StakeAddressPointer
       | NoStakeAddress
  deriving (Eq, Show)

type StakeAddressPointer = Shelley.Ptr


instance HasTypeProxy (Address Byron) where
    data AsType (Address Byron) = AsByronAddress
    proxyToAsType _ = AsByronAddress


instance HasTypeProxy (Address Shelley) where
    data AsType (Address Shelley) = AsShelleyAddress
    proxyToAsType _ = AsShelleyAddress


instance HasTypeProxy StakeAddress where
    data AsType StakeAddress = AsStakeAddress
    proxyToAsType _ = AsStakeAddress


instance SerialiseAsRawBytes (Address Byron) where
    serialiseToRawBytes (ByronAddress addr) = CBOR.serialize' addr

    deserialiseFromRawBytes AsByronAddress bs =
      case CBOR.decodeFull' bs of
        Left  _    -> Nothing
        Right addr -> Just (ByronAddress addr)


instance SerialiseAsRawBytes (Address Shelley) where
    serialiseToRawBytes (ByronAddress addr) =
        Shelley.serialiseAddr
      . Shelley.AddrBootstrap
      . Shelley.BootstrapAddress
      $ addr

    serialiseToRawBytes (ShelleyAddress nw pc scr) =
        Shelley.serialiseAddr (Shelley.Addr nw pc scr)

    deserialiseFromRawBytes AsShelleyAddress bs =
        case Shelley.deserialiseAddr bs of
          Nothing -> Nothing
          Just (Shelley.Addr nw pc scr) ->
            Just (ShelleyAddress nw pc scr)

          Just (Shelley.AddrBootstrap (Shelley.BootstrapAddress addr)) ->
            Just (ByronAddress addr)


instance SerialiseAsRawBytes StakeAddress where
    serialiseToRawBytes (StakeAddress nw sc) =
        Shelley.serialiseRewardAcnt (Shelley.RewardAcnt nw sc)

    deserialiseFromRawBytes AsStakeAddress bs =
        case Shelley.deserialiseRewardAcnt bs of
          Nothing -> Nothing
          Just (Shelley.RewardAcnt nw sc) -> Just (StakeAddress nw sc)


instance SerialiseAsBech32 (Address Shelley) where
    bech32PrefixFor (ShelleyAddress Shelley.Mainnet _ _) = "addr"
    bech32PrefixFor (ShelleyAddress Shelley.Testnet _ _) = "addr_test"
    bech32PrefixFor (ByronAddress _)                     = "addr"

    bech32PrefixesPermitted AsShelleyAddress = ["addr", "addr_test"]


instance SerialiseAsBech32 StakeAddress where
    bech32PrefixFor (StakeAddress Shelley.Mainnet _) = "stake"
    bech32PrefixFor (StakeAddress Shelley.Testnet _) = "stake_test"

    bech32PrefixesPermitted AsStakeAddress = ["stake", "stake_test"]


instance SerialiseAddress (Address Byron) where
    serialiseAddress addr@ByronAddress{} =
         Text.decodeLatin1
       . Base58.encodeBase58 Base58.bitcoinAlphabet
       . serialiseToRawBytes
       $ addr

    deserialiseAddress AsByronAddress txt = do
      bs <- Base58.decodeBase58 Base58.bitcoinAlphabet (Text.encodeUtf8 txt)
      deserialiseFromRawBytes AsByronAddress bs

instance SerialiseAddress (Address Shelley) where
    serialiseAddress (ByronAddress addr) =
      serialiseAddress (ByronAddress addr :: Address Byron)

    serialiseAddress addr@ShelleyAddress{} =
      serialiseToBech32 addr

    deserialiseAddress AsShelleyAddress t =
          deserialiseAsShelleyAddress
      <|> deserialiseAsByronAddress
      where
        deserialiseAsShelleyAddress =
          either (const Nothing) Just $
          deserialiseFromBech32 AsShelleyAddress t

        deserialiseAsByronAddress =
          castByronToShelleyAddress <$>
          deserialiseAddress AsByronAddress t

        castByronToShelleyAddress :: Address Byron -> Address Shelley
        castByronToShelleyAddress (ByronAddress addr) = ByronAddress addr


instance SerialiseAddress StakeAddress where
    serialiseAddress addr@StakeAddress{} =
      serialiseToBech32 addr

    deserialiseAddress AsStakeAddress t =
      either (const Nothing) Just $
      deserialiseFromBech32 AsStakeAddress t


makeByronAddress :: NetworkId
                 -> VerificationKey ByronKey
                 -> Address era
makeByronAddress nw (ByronVerificationKey vk) =
    ByronAddress $
      Byron.makeVerKeyAddress
        (toByronNetworkMagic nw)
        vk


makeShelleyAddress :: NetworkId
                   -> PaymentCredential
                   -> StakeAddressReference
                   -> Address Shelley
makeShelleyAddress nw pc scr =
    ShelleyAddress
      (toShelleyNetwork nw)
      (toShelleyPaymentCredential pc)
      (toShelleyStakeReference scr)


makeStakeAddress :: NetworkId
                 -> StakeCredential
                 -> StakeAddress
makeStakeAddress nw sc =
    StakeAddress
      (toShelleyNetwork nw)
      (toShelleyStakeCredential sc)


toByronProtocolMagicId :: NetworkId -> Byron.ProtocolMagicId
toByronProtocolMagicId Mainnet = Byron.mainnetProtocolMagicId
toByronProtocolMagicId (Testnet (NetworkMagic pm)) = Byron.ProtocolMagicId pm

toByronNetworkMagic :: NetworkId -> Byron.NetworkMagic
toByronNetworkMagic Mainnet                     = Byron.NetworkMainOrStage
toByronNetworkMagic (Testnet (NetworkMagic nm)) = Byron.NetworkTestnet nm

toByronRequiresNetworkMagic :: NetworkId -> Byron.RequiresNetworkMagic
toByronRequiresNetworkMagic Mainnet   = Byron.RequiresNoMagic
toByronRequiresNetworkMagic Testnet{} = Byron.RequiresMagic

toShelleyNetwork :: NetworkId -> Shelley.Network
toShelleyNetwork  Mainnet    = Shelley.Mainnet
toShelleyNetwork (Testnet _) = Shelley.Testnet

toNetworkMagic :: NetworkId -> NetworkMagic
toNetworkMagic Mainnet      = NetworkMagic (Byron.unProtocolMagicId Byron.mainnetProtocolMagicId)
toNetworkMagic (Testnet nm) = nm

toShelleyAddr :: Address era -> Shelley.Addr StandardShelley
toShelleyAddr (ByronAddress addr)        = Shelley.AddrBootstrap
                                             (Shelley.BootstrapAddress addr)
toShelleyAddr (ShelleyAddress nw pc scr) = Shelley.Addr nw pc scr

toShelleyStakeAddr :: StakeAddress -> Shelley.RewardAcnt StandardShelley
toShelleyStakeAddr (StakeAddress nw sc) =
    Shelley.RewardAcnt {
      Shelley.getRwdNetwork = nw,
      Shelley.getRwdCred    = sc
    }

toShelleyPaymentCredential :: PaymentCredential
                           -> Shelley.PaymentCredential StandardShelley
toShelleyPaymentCredential (PaymentCredentialByKey (PaymentKeyHash kh)) =
    Shelley.KeyHashObj kh
toShelleyPaymentCredential (PaymentCredentialByScript (ScriptHash sh)) =
    Shelley.ScriptHashObj sh

toShelleyStakeCredential :: StakeCredential
                         -> Shelley.StakeCredential StandardShelley
toShelleyStakeCredential (StakeCredentialByKey (StakeKeyHash kh)) =
    Shelley.KeyHashObj kh
toShelleyStakeCredential (StakeCredentialByScript (ScriptHash kh)) =
    Shelley.ScriptHashObj kh

toShelleyStakeReference :: StakeAddressReference
                        -> Shelley.StakeReference StandardShelley
toShelleyStakeReference (StakeAddressByValue stakecred) =
    Shelley.StakeRefBase (toShelleyStakeCredential stakecred)
toShelleyStakeReference (StakeAddressByPointer ptr) =
    Shelley.StakeRefPtr ptr
toShelleyStakeReference  NoStakeAddress =
    Shelley.StakeRefNull


-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Shelley.Hash StandardShelley ())
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)
               -- We use the Shelley representation and convert the Byron one

instance HasTypeProxy TxId where
    data AsType TxId = AsTxId
    proxyToAsType _ = AsTxId

instance SerialiseAsRawBytes TxId where
    serialiseToRawBytes (TxId h) = Crypto.hashToBytes h
    deserialiseFromRawBytes AsTxId bs = TxId <$> Crypto.hashFromBytes bs

toByronTxId :: TxId -> Byron.TxId
toByronTxId (TxId h) =
    Byron.unsafeHashFromBytes (Crypto.hashToBytes h)

toShelleyTxId :: TxId -> Shelley.TxId StandardShelley
toShelleyTxId (TxId h) =
    Shelley.TxId (Crypto.castHash h)

-- | Calculate the transaction identifier for a 'TxBody'.
--
getTxId :: TxBody era -> TxId
getTxId (ByronTxBody tx) =
    TxId
  . Crypto.UnsafeHash
  . SBS.toShort
  . recoverBytes
  $ tx

getTxId (ShelleyTxBody tx _) =
    TxId
  . Crypto.castHash
  . (\(Shelley.TxId txhash) -> txhash)
  . Shelley.txid
  $ tx


-- ----------------------------------------------------------------------------
-- Transaction constituent types
--

data TxIn = TxIn TxId TxIx

deriving instance Eq TxIn
deriving instance Show TxIn

newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

data TxOut era = TxOut (Address era) Lovelace

deriving instance Eq (TxOut Byron)
deriving instance Eq (TxOut Shelley)
deriving instance Show (TxOut Byron)
deriving instance Show (TxOut Shelley)

newtype Lovelace = Lovelace Integer
  deriving (Eq, Ord, Enum, Show)


toByronTxIn  :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
    Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toByronTxOut :: TxOut Byron -> Maybe Byron.TxOut
toByronTxOut (TxOut (ByronAddress addr) value) =
    Byron.TxOut addr <$> toByronLovelace value

toByronLovelace :: Lovelace -> Maybe Byron.Lovelace
toByronLovelace (Lovelace x) =
    case Byron.integerToLovelace x of
      Left  _  -> Nothing
      Right x' -> Just x'

toShelleyTxIn  :: TxIn -> Shelley.TxIn StandardShelley
toShelleyTxIn (TxIn txid (TxIx txix)) =
    Shelley.TxIn (toShelleyTxId txid) (fromIntegral txix)

toShelleyTxOut :: TxOut era -> Shelley.TxOut StandardShelley
toShelleyTxOut (TxOut addr value) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyLovelace :: Lovelace -> Shelley.Coin
toShelleyLovelace (Lovelace l) = Shelley.Coin l
--TODO: validate bounds


-- ----------------------------------------------------------------------------
-- Unsigned transactions
--

data TxBody era where

     ByronTxBody
       :: Annotated Byron.Tx ByteString
       -> TxBody Byron

     ShelleyTxBody
       :: Shelley.TxBody StandardShelley
       -> Maybe Shelley.MetaData
       -> TxBody Shelley

deriving instance Eq (TxBody Byron)
deriving instance Show (TxBody Byron)

deriving instance Eq (TxBody Shelley)
deriving instance Show (TxBody Shelley)

instance HasTypeProxy (TxBody Byron) where
    data AsType (TxBody Byron) = AsByronTxBody
    proxyToAsType _ = AsByronTxBody

instance HasTypeProxy (TxBody Shelley) where
    data AsType (TxBody Shelley) = AsShelleyTxBody
    proxyToAsType _ = AsShelleyTxBody


instance SerialiseAsCBOR (TxBody Byron) where
    serialiseToCBOR (ByronTxBody txbody) =
      recoverBytes txbody

    deserialiseFromCBOR AsByronTxBody bs = do
      ByronTxBody <$>
        CBOR.decodeFullAnnotatedBytes
          "Byron TxBody"
          CBOR.fromCBORAnnotated
          (LBS.fromStrict bs)

instance SerialiseAsCBOR (TxBody Shelley) where
    serialiseToCBOR (ShelleyTxBody txbody txmetadata) =
      CBOR.serializeEncoding' $
          CBOR.encodeListLen 2
       <> CBOR.toCBOR txbody
       <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata

    deserialiseFromCBOR AsShelleyTxBody bs =
      CBOR.decodeAnnotator
        "Shelley TxBody"
        decodeAnnotatedPair
        (LBS.fromStrict bs)
      where
        decodeAnnotatedPair :: CBOR.Decoder s (CBOR.Annotator (TxBody Shelley))
        decodeAnnotatedPair =  do
          CBOR.decodeListLenOf 2
          txbody     <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody
              (flip CBOR.runAnnotator fbs txbody)
              (flip CBOR.runAnnotator fbs <$> txmetadata)


instance HasTextEnvelope (TxBody Byron) where
    textEnvelopeType _ = "TxUnsignedByron"

instance HasTextEnvelope (TxBody Shelley) where
    textEnvelopeType _ = "TxUnsignedShelley"


data ByronTxBodyConversionError =
       ByronTxBodyEmptyTxIns
     | ByronTxBodyEmptyTxOuts
     | ByronTxBodyLovelaceOverflow (TxOut Byron)
     deriving Show

makeByronTransaction :: [TxIn]
                     -> [TxOut Byron]
                     -> Either ByronTxBodyConversionError
                               (TxBody Byron)
makeByronTransaction ins outs = do
    ins'  <- NonEmpty.nonEmpty ins        ?! ByronTxBodyEmptyTxIns
    let ins'' = NonEmpty.map toByronTxIn ins'

    outs'  <- NonEmpty.nonEmpty outs      ?! ByronTxBodyEmptyTxOuts
    outs'' <- traverse
                (\out -> toByronTxOut out ?! ByronTxBodyLovelaceOverflow out)
                outs'
    return $
      ByronTxBody $
        reAnnotate $
          Annotated
            (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
            ()


data TxExtraContent =
     TxExtraContent {
       txMetadata        :: Maybe TxMetadata,
       txWithdrawals     :: [(StakeAddress, Lovelace)],
       txCertificates    :: [Certificate],
       txUpdateProposal  :: Maybe UpdateProposal
     }

txExtraContentEmpty :: TxExtraContent
txExtraContentEmpty =
    TxExtraContent {
      txMetadata        = Nothing,
      txWithdrawals     = [],
      txCertificates    = [],
      txUpdateProposal  = Nothing
    }

type TxFee = Lovelace
type TTL   = SlotNo

makeShelleyTransaction :: TxExtraContent
                       -> TTL
                       -> TxFee
                       -> [TxIn]
                       -> [TxOut anyera]
                       -> TxBody Shelley
makeShelleyTransaction TxExtraContent {
                         txMetadata,
                         txWithdrawals,
                         txCertificates,
                         txUpdateProposal
                       } ttl fee ins outs =
    --TODO: validate the txins are not empty, and tx out coin values are in range
    ShelleyTxBody
      (Shelley.TxBody
        (Set.fromList (map toShelleyTxIn ins))
        (Seq.fromList (map toShelleyTxOut outs))
        (Seq.fromList [ cert | Certificate cert <- txCertificates ])
        (toShelleyWdrl txWithdrawals)
        (toShelleyLovelace fee)
        ttl
        (toShelleyUpdate <$> maybeToStrictMaybe txUpdateProposal)
        (toShelleyMetadataHash <$> maybeToStrictMaybe txMetadata))
      (toShelleyMetadata <$> txMetadata)
  where
    toShelleyUpdate (UpdateProposal p) = p

    toShelleyMetadata     (TxMetadataShelley m) = m
    toShelleyMetadataHash (TxMetadataShelley m) = Shelley.hashMetaData m

    toShelleyWdrl :: [(StakeAddress, Lovelace)] -> Shelley.Wdrl StandardShelley
    toShelleyWdrl wdrls =
        Shelley.Wdrl $
          Map.fromList
            [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
            | (stakeAddr, value) <- wdrls ]


-- ----------------------------------------------------------------------------
-- Signed transactions
--

data Tx era where

     ByronTx
       :: Byron.ATxAux ByteString
       -> Tx Byron

     ShelleyTx
       :: Shelley.Tx StandardShelley
       -> Tx Shelley

deriving instance Eq (Tx Byron)
deriving instance Show (Tx Byron)

deriving instance Eq (Tx Shelley)
deriving instance Show (Tx Shelley)

instance HasTypeProxy (Tx Byron) where
    data AsType (Tx Byron) = AsByronTx
    proxyToAsType _ = AsByronTx

instance HasTypeProxy (Tx Shelley) where
    data AsType (Tx Shelley) = AsShelleyTx
    proxyToAsType _ = AsShelleyTx


instance SerialiseAsCBOR (Tx Byron) where
    serialiseToCBOR (ByronTx tx) = CBOR.recoverBytes tx

    deserialiseFromCBOR AsByronTx bs =
      ByronTx <$>
        CBOR.decodeFullAnnotatedBytes "Byron Tx" fromCBOR (LBS.fromStrict bs)

instance SerialiseAsCBOR (Tx Shelley) where
    serialiseToCBOR (ShelleyTx tx) =
      CBOR.serialize' tx

    deserialiseFromCBOR AsShelleyTx bs =
      ShelleyTx <$>
        CBOR.decodeAnnotator "Shelley Tx" fromCBOR (LBS.fromStrict bs)

instance HasTextEnvelope (Tx Byron) where
    textEnvelopeType _ = "TxSignedByron"

instance HasTextEnvelope (Tx Shelley) where
    textEnvelopeType _ = "TxSignedShelley"


data Witness era where

     ByronKeyWitness
       :: Byron.TxInWitness
       -> Witness Byron

     ShelleyBootstrapWitness
       :: Shelley.BootstrapWitness StandardShelley
       -> Witness Shelley

     ShelleyKeyWitness
       :: Shelley.WitVKey StandardShelley Shelley.Witness
       -> Witness Shelley

     ShelleyScriptWitness
       :: Shelley.Script StandardShelley
       -> Witness Shelley

deriving instance Eq (Witness Byron)
deriving instance Show (Witness Byron)

deriving instance Eq (Witness Shelley)
deriving instance Show (Witness Shelley)

instance HasTypeProxy (Witness Byron) where
    data AsType (Witness Byron) = AsByronWitness
    proxyToAsType _ = AsByronWitness

instance HasTypeProxy (Witness Shelley) where
    data AsType (Witness Shelley) = AsShelleyWitness
    proxyToAsType _ = AsShelleyWitness

instance SerialiseAsCBOR (Witness Byron) where
    serialiseToCBOR (ByronKeyWitness wit) = CBOR.serialize' wit

    deserialiseFromCBOR AsByronWitness bs =
      ByronKeyWitness <$> CBOR.decodeFull' bs

instance SerialiseAsCBOR (Witness Shelley) where
    serialiseToCBOR = CBOR.serializeEncoding' . encodeShelleyWitness
      where
        encodeShelleyWitness :: Witness Shelley -> CBOR.Encoding
        encodeShelleyWitness (ShelleyKeyWitness    wit) =
            CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> toCBOR wit
        encodeShelleyWitness (ShelleyBootstrapWitness wit) =
            CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> toCBOR wit
        encodeShelleyWitness (ShelleyScriptWitness wit) =
            CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> toCBOR wit

    deserialiseFromCBOR AsShelleyWitness bs =
        CBOR.decodeAnnotator "Shelley Witness"
                             decodeShelleyWitness (LBS.fromStrict bs)
      where
        decodeShelleyWitness :: CBOR.Decoder s (CBOR.Annotator (Witness Shelley))
        decodeShelleyWitness =  do
          CBOR.decodeListLenOf 2
          t <- CBOR.decodeWord
          case t of
            0 -> fmap (fmap ShelleyKeyWitness) fromCBOR
            1 -> fmap (fmap ShelleyBootstrapWitness) fromCBOR
            2 -> fmap (fmap ShelleyScriptWitness) fromCBOR
            _ -> CBOR.cborError $ CBOR.DecoderErrorUnknownTag
                                    "Shelley Witness" (fromIntegral t)

instance HasTextEnvelope (Witness Byron) where
    textEnvelopeType _ = "TxWitnessByron"

instance HasTextEnvelope (Witness Shelley) where
    textEnvelopeType _ = "TxWitnessShelley"


getTxBody :: Tx era -> TxBody era
getTxBody (ByronTx Byron.ATxAux { Byron.aTaTx = txbody }) =
    ByronTxBody txbody

getTxBody (ShelleyTx Shelley.Tx {
                       Shelley._body     = txbody,
                       Shelley._metadata = txmetadata
                     }) =
    ShelleyTxBody txbody (strictMaybeToMaybe txmetadata)


getTxWitnesses :: Tx era -> [Witness era]
getTxWitnesses (ByronTx Byron.ATxAux { Byron.aTaWitness = witnesses }) =
    map ByronKeyWitness
  . Vector.toList
  . unAnnotated
  $ witnesses

getTxWitnesses (ShelleyTx Shelley.Tx {
                       Shelley._witnessSet =
                         Shelley.WitnessSet {
                           Shelley.addrWits,
                           Shelley.bootWits,
                           Shelley.msigWits
                         }
                     }) =
    map ShelleyBootstrapWitness (Set.elems bootWits)
 ++ map ShelleyKeyWitness       (Set.elems addrWits)
 ++ map (ShelleyScriptWitness . Shelley.MultiSigScript) (Map.elems msigWits)


makeSignedTransaction :: [Witness era]
                      -> TxBody era
                      -> Tx era
makeSignedTransaction witnesses (ByronTxBody txbody) =
    ByronTx
  . Byron.annotateTxAux
  $ Byron.mkTxAux
      (unAnnotated txbody)
      (Vector.fromList (map selectByronWitness witnesses))
  where
    selectByronWitness :: Witness Byron -> Byron.TxInWitness
    selectByronWitness (ByronKeyWitness w) = w

makeSignedTransaction witnesses (ShelleyTxBody txbody txmetadata) =
    ShelleyTx $
      Shelley.Tx
        txbody
        Shelley.WitnessSet {
          Shelley.bootWits = Set.fromList
                               [ w | ShelleyBootstrapWitness w <- witnesses ],
          Shelley.addrWits = Set.fromList
                               [ w | ShelleyKeyWitness w <- witnesses ],
          Shelley.msigWits = Map.fromList
                               [ (Shelley.hashMultiSigScript sw, sw)
                               | ShelleyScriptWitness
                                   (Shelley.MultiSigScript sw) <- witnesses ]
        }
        (maybeToStrictMaybe txmetadata)

makeByronKeyWitness :: NetworkId
                    -> TxBody Byron
                    -> SigningKey ByronKey
                    -> Witness Byron
makeByronKeyWitness nw (ByronTxBody txbody) =
    let txhash :: Byron.Hash Byron.Tx
        txhash = Byron.hashDecoded txbody

        pm :: Byron.ProtocolMagicId
        pm = toByronProtocolMagicId nw

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in \(ByronSigningKey sk) ->
        ByronKeyWitness $
          Byron.VKWitness
            (Byron.toVerification sk)
            (Byron.sign pm Byron.SignTx sk (Byron.TxSigData txhash))

-- | Either a network ID or a Byron address to be used in constructing a
-- Shelley bootstrap witness.
data WitnessNetworkIdOrByronAddress
  = WitnessNetworkId !NetworkId
  -- ^ Network ID.
  --
  -- If this value is used in the construction of a Shelley bootstrap witness,
  -- the result will not consist of a derivation path. If that is required,
  -- specify a 'WitnessByronAddress' value instead.
  | WitnessByronAddress !(Address Byron)
  -- ^ Byron address.
  --
  -- If this value is used in the construction of a Shelley bootstrap witness,
  -- both the network ID and derivation path will be extracted from the
  -- address and used in the construction of the witness.

makeShelleyBootstrapWitness :: WitnessNetworkIdOrByronAddress
                            -> TxBody Shelley
                            -> SigningKey ByronKey
                            -> Witness Shelley
makeShelleyBootstrapWitness nwOrAddr (ShelleyTxBody txbody _) (ByronSigningKey sk) =
    ShelleyBootstrapWitness $
      -- Byron era witnesses were weird. This reveals all that weirdness.
      Shelley.BootstrapWitness {
        Shelley.bwKey        = vk,
        Shelley.bwSig        = signature,
        Shelley.bwChainCode  = chainCode,
        Shelley.bwAttributes = attributes
      }
  where
    -- Starting with the easy bits: we /can/ convert the Byron verification key
    -- to a the pair of a Shelley verification key plus the chain code.
    --
    (vk, chainCode) = Shelley.unpackByronVKey (Byron.toVerification sk)

    -- Now the hairy bits.
    --
    -- Byron era signing keys were all /extended/ ed25519 keys. We have to
    -- produce a signature using this extended signing key directly. They
    -- /cannot/ be converted to a plain (non-extended) signing keys. Since we
    -- now support extended signing keys for the Shelley too, we are able to
    -- reuse that here.
    --
    signature :: Shelley.SignedDSIGN StandardShelley
                  (Shelley.Hash StandardShelley (Shelley.TxBody StandardShelley))
    signature = makeShelleySignature
                  txhash
                  -- Make the signature with the extended key directly:
                  (ShelleyExtendedSigningKey (Byron.unSigningKey sk))

    txhash :: Shelley.Hash StandardShelley (Shelley.TxBody StandardShelley)
    txhash = Crypto.hashWith CBOR.serialize' txbody

    -- And finally we need to provide the extra suffix bytes necessary to
    -- reconstruct the mini-Merkel tree that is a Byron address. The suffix
    -- bytes are the serialised address attributes.
    attributes =
      CBOR.serialize' $
        Byron.mkAttributes Byron.AddrAttributes {
          Byron.aaVKDerivationPath = derivationPath,
          Byron.aaNetworkMagic     = networkMagic
        }

    -- The 'WitnessNetworkIdOrByronAddress' value converted to an 'Either'.
    eitherNwOrAddr :: Either NetworkId (Address Byron)
    eitherNwOrAddr =
      case nwOrAddr of
        WitnessNetworkId nw -> Left nw
        WitnessByronAddress addr -> Right addr

    unByronAddr :: Address Byron -> Byron.Address
    unByronAddr (ByronAddress addr) = addr

    unAddrAttrs :: Address Byron -> Byron.AddrAttributes
    unAddrAttrs = Byron.attrData . Byron.addrAttributes . unByronAddr

    derivationPath :: Maybe Byron.HDAddressPayload
    derivationPath =
      either
        (const Nothing)
        (Byron.aaVKDerivationPath . unAddrAttrs)
        eitherNwOrAddr

    networkMagic :: Byron.NetworkMagic
    networkMagic =
      either
        toByronNetworkMagic
        (Byron.aaNetworkMagic . unAddrAttrs)
        eitherNwOrAddr

data ShelleyWitnessSigningKey =
       WitnessPaymentKey         (SigningKey PaymentKey)
     | WitnessPaymentExtendedKey (SigningKey PaymentExtendedKey)
     | WitnessStakeKey           (SigningKey StakeKey)
     | WitnessStakeExtendedKey   (SigningKey StakeExtendedKey)
     | WitnessStakePoolKey       (SigningKey StakePoolKey)
     | WitnessGenesisKey         (SigningKey GenesisKey)
     | WitnessGenesisExtendedKey (SigningKey GenesisExtendedKey)
     | WitnessGenesisDelegateKey (SigningKey GenesisDelegateKey)
     | WitnessGenesisDelegateExtendedKey
                                 (SigningKey GenesisDelegateExtendedKey)
     | WitnessGenesisUTxOKey     (SigningKey GenesisUTxOKey)


makeShelleyKeyWitness :: TxBody Shelley
                      -> ShelleyWitnessSigningKey
                      -> Witness Shelley
makeShelleyKeyWitness (ShelleyTxBody txbody _) =
    let txhash :: Shelley.Hash StandardShelley (Shelley.TxBody StandardShelley)
        txhash = Crypto.hashWith CBOR.serialize' txbody

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in \wsk ->
        let sk        = toShelleySigningKey wsk
            vk        = getShelleyKeyWitnessVerificationKey sk
            signature = makeShelleySignature txhash sk
         in ShelleyKeyWitness $
              Shelley.WitVKey vk signature


-- | We support making key witnesses with both normal and extended signing keys.
--
data ShelleySigningKey =
       -- | A normal ed25519 signing key
       ShelleyNormalSigningKey   (Shelley.SignKeyDSIGN StandardShelley)

       -- | An extended ed25519 signing key
     | ShelleyExtendedSigningKey Crypto.HD.XPrv


toShelleySigningKey :: ShelleyWitnessSigningKey -> ShelleySigningKey
toShelleySigningKey key = case key of
  WitnessPaymentKey     (PaymentSigningKey     sk) -> ShelleyNormalSigningKey sk
  WitnessStakeKey       (StakeSigningKey       sk) -> ShelleyNormalSigningKey sk
  WitnessStakePoolKey   (StakePoolSigningKey   sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisKey     (GenesisSigningKey     sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisUTxOKey (GenesisUTxOSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisDelegateKey (GenesisDelegateSigningKey sk) ->
    ShelleyNormalSigningKey sk

  -- The cases for extended keys
  WitnessPaymentExtendedKey (PaymentExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessStakeExtendedKey (StakeExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessGenesisExtendedKey (GenesisExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessGenesisDelegateExtendedKey (GenesisDelegateExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk


getShelleyKeyWitnessVerificationKey
  :: ShelleySigningKey
  -> Shelley.VKey Shelley.Witness StandardShelley
getShelleyKeyWitnessVerificationKey (ShelleyNormalSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardShelley
                             -> Shelley.VKey Shelley.Witness StandardShelley)
    . (\(PaymentVerificationKey vk) -> vk)
    . getVerificationKey
    . PaymentSigningKey
    $ sk

getShelleyKeyWitnessVerificationKey (ShelleyExtendedSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardShelley
                             -> Shelley.VKey Shelley.Witness StandardShelley)
    . (\(PaymentVerificationKey vk) -> vk)
    . (castVerificationKey :: VerificationKey PaymentExtendedKey
                           -> VerificationKey PaymentKey)
    . getVerificationKey
    . PaymentExtendedSigningKey
    $ sk


makeShelleySignature
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> ShelleySigningKey
  -> Shelley.SignedDSIGN StandardShelley tosign
makeShelleySignature tosign (ShelleyNormalSigningKey sk) =
    Crypto.signedDSIGN () tosign sk

makeShelleySignature tosign (ShelleyExtendedSigningKey sk) =
    fromXSignature $
      Crypto.HD.sign
        BS.empty  -- passphrase for (unused) in-memory encryption
        sk
        (Crypto.getSignableRepresentation tosign)
  where
    fromXSignature :: Crypto.HD.XSignature
                   -> Shelley.SignedDSIGN StandardShelley b
    fromXSignature =
        Crypto.SignedDSIGN
      . fromMaybe impossible
      . Crypto.rawDeserialiseSigDSIGN
      . Crypto.HD.unXSignature

    impossible =
      error "makeShelleyKeyWitnessSignature: byron and shelley signature sizes do not match"


-- order of signing keys must match txins
signByronTransaction :: NetworkId
                     -> TxBody Byron
                     -> [SigningKey ByronKey]
                     -> Tx Byron
signByronTransaction nw txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeByronKeyWitness nw txbody) sks

-- signing keys is a set
signShelleyTransaction :: TxBody Shelley
                       -> [ShelleyWitnessSigningKey]
                       -> Tx Shelley
signShelleyTransaction txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeShelleyKeyWitness txbody) sks


-- ----------------------------------------------------------------------------
-- Transaction fees
--

-- | For a concrete fully-constructed transaction, determine the minimum fee
-- that it needs to pay.
--
-- This function is simple, but if you are doing input selection then you
-- probably want to consider estimateTransactionFee.
--
transactionFee :: Natural -- ^ The fixed tx fee
               -> Natural -- ^ The tx fee per byte
               -> Tx Shelley
               -> Lovelace
transactionFee txFeeFixed txFeePerByte (ShelleyTx tx) =
    Lovelace (a * x + b)
  where
    a = toInteger txFeePerByte
    x = Shelley.txsize tx
    b = toInteger txFeeFixed

--TODO: in the Byron case the per-byte is non-integral, would need different
-- parameters. e.g. a new data type for fee params, Byron vs Shelley

-- | This can estimate what the transaction fee will be, based on a starting
-- base transaction, plus the numbers of the additional components of the
-- transaction that may be added.
--
-- So for example with wallet coin selection, the base transaction should
-- contain all the things not subject to coin selection (such as script inputs,
-- metadata, withdrawals, certs etc)
--
estimateTransactionFee :: NetworkId
                       -> Natural -- ^ The fixed tx fee
                       -> Natural -- ^ The tx fee per byte
                       -> Tx Shelley
                       -> Int -- ^ The number of extra UTxO transaction inputs
                       -> Int -- ^ The number of extra transaction outputs
                       -> Int -- ^ The number of extra Shelley key witnesses
                       -> Int -- ^ The number of extra Byron key witnesses
                       -> Lovelace
estimateTransactionFee nw txFeeFixed txFeePerByte (ShelleyTx tx) =
    let Lovelace baseFee = transactionFee txFeeFixed txFeePerByte (ShelleyTx tx)
     in \nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses ->

        --TODO: this is fragile. Move something like this to the ledger and
        -- make it robust, based on the txsize calculation.
        let extraBytes :: Int
            extraBytes = nInputs               * sizeInput
                       + nOutputs              * sizeOutput
                       + nByronKeyWitnesses    * sizeByronKeyWitnesses
                       + nShelleyKeyWitnesses  * sizeShelleyKeyWitnesses

         in Lovelace (baseFee + toInteger txFeePerByte * toInteger extraBytes)
  where
    sizeInput               = smallArray + uint + hashObj
    sizeOutput              = smallArray + uint + address
    sizeByronKeyWitnesses   = smallArray + keyObj + sigObj + ccodeObj + attrsObj
    sizeShelleyKeyWitnesses = smallArray + keyObj + sigObj

    smallArray  = 1
    uint        = 5

    hashObj     = 2 + hashLen
    hashLen     = 32

    keyObj      = 2 + keyLen
    keyLen      = 32

    sigObj      = 2 + sigLen
    sigLen      = 64

    ccodeObj    = 2 + ccodeLen
    ccodeLen    = 32

    address     = 2 + addrHeader + 2 * addrHashLen
    addrHeader  = 1
    addrHashLen = 28

    attrsObj    = 2 + BS.length attributes
    attributes  = CBOR.serialize' $
                    Byron.mkAttributes Byron.AddrAttributes {
                      Byron.aaVKDerivationPath = Nothing,
                      Byron.aaNetworkMagic     = toByronNetworkMagic nw
                    }


-- ----------------------------------------------------------------------------
-- Scripts
--

newtype Script = Script (Shelley.Script StandardShelley)
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToCBOR)

newtype instance Hash Script = ScriptHash (Shelley.ScriptHash StandardShelley)
  deriving (Eq, Ord, Show)

data MultiSigScript = RequireSignature (Hash PaymentKey)
                    | RequireAllOf [MultiSigScript]
                    | RequireAnyOf [MultiSigScript]
                    | RequireMOf Int [MultiSigScript]
  deriving (Eq, Show)

instance ToJSON MultiSigScript where
  toJSON (RequireSignature pKeyHash) =
    object [ "keyHash" .= String (Text.decodeUtf8 . serialiseToRawBytesHex $ pKeyHash)
           , "type" .= String "sig"
           ]
  toJSON (RequireAnyOf reqScripts) =
    object [ "type" .= String "any", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireAllOf reqScripts) =
    object [ "type" .= String "all", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireMOf reqNum reqScripts) =
    object [ "type" .= String "atLeast"
           , "required" .= reqNum
           , "scripts" .= map toJSON reqScripts
           ]

instance FromJSON MultiSigScript where
  parseJSON = parseScript

parseScript :: Value -> Aeson.Parser MultiSigScript
parseScript v = parseScriptSig v
                  <|> parseScriptAny v
                  <|> parseScriptAll v
                  <|> parseScriptAtLeast v

parseScriptAny :: Value -> Aeson.Parser MultiSigScript
parseScriptAny = Aeson.withObject "any" $ \obj -> do
  t <- obj .: "type"
  case t :: Text of
    "any" -> do s <- obj .: "scripts"
                RequireAnyOf <$> gatherMultiSigScripts s
    _ -> fail "\"any\" multi-signature script value not found"

parseScriptAll :: Value -> Aeson.Parser MultiSigScript
parseScriptAll = Aeson.withObject "all" $ \obj -> do
  t <- obj .: "type"
  case t :: Text of
    "all" -> do s <- obj .: "scripts"
                RequireAllOf <$> gatherMultiSigScripts s
    _ -> fail "\"all\" multi-signature script value not found"

parseScriptAtLeast :: Value -> Aeson.Parser MultiSigScript
parseScriptAtLeast = Aeson.withObject "atLeast" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "atLeast" -> do
      r <- obj .: "required"
      s <- obj .: "scripts"
      case r of
        Number sci ->
          case toBoundedInteger sci of
            Just reqInt ->
              do msigscripts <- gatherMultiSigScripts s
                 let numScripts = length msigscripts
                 when
                   (reqInt > numScripts)
                   (fail $ "Required number of script signatures exceeds the number of scripts."
                         <> " Required number: " <> show reqInt
                         <> " Number of scripts: " <> show numScripts)
                 return $ RequireMOf reqInt msigscripts
            Nothing -> fail $ "Error in multi-signature \"required\" key: "
                            <> show sci <> " is not a valid Int"
        _ -> fail "\"required\" value should be an integer"
    _        -> fail "\"atLeast\" multi-signature script value not found"

parseScriptSig :: Value -> Aeson.Parser MultiSigScript
parseScriptSig = Aeson.withObject "sig" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "sig" -> do k <- obj .: "keyHash"
                RequireSignature <$> convertToHash k
    _     -> fail "\"sig\" multi-signature script value not found"

convertToHash :: Text -> Aeson.Parser (Hash PaymentKey)
convertToHash txt = case deserialiseFromRawBytesHex (AsHash AsPaymentKey) $ Text.encodeUtf8 txt of
                      Just payKeyHash -> return payKeyHash
                      Nothing -> fail $ "Error deserialising payment key hash: " <> Text.unpack txt

gatherMultiSigScripts :: Vector Value -> Aeson.Parser [MultiSigScript]
gatherMultiSigScripts vs = sequence . Vector.toList $ Vector.map parseScript vs

instance HasTypeProxy Script where
    data AsType Script = AsScript
    proxyToAsType _ = AsScript

instance SerialiseAsRawBytes (Hash Script) where
    serialiseToRawBytes (ScriptHash (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes (AsHash AsScript) bs =
      ScriptHash . Shelley.ScriptHash <$> Crypto.hashFromBytes bs

instance SerialiseAsCBOR Script where
    serialiseToCBOR (Script s) =
      CBOR.serialize' s

    deserialiseFromCBOR AsScript bs =
      Script <$>
        CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

instance HasTextEnvelope Script where
    textEnvelopeType _ = "Script"
    textEnvelopeDefaultDescr (Script script) =
      case script of
        Shelley.MultiSigScript {} -> "Multi-signature script"


scriptHash :: Script -> Hash Script
scriptHash (Script s) = ScriptHash (Shelley.hashAnyScript s)

makeMultiSigScript :: MultiSigScript -> Script
makeMultiSigScript = Script . Shelley.MultiSigScript . go
  where
    go :: MultiSigScript -> Shelley.MultiSig StandardShelley
    go (RequireSignature (PaymentKeyHash kh))
                        = Shelley.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Shelley.RequireAllOf (map go s)
    go (RequireAnyOf s) = Shelley.RequireAnyOf (map go s)
    go (RequireMOf m s) = Shelley.RequireMOf m (map go s)

makeShelleyScriptWitness :: Script -> Witness Shelley
makeShelleyScriptWitness (Script s) = ShelleyScriptWitness s


-- ----------------------------------------------------------------------------
-- Certificates embedded in transactions
--

newtype Certificate = Certificate (Shelley.DCert StandardShelley)
  deriving stock (Eq, Show)
  deriving newtype (ToCBOR, FromCBOR)
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy Certificate where
    data AsType Certificate = AsCertificate
    proxyToAsType _ = AsCertificate

instance HasTextEnvelope Certificate where
    textEnvelopeType _ = "CertificateShelley"
    textEnvelopeDefaultDescr (Certificate cert) = case cert of
      Shelley.DCertDeleg Shelley.RegKey {}    -> "Stake address registration"
      Shelley.DCertDeleg Shelley.DeRegKey {}  -> "Stake address de-registration"
      Shelley.DCertDeleg Shelley.Delegate {}  -> "Stake address delegation"
      Shelley.DCertPool Shelley.RegPool {}    -> "Pool registration"
      Shelley.DCertPool Shelley.RetirePool {} -> "Pool retirement"
      Shelley.DCertGenesis{}                  -> "Genesis key delegation"
      Shelley.DCertMir{}                      -> "MIR"

makeStakeAddressRegistrationCertificate
  :: StakeCredential
  -> Certificate
makeStakeAddressRegistrationCertificate stakecred =
    Certificate
  . Shelley.DCertDeleg
  $ Shelley.RegKey
      (toShelleyStakeCredential stakecred)

makeStakeAddressDeregistrationCertificate
  :: StakeCredential
  -> Certificate
makeStakeAddressDeregistrationCertificate stakecred =
    Certificate
  . Shelley.DCertDeleg
  $ Shelley.DeRegKey
      (toShelleyStakeCredential stakecred)

makeStakeAddressDelegationCertificate
  :: StakeCredential
  -> PoolId
  -> Certificate
makeStakeAddressDelegationCertificate stakecred (StakePoolKeyHash poolid) =
    Certificate
  . Shelley.DCertDeleg
  . Shelley.Delegate
  $ Shelley.Delegation
      (toShelleyStakeCredential stakecred)
      poolid

makeGenesisKeyDelegationCertificate
  :: Hash GenesisKey
  -> Hash GenesisDelegateKey
  -> Hash VrfKey
  -> Certificate
makeGenesisKeyDelegationCertificate (GenesisKeyHash         genesiskh)
                                    (GenesisDelegateKeyHash delegatekh)
                                    (VrfKeyHash             vrfkh) =
    Certificate
  . Shelley.DCertGenesis
  $ Shelley.GenesisDelegCert
      genesiskh
      delegatekh
      vrfkh

makeMIRCertificate
  :: MIRPot
  -> [(StakeCredential, Lovelace)]
  -> Certificate
makeMIRCertificate mirpot amounts =
    Certificate
  . Shelley.DCertMir
  $ Shelley.MIRCert
      mirpot
      (Map.fromListWith (<>)
         [ (toShelleyStakeCredential sc, toShelleyLovelace v)
         | (sc, v) <- amounts ])



-- ----------------------------------------------------------------------------
-- Stake pool certificates
--

makeStakePoolRegistrationCertificate
  :: StakePoolParameters
  -> Certificate
makeStakePoolRegistrationCertificate poolparams =
    Certificate
  . Shelley.DCertPool
  $ Shelley.RegPool
      (toShelleyPoolParams poolparams)

makeStakePoolRetirementCertificate
  :: PoolId
  -> EpochNo
  -> Certificate
makeStakePoolRetirementCertificate (StakePoolKeyHash poolid) epochno =
    Certificate
  . Shelley.DCertPool
  $ Shelley.RetirePool
      poolid
      epochno

type PoolId = Hash StakePoolKey

data StakePoolParameters =
     StakePoolParameters {
       stakePoolId            :: PoolId,
       stakePoolVRF           :: Hash VrfKey,
       stakePoolCost          :: Lovelace,
       stakePoolMargin        :: Rational,
       stakePoolRewardAccount :: StakeAddress,
       stakePoolPledge        :: Lovelace,
       stakePoolOwners        :: [Hash StakeKey],
       stakePoolRelays        :: [StakePoolRelay],
       stakePoolMetadata      :: Maybe StakePoolMetadataReference
     }
  deriving (Eq, Show)

data StakePoolRelay =

       -- | One or both of IPv4 & IPv6
       StakePoolRelayIp
          (Maybe IPv4) (Maybe IPv6) (Maybe PortNumber)

       -- | An DNS name pointing to a @A@ or @AAAA@ record.
     | StakePoolRelayDnsARecord
          ByteString (Maybe PortNumber)

       -- | A DNS name pointing to a @SRV@ record.
     | StakePoolRelayDnsSrvRecord
          ByteString

  deriving (Eq, Show)

data StakePoolMetadataReference =
     StakePoolMetadataReference {
       stakePoolMetadataURL  :: URI.URI,
       stakePoolMetadataHash :: Hash StakePoolMetadata
     }
  deriving (Eq, Show)

toShelleyPoolParams :: StakePoolParameters -> Shelley.PoolParams StandardShelley
toShelleyPoolParams StakePoolParameters {
                      stakePoolId            = StakePoolKeyHash poolkh
                    , stakePoolVRF           = VrfKeyHash vrfkh
                    , stakePoolCost
                    , stakePoolMargin
                    , stakePoolRewardAccount
                    , stakePoolPledge
                    , stakePoolOwners
                    , stakePoolRelays
                    , stakePoolMetadata
                    } =
    --TODO: validate pool parameters
    Shelley.PoolParams {
      Shelley._poolPubKey = poolkh
    , Shelley._poolVrf    = vrfkh
    , Shelley._poolPledge = toShelleyLovelace stakePoolPledge
    , Shelley._poolCost   = toShelleyLovelace stakePoolCost
    , Shelley._poolMargin = Shelley.truncateUnitInterval
                              (fromRational stakePoolMargin)
    , Shelley._poolRAcnt  = toShelleyStakeAddr stakePoolRewardAccount
    , Shelley._poolOwners = Set.fromList
                              [ kh | StakeKeyHash kh <- stakePoolOwners ]
    , Shelley._poolRelays = Seq.fromList
                              (map toShelleyStakePoolRelay stakePoolRelays)
    , Shelley._poolMD     = toShelleyPoolMetaData <$>
                              maybeToStrictMaybe stakePoolMetadata
    }
  where
    toShelleyStakePoolRelay :: StakePoolRelay -> Shelley.StakePoolRelay
    toShelleyStakePoolRelay (StakePoolRelayIp mipv4 mipv6 mport) =
      Shelley.SingleHostAddr
        (fromIntegral <$> maybeToStrictMaybe mport)
        (maybeToStrictMaybe mipv4)
        (maybeToStrictMaybe mipv6)

    toShelleyStakePoolRelay (StakePoolRelayDnsARecord dnsname mport) =
      Shelley.SingleHostName
        (fromIntegral <$> maybeToStrictMaybe mport)
        (toShelleyDnsName dnsname)

    toShelleyStakePoolRelay (StakePoolRelayDnsSrvRecord dnsname) =
      Shelley.MultiHostName
        (toShelleyDnsName dnsname)

    toShelleyPoolMetaData :: StakePoolMetadataReference -> Shelley.PoolMetaData
    toShelleyPoolMetaData StakePoolMetadataReference {
                            stakePoolMetadataURL
                          , stakePoolMetadataHash = StakePoolMetadataHash mdh
                          } =
      Shelley.PoolMetaData {
        Shelley._poolMDUrl  = toShelleyUrl stakePoolMetadataURL
      , Shelley._poolMDHash = Crypto.hashToBytes mdh
      }

    toShelleyDnsName :: ByteString -> Shelley.DnsName
    toShelleyDnsName = fromMaybe (error "toShelleyDnsName: invalid dns name. TODO: proper validation")
                     . Shelley.textToDns
                     . Text.decodeLatin1

    toShelleyUrl :: URI.URI -> Shelley.Url
    toShelleyUrl uri = fromMaybe (error "toShelleyUrl: invalid url. TODO: proper validation")
                     . Shelley.textToUrl
                     . Text.pack
                     $ URI.uriToString id uri ""


-- ----------------------------------------------------------------------------
-- Stake pool metadata
--

-- | A representation of the required fields for off-chain stake pool metadata.
--
data StakePoolMetadata =
     StakePoolMetadata {

        -- | A name of up to 50 characters.
        stakePoolName :: !Text

        -- | A description of up to 255 characters.
     , stakePoolDescription :: !Text

        -- | A ticker of 3-5 characters, for a compact display of stake pools in
        -- a wallet.
     , stakePoolTicker :: !Text

       -- | A URL to a homepage with additional information about the pool.
       -- n.b. the spec does not specify a character limit for this field.
     , stakePoolHomepage :: !Text
     }
  deriving (Eq, Show)

newtype instance Hash StakePoolMetadata =
                 StakePoolMetadataHash (Shelley.Hash StandardShelley ByteString)
    deriving (Eq, Show)

instance HasTypeProxy StakePoolMetadata where
    data AsType StakePoolMetadata = AsStakePoolMetadata
    proxyToAsType _ = AsStakePoolMetadata

instance SerialiseAsRawBytes (Hash StakePoolMetadata) where
    serialiseToRawBytes (StakePoolMetadataHash h) = Crypto.hashToBytes h

    deserialiseFromRawBytes (AsHash AsStakePoolMetadata) bs =
      StakePoolMetadataHash <$> Crypto.hashFromBytes bs

--TODO: instance ToJSON StakePoolMetadata where

instance FromJSON StakePoolMetadata where
    parseJSON =
        Aeson.withObject "StakePoolMetadata" $ \obj ->
          StakePoolMetadata
            <$> parseName obj
            <*> parseDescription obj
            <*> parseTicker obj
            <*> obj .: "homepage"

      where
        -- Parse and validate the stake pool metadata name from a JSON object.
        -- The name must be 50 characters or fewer.
        --
        parseName :: Aeson.Object -> Aeson.Parser Text
        parseName obj = do
          name <- obj .: "name"
          if Text.length name <= 50
            then pure name
            else fail $ "\"name\" must have at most 50 characters, but it has "
                     <> show (Text.length name)
                     <> " characters."

        -- Parse and validate the stake pool metadata description
        -- The description must be 255 characters or fewer.
        --
        parseDescription :: Aeson.Object -> Aeson.Parser Text
        parseDescription obj = do
          description <- obj .: "description"
          if Text.length description <= 255
            then pure description
            else fail $
                 "\"description\" must have at most 255 characters, but it has "
              <> show (Text.length description)
              <> " characters."

        -- | Parse and validate the stake pool ticker description
        -- The ticker must be 3 to 5 characters long.
        --
        parseTicker :: Aeson.Object -> Aeson.Parser Text
        parseTicker obj = do
          ticker <- obj .: "ticker"
          let tickerLen = Text.length ticker
          if tickerLen >= 3 && tickerLen <= 5
            then pure ticker
            else fail $
                 "\"ticker\" must have at least 3 and at most 5 "
              <> "characters, but it has "
              <> show (Text.length ticker)
              <> " characters."

-- | A stake pool metadata validation error.
data StakePoolMetadataValidationError
  = StakePoolMetadataJsonDecodeError !String
  | StakePoolMetadataInvalidLengthError
    -- ^ The length of the JSON-encoded stake pool metadata exceeds the
    -- maximum.
      !Int
      -- ^ Maximum byte length.
      !Int
      -- ^ Actual byte length.
  deriving Show

instance Error StakePoolMetadataValidationError where
    displayError (StakePoolMetadataJsonDecodeError errStr) = errStr
    displayError (StakePoolMetadataInvalidLengthError maxLen actualLen) =
         "Stake pool metadata must consist of at most "
      <> show maxLen
      <> " bytes, but it consists of "
      <> show actualLen
      <> " bytes."

-- | Decode and validate the provided JSON-encoded bytes as 'StakePoolMetadata'.
-- Return the decoded metadata and the hash of the original bytes.
--
validateAndHashStakePoolMetadata
  :: ByteString
  -> Either StakePoolMetadataValidationError
            (StakePoolMetadata, Hash StakePoolMetadata)
validateAndHashStakePoolMetadata bs
  | BS.length bs <= 512 = do
      md <- first StakePoolMetadataJsonDecodeError
                  (Aeson.eitherDecodeStrict' bs)
      let mdh = StakePoolMetadataHash (Crypto.hashWith id bs)
      return (md, mdh)
  | otherwise = Left $ StakePoolMetadataInvalidLengthError 512 (BS.length bs)



-- ----------------------------------------------------------------------------
-- Metadata embedded in transactions
--

newtype TxMetadata = TxMetadataShelley Shelley.MetaData
    deriving stock (Eq, Show)

{-# COMPLETE TxMetadata #-}
pattern TxMetadata :: Map Word64 TxMetadataValue -> TxMetadata
pattern TxMetadata m <- TxMetadataShelley (fromShelleyMetaData -> m) where
    TxMetadata = TxMetadataShelley . toShelleyMetaData

data TxMetadataValue = TxMetaNumber Integer -- -2^64 .. 2^64-1
                     | TxMetaBytes  ByteString
                     | TxMetaText   Text
                     | TxMetaList   [TxMetadataValue]
                     | TxMetaMap    [(TxMetadataValue, TxMetadataValue)]
    deriving stock (Eq, Ord, Show)

-- | Merge metadata maps. When there are clashing entries the left hand side
-- takes precedence.
--
instance Semigroup TxMetadata where
    TxMetadataShelley (Shelley.MetaData m1)
      <> TxMetadataShelley (Shelley.MetaData m2) =

      TxMetadataShelley (Shelley.MetaData (m1 <> m2))

instance Monoid TxMetadata where
    mempty = TxMetadataShelley (Shelley.MetaData mempty)

instance HasTypeProxy TxMetadata where
    data AsType TxMetadata = AsTxMetadata
    proxyToAsType _ = AsTxMetadata

instance SerialiseAsCBOR TxMetadata where
    serialiseToCBOR (TxMetadataShelley tx) =
      CBOR.serialize' tx

    deserialiseFromCBOR AsTxMetadata bs =
      TxMetadataShelley <$>
        CBOR.decodeAnnotator "TxMetadata" fromCBOR (LBS.fromStrict bs)

makeTransactionMetadata :: Map Word64 TxMetadataValue -> TxMetadata
makeTransactionMetadata = TxMetadata

toShelleyMetaData :: Map Word64 TxMetadataValue -> Shelley.MetaData
toShelleyMetaData =
    Shelley.MetaData
  . Map.map toShelleyMetaDatum
  where
    toShelleyMetaDatum :: TxMetadataValue -> Shelley.MetaDatum
    toShelleyMetaDatum (TxMetaNumber x) = Shelley.I x
    toShelleyMetaDatum (TxMetaBytes  x) = Shelley.B x
    toShelleyMetaDatum (TxMetaText   x) = Shelley.S x
    toShelleyMetaDatum (TxMetaList  xs) = Shelley.List
                                            [ toShelleyMetaDatum x | x <- xs ]
    toShelleyMetaDatum (TxMetaMap   xs) = Shelley.Map
                                            [ (toShelleyMetaDatum k,
                                               toShelleyMetaDatum v)
                                            | (k,v) <- xs ]

fromShelleyMetaData :: Shelley.MetaData -> Map Word64 TxMetadataValue
fromShelleyMetaData (Shelley.MetaData mdMap) =
    Map.Lazy.map fromShelleyMetaDatum mdMap
  where
    fromShelleyMetaDatum :: Shelley.MetaDatum -> TxMetadataValue
    fromShelleyMetaDatum (Shelley.I     x) = TxMetaNumber x
    fromShelleyMetaDatum (Shelley.B     x) = TxMetaBytes  x
    fromShelleyMetaDatum (Shelley.S     x) = TxMetaText   x
    fromShelleyMetaDatum (Shelley.List xs) = TxMetaList
                                               [ fromShelleyMetaDatum x | x <- xs ]
    fromShelleyMetaDatum (Shelley.Map  xs) = TxMetaMap
                                               [ (fromShelleyMetaDatum k,
                                                  fromShelleyMetaDatum v)
                                               | (k,v) <- xs ]


-- ----------------------------------------------------------------------------
-- Protocol updates embedded in transactions
--

newtype UpdateProposal = UpdateProposal (Shelley.Update StandardShelley)
    deriving stock (Eq, Show)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

instance HasTypeProxy UpdateProposal where
    data AsType UpdateProposal = AsUpdateProposal
    proxyToAsType _ = AsUpdateProposal

instance HasTextEnvelope UpdateProposal where
    textEnvelopeType _ = "UpdateProposalShelley"

data ProtocolParametersUpdate =
     ProtocolParametersUpdate {

       -- | Protocol version, major and minor. Updating the major version is
       -- used to trigger hard forks.
       --
       protocolUpdateProtocolVersion :: Maybe (Natural, Natural),

       -- | The decentralization parameter. This is fraction of slots that
       -- belong to the BFT overlay schedule, rather than the Praos schedule.
       -- So 1 means fully centralised, while 0 means fully decentralised.
       --
       -- This is the \"d\" parameter from the design document.
       --
       protocolUpdateDecentralization :: Maybe Rational,

       -- | Extra entropy for the Praos per-epoch nonce.
       --
       -- This can be used to add extra entropy during the decentralisation
       -- process. If the extra entropy can be demonstrated to be generated
       -- randomly then this method can be used to show that the initial
       -- federated operators did not subtly bias the initial schedule so that
       -- they retain undue influence after decentralisation.
       --
       protocolUpdateExtraPraosEntropy :: Maybe (Maybe ByteString),

       -- | The maximum permitted size of a block header.
       --
       -- This must be at least as big as the largest legitimate block headers
       -- but should not be too much larger, to help prevent DoS attacks.
       --
       -- Caution: setting this to be smaller than legitimate block headers is
       -- a sure way to brick the system!
       --
       protocolUpdateMaxBlockHeaderSize :: Maybe Natural,

       -- | The maximum permitted size of the block body (that is, the block
       -- payload, without the block header).
       --
       -- This should be picked with the Praos network delta security parameter
       -- in mind. Making this too large can severely weaken the Praos
       -- consensus properties.
       --
       -- Caution: setting this to be smaller than a transaction that can
       -- change the protocol parameters is a sure way to brick the system!
       --
       protocolUpdateMaxBlockBodySize :: Maybe Natural,

       -- | The maximum permitted size of a transaction.
       --
       -- Typically this should not be too high a fraction of the block size,
       -- otherwise wastage from block fragmentation becomes a problem, and
       -- the current implementation does not use any sophisticated box packing
       -- algorithm.
       --
       protocolUpdateMaxTxSize :: Maybe Natural,

       -- | The constant factor for the minimum fee calculation.
       --
       protocolUpdateTxFeeFixed :: Maybe Natural,

       -- | The linear factor for the minimum fee calculation.
       --
       protocolUpdateTxFeePerByte :: Maybe Natural,

       -- | The minimum permitted value for new UTxO entries, ie for
       -- transaction outputs.
       --
       protocolUpdateMinUTxOValue :: Maybe Lovelace,

       -- | The deposit required to register a stake address.
       --
       protocolUpdateStakeAddressDeposit :: Maybe Lovelace,

       -- | The deposit required to register a stake pool.
       --
       protocolUpdateStakePoolDeposit :: Maybe Lovelace,

       -- | The minimum value that stake pools are permitted to declare for
       -- their cost parameter.
       --
       protocolUpdateMinPoolCost :: Maybe Lovelace,

       -- | The maximum number of epochs into the future that stake pools
       -- are permitted to schedule a retirement.
       --
       protocolUpdatePoolRetireMaxEpoch :: Maybe EpochNo,

       -- | The equilibrium target number of stake pools.
       --
       -- This is the \"k\" incentives parameter from the design document.
       --
       protocolUpdateStakePoolTargetNum :: Maybe Natural,

       -- | The influence of the pledge in stake pool rewards.
       --
       -- This is the \"a_0\" incentives parameter from the design document.
       --
       protocolUpdatePoolPledgeInfluence :: Maybe Rational,

       -- | The monetary expansion rate. This determines the fraction of the
       -- reserves that are added to the fee pot each epoch.
       --
       -- This is the \"rho\" incentives parameter from the design document.
       --
       protocolUpdateMonetaryExpansion :: Maybe Rational,

       -- | The fraction of the fee pot each epoch that goes to the treasury.
       --
       -- This is the \"tau\" incentives parameter from the design document.
       --
       protocolUpdateTreasuryCut :: Maybe Rational
    }
  deriving (Eq, Show)

instance Semigroup ProtocolParametersUpdate where
    ppu1 <> ppu2 =
      ProtocolParametersUpdate {
        protocolUpdateProtocolVersion     = merge protocolUpdateProtocolVersion
      , protocolUpdateDecentralization    = merge protocolUpdateDecentralization
      , protocolUpdateExtraPraosEntropy   = merge protocolUpdateExtraPraosEntropy
      , protocolUpdateMaxBlockHeaderSize  = merge protocolUpdateMaxBlockHeaderSize
      , protocolUpdateMaxBlockBodySize    = merge protocolUpdateMaxBlockBodySize
      , protocolUpdateMaxTxSize           = merge protocolUpdateMaxTxSize
      , protocolUpdateTxFeeFixed          = merge protocolUpdateTxFeeFixed
      , protocolUpdateTxFeePerByte        = merge protocolUpdateTxFeePerByte
      , protocolUpdateMinUTxOValue        = merge protocolUpdateMinUTxOValue
      , protocolUpdateStakeAddressDeposit = merge protocolUpdateStakeAddressDeposit
      , protocolUpdateStakePoolDeposit    = merge protocolUpdateStakePoolDeposit
      , protocolUpdateMinPoolCost         = merge protocolUpdateMinPoolCost
      , protocolUpdatePoolRetireMaxEpoch  = merge protocolUpdatePoolRetireMaxEpoch
      , protocolUpdateStakePoolTargetNum  = merge protocolUpdateStakePoolTargetNum
      , protocolUpdatePoolPledgeInfluence = merge protocolUpdatePoolPledgeInfluence
      , protocolUpdateMonetaryExpansion   = merge protocolUpdateMonetaryExpansion
      , protocolUpdateTreasuryCut         = merge protocolUpdateTreasuryCut
      }
      where
        -- prefer the right hand side:
        merge :: (ProtocolParametersUpdate -> Maybe a) -> Maybe a
        merge f = f ppu2 `mplus` f ppu1

instance Monoid ProtocolParametersUpdate where
    mempty =
      ProtocolParametersUpdate {
        protocolUpdateProtocolVersion     = Nothing
      , protocolUpdateDecentralization    = Nothing
      , protocolUpdateExtraPraosEntropy   = Nothing
      , protocolUpdateMaxBlockHeaderSize  = Nothing
      , protocolUpdateMaxBlockBodySize    = Nothing
      , protocolUpdateMaxTxSize           = Nothing
      , protocolUpdateTxFeeFixed          = Nothing
      , protocolUpdateTxFeePerByte        = Nothing
      , protocolUpdateMinUTxOValue        = Nothing
      , protocolUpdateStakeAddressDeposit = Nothing
      , protocolUpdateStakePoolDeposit    = Nothing
      , protocolUpdateMinPoolCost         = Nothing
      , protocolUpdatePoolRetireMaxEpoch  = Nothing
      , protocolUpdateStakePoolTargetNum  = Nothing
      , protocolUpdatePoolPledgeInfluence = Nothing
      , protocolUpdateMonetaryExpansion   = Nothing
      , protocolUpdateTreasuryCut         = Nothing
      }

makeShelleyUpdateProposal :: ProtocolParametersUpdate
                          -> [Hash GenesisKey]
                          -> EpochNo
                          -> UpdateProposal
makeShelleyUpdateProposal params genesisKeyHashes epochno =
    --TODO decide how to handle parameter validation
    let ppup = toShelleyPParamsUpdate params in
    UpdateProposal $
      Shelley.Update
        (Shelley.ProposedPPUpdates
           (Map.fromList
              [ (kh, ppup) | GenesisKeyHash kh <- genesisKeyHashes ]))
        epochno

toShelleyPParamsUpdate :: ProtocolParametersUpdate
                       -> Shelley.PParamsUpdate StandardShelley
toShelleyPParamsUpdate
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    , protocolUpdateExtraPraosEntropy
    , protocolUpdateMaxBlockHeaderSize
    , protocolUpdateMaxBlockBodySize
    , protocolUpdateMaxTxSize
    , protocolUpdateTxFeeFixed
    , protocolUpdateTxFeePerByte
    , protocolUpdateMinUTxOValue
    , protocolUpdateStakeAddressDeposit
    , protocolUpdateStakePoolDeposit
    , protocolUpdateMinPoolCost
    , protocolUpdatePoolRetireMaxEpoch
    , protocolUpdateStakePoolTargetNum
    , protocolUpdatePoolPledgeInfluence
    , protocolUpdateMonetaryExpansion
    , protocolUpdateTreasuryCut
    } =
    Shelley.PParams {
      Shelley._minfeeA     = maybeToStrictMaybe protocolUpdateTxFeePerByte
    , Shelley._minfeeB     = maybeToStrictMaybe protocolUpdateTxFeeFixed
    , Shelley._maxBBSize   = maybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , Shelley._maxTxSize   = maybeToStrictMaybe protocolUpdateMaxTxSize
    , Shelley._maxBHSize   = maybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , Shelley._keyDeposit  = toShelleyLovelace <$>
                               maybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , Shelley._poolDeposit = toShelleyLovelace <$>
                               maybeToStrictMaybe protocolUpdateStakePoolDeposit
    , Shelley._eMax        = maybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , Shelley._nOpt        = maybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , Shelley._a0          = maybeToStrictMaybe protocolUpdatePoolPledgeInfluence
    , Shelley._rho         = Shelley.truncateUnitInterval . fromRational <$>
                               maybeToStrictMaybe protocolUpdateMonetaryExpansion
    , Shelley._tau         = Shelley.truncateUnitInterval . fromRational <$>
                               maybeToStrictMaybe protocolUpdateTreasuryCut
    , Shelley._d           = Shelley.truncateUnitInterval . fromRational <$>
                               maybeToStrictMaybe protocolUpdateDecentralization
    , Shelley._extraEntropy    = mkNonce <$>
                                   maybeToStrictMaybe protocolUpdateExtraPraosEntropy
    , Shelley._protocolVersion = uncurry Shelley.ProtVer <$>
                                   maybeToStrictMaybe protocolUpdateProtocolVersion
    , Shelley._minUTxOValue    = toShelleyLovelace <$>
                                   maybeToStrictMaybe protocolUpdateMinUTxOValue
    , Shelley._minPoolCost     = toShelleyLovelace <$>
                                   maybeToStrictMaybe protocolUpdateMinPoolCost
    }
  where
    mkNonce Nothing   = Shelley.NeutralNonce
    mkNonce (Just bs) = Shelley.Nonce
                      . Crypto.castHash
                      . Crypto.hashWith id
                      $ bs

-- ----------------------------------------------------------------------------
-- Operational certificates
--

data OperationalCertificate =
     OperationalCertificate
       !(Shelley.OCert StandardShelley)
       !(VerificationKey StakePoolKey)
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR

data OperationalCertificateIssueCounter =
     OperationalCertificateIssueCounter
       !Word64
       !(VerificationKey StakePoolKey) -- For consistency checking
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance ToCBOR OperationalCertificate where
    toCBOR (OperationalCertificate ocert vkey) =
      toCBOR (CBOR.CBORGroup ocert, vkey)

instance FromCBOR OperationalCertificate where
    fromCBOR = do
      (CBOR.CBORGroup ocert, vkey) <- fromCBOR
      return (OperationalCertificate ocert vkey)

instance ToCBOR OperationalCertificateIssueCounter where
    toCBOR (OperationalCertificateIssueCounter counter vkey) =
      toCBOR (counter, vkey)

instance FromCBOR OperationalCertificateIssueCounter where
    fromCBOR = do
      (counter, vkey) <- fromCBOR
      return (OperationalCertificateIssueCounter counter vkey)

instance HasTypeProxy OperationalCertificate where
    data AsType OperationalCertificate = AsOperationalCertificate
    proxyToAsType _ = AsOperationalCertificate

instance HasTypeProxy OperationalCertificateIssueCounter where
    data AsType OperationalCertificateIssueCounter = AsOperationalCertificateIssueCounter
    proxyToAsType _ = AsOperationalCertificateIssueCounter

instance HasTextEnvelope OperationalCertificate where
    textEnvelopeType _ = "NodeOperationalCertificate"

instance HasTextEnvelope OperationalCertificateIssueCounter where
    textEnvelopeType _ = "NodeOperationalCertificateIssueCounter"

data OperationalCertIssueError =
       -- | The stake pool verification key expected for the
       -- 'OperationalCertificateIssueCounter' does not match the signing key
       -- supplied for signing.
       --
       -- Order: pool vkey expected, pool skey supplied
       --
       OperationalCertKeyMismatch (VerificationKey StakePoolKey)
                                  (VerificationKey StakePoolKey)
  deriving Show

instance Error OperationalCertIssueError where
    displayError (OperationalCertKeyMismatch _counterKey _signingKey) =
      "Key mismatch: the signing key does not match the one that goes with the counter"
      --TODO: include key ids

issueOperationalCertificate :: VerificationKey KesKey
                            -> Either (SigningKey StakePoolKey)
                                      (SigningKey GenesisDelegateExtendedKey)
                               --TODO: this may be better with a type that
                               -- captured the three (four?) choices, stake pool
                               -- or genesis delegate, extended or normal.
                            -> Shelley.KESPeriod
                            -> OperationalCertificateIssueCounter
                            -> Either OperationalCertIssueError
                                      (OperationalCertificate,
                                      OperationalCertificateIssueCounter)
issueOperationalCertificate (KesVerificationKey kesVKey)
                            skey
                            kesPeriod
                            (OperationalCertificateIssueCounter counter poolVKey)
    | poolVKey /= poolVKey'
    = Left (OperationalCertKeyMismatch poolVKey poolVKey')

    | otherwise
    = Right (OperationalCertificate ocert poolVKey,
            OperationalCertificateIssueCounter (succ counter) poolVKey)
  where
    poolVKey' :: VerificationKey StakePoolKey
    poolVKey' = either getVerificationKey (convert . getVerificationKey) skey
      where
        convert :: VerificationKey GenesisDelegateExtendedKey
                -> VerificationKey StakePoolKey
        convert = (castVerificationKey :: VerificationKey GenesisDelegateKey
                                       -> VerificationKey StakePoolKey)
                . (castVerificationKey :: VerificationKey GenesisDelegateExtendedKey
                                       -> VerificationKey GenesisDelegateKey)

    ocert     :: Shelley.OCert StandardShelley
    ocert     = Shelley.OCert kesVKey counter kesPeriod signature

    signature :: Shelley.SignedDSIGN
                   StandardShelley
                   (Shelley.OCertSignable StandardShelley)
    signature = makeShelleySignature
                  (Shelley.OCertSignable kesVKey counter kesPeriod)
                  skey'
      where
        skey' :: ShelleySigningKey
        skey' = case skey of
                  Left (StakePoolSigningKey poolSKey) ->
                    ShelleyNormalSigningKey poolSKey
                  Right (GenesisDelegateExtendedSigningKey delegSKey) ->
                    ShelleyExtendedSigningKey delegSKey


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
      -> LocalStateQueryClient block (Query block) IO ()
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


-- ----------------------------------------------------------------------------
-- CBOR serialisation
--

class HasTypeProxy a => SerialiseAsCBOR a where
    serialiseToCBOR :: a -> ByteString
    deserialiseFromCBOR :: AsType a -> ByteString -> Either CBOR.DecoderError a

    default serialiseToCBOR :: ToCBOR a => a -> ByteString
    serialiseToCBOR = CBOR.serialize'

    default deserialiseFromCBOR :: FromCBOR a
                                => AsType a
                                -> ByteString
                                -> Either CBOR.DecoderError a
    deserialiseFromCBOR _proxy = CBOR.decodeFull'


-- ----------------------------------------------------------------------------
-- JSON serialisation
--

newtype JsonDecodeError = JsonDecodeError String

serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

deserialiseFromJSON :: FromJSON a
                    => AsType a
                    -> ByteString
                    -> Either JsonDecodeError a
deserialiseFromJSON _proxy = either (Left . JsonDecodeError) Right
                           . Aeson.eitherDecodeStrict'


-- ----------------------------------------------------------------------------
-- Raw binary serialisation
--

class HasTypeProxy a => SerialiseAsRawBytes a where

  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Maybe a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

deserialiseFromRawBytesHex :: SerialiseAsRawBytes a
                           => AsType a -> ByteString -> Maybe a
deserialiseFromRawBytesHex proxy hex =
  case Base16.decode hex of
    (raw, trailing)
      | BS.null trailing -> deserialiseFromRawBytes proxy raw
      | otherwise        -> Nothing


-- | For use with @deriving via@, to provide 'Show' and\/or 'IsString' instances
-- using a hex encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
--
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
    show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
    fromString str =
      case Base16.decode (BSC.pack str) of
        (raw, trailing)
          | BS.null trailing ->
              case deserialiseFromRawBytes ttoken raw of
                Just x  -> UsingRawBytesHex x
                Nothing -> error ("fromString: cannot deserialise " ++ show str)
          | otherwise        ->
              error ("fromString: invalid hex " ++ show str)
      where
        ttoken :: AsType a
        ttoken = proxyToAsType Proxy


-- ----------------------------------------------------------------------------
-- Bech32 Serialisation
--

class (HasTypeProxy a, SerialiseAsRawBytes a) => SerialiseAsBech32 a where

    -- | The human readable prefix to use when encoding this value to Bech32.
    --
    bech32PrefixFor :: a -> Text

    -- | The set of human readable prefixes that can be used for this type.
    --
    bech32PrefixesPermitted :: AsType a -> [Text]


serialiseToBech32 :: SerialiseAsBech32 a => a -> Text
serialiseToBech32 a =
    Bech32.encodeLenient
      humanReadablePart
      (Bech32.dataPartFromBytes (serialiseToRawBytes a))
  where
    humanReadablePart =
      case Bech32.humanReadablePartFromText (bech32PrefixFor a) of
        Right p  -> p
        Left err -> error $ "serialiseToBech32: invalid prefix "
                         ++ show (bech32PrefixFor a)
                         ++ ", " ++ show err

deserialiseFromBech32 :: SerialiseAsBech32 a
                      => AsType a -> Text -> Either Bech32DecodeError a
deserialiseFromBech32 asType bech32Str = do
    (prefix, dataPart) <- Bech32.decodeLenient bech32Str
                            ?!. Bech32DecodingError

    let actualPrefix      = Bech32.humanReadablePartToText prefix
        permittedPrefixes = bech32PrefixesPermitted asType
    guard (actualPrefix `elem` permittedPrefixes)
      ?! Bech32UnexpectedPrefix actualPrefix (Set.fromList permittedPrefixes)

    payload <- Bech32.dataPartToBytes dataPart
                 ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

    value <- deserialiseFromRawBytes asType payload
               ?! Bech32DeserialiseFromBytesError payload

    let expectedPrefix = bech32PrefixFor value
    guard (actualPrefix == expectedPrefix)
      ?! Bech32WrongPrefix actualPrefix expectedPrefix

    return value

deserialiseAnyOfFromBech32
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> Text
  -> Either Bech32DecodeError b
deserialiseAnyOfFromBech32 types bech32Str = do
    (prefix, dataPart) <- Bech32.decodeLenient bech32Str
                            ?!. Bech32DecodingError

    let actualPrefix = Bech32.humanReadablePartToText prefix

    FromSomeType actualType fromType <-
      findForPrefix actualPrefix
        ?! Bech32UnexpectedPrefix actualPrefix permittedPrefixes

    payload <- Bech32.dataPartToBytes dataPart
                 ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

    value <- deserialiseFromRawBytes actualType payload
               ?! Bech32DeserialiseFromBytesError payload

    let expectedPrefix = bech32PrefixFor value
    guard (actualPrefix == expectedPrefix)
      ?! Bech32WrongPrefix actualPrefix expectedPrefix

    return (fromType value)
  where
    findForPrefix
      :: Text
      -> Maybe (FromSomeType SerialiseAsBech32 b)
    findForPrefix prefix =
      List.find
        (\(FromSomeType t _) -> prefix `elem` bech32PrefixesPermitted t)
        types

    permittedPrefixes :: Set Text
    permittedPrefixes =
      Set.fromList $ concat
        [ bech32PrefixesPermitted ttoken
        | FromSomeType ttoken _f <- types
        ]

-- | Bech32 decoding error.
data Bech32DecodeError =

       -- | There was an error decoding the string as Bech32.
       Bech32DecodingError !Bech32.DecodingError

       -- | The human-readable prefix in the Bech32-encoded string is not one
       -- of the ones expected.
     | Bech32UnexpectedPrefix !Text !(Set Text)

       -- | There was an error in extracting a 'ByteString' from the data part of
       -- the Bech32-encoded string.
     | Bech32DataPartToBytesError !Text

       -- | There was an error in deserialising the bytes into a value of the
       -- expected type.
     | Bech32DeserialiseFromBytesError !ByteString

       -- | The human-readable prefix in the Bech32-encoded string does not
       -- correspond to the prefix that should be used for the payload value.
     | Bech32WrongPrefix !Text !Text

  deriving (Eq, Show)

instance Error Bech32DecodeError where
  displayError err = case err of
    Bech32DecodingError decErr -> show decErr -- TODO

    Bech32UnexpectedPrefix actual permitted ->
        "Unexpected Bech32 prefix: the actual prefix is " <> show actual
     <> ", but it was expected to be "
     <> List.intercalate " or " (map show (Set.toList permitted))

    Bech32DataPartToBytesError _dataPart ->
        "There was an error in extracting the bytes from the data part of the \
        \Bech32-encoded string."

    Bech32DeserialiseFromBytesError _bytes ->
        "There was an error in deserialising the data part of the \
        \Bech32-encoded string into a value of the expected type."

    Bech32WrongPrefix actual expected ->
        "Mismatch in the Bech32 prefix: the actual prefix is " <> show actual
     <> ", but the prefix for this payload value should be " <> show expected



-- ----------------------------------------------------------------------------
-- Address Serialisation
--

-- | Address serialisation uses different serialisation formats for different
-- kinds of addresses, so it needs its own class.
--
-- In particular, Byron addresses are typically formatted in base 58, while
-- Shelley addresses (payment and stake) are formatted using Bech32.
--
class HasTypeProxy addr => SerialiseAddress addr where

    serialiseAddress :: addr -> Text

    deserialiseAddress :: AsType addr -> Text -> Maybe addr
    -- TODO: consider adding data AddressDecodeError


-- ----------------------------------------------------------------------------
-- TextEnvelope Serialisation
--

type TextEnvelope = TextView.TextView
type TextEnvelopeType = TextView.TextViewType
type TextEnvelopeDescr = TextView.TextViewDescription

class SerialiseAsCBOR a => HasTextEnvelope a where
    textEnvelopeType :: AsType a -> TextEnvelopeType

    textEnvelopeDefaultDescr :: a -> TextEnvelopeDescr
    textEnvelopeDefaultDescr _ = ""

type TextEnvelopeError = TextView.TextViewError

data FileError e = FileError   FilePath e
                 | FileIOError FilePath IOException
  deriving Show

instance Error e => Error (FileError e) where
  displayError (FileIOError path ioe) =
    path ++ ": " ++ displayException ioe
  displayError (FileError path e) =
    path ++ ": " ++ displayError e

instance Error TextView.TextViewError where
  displayError = Text.unpack . TextView.renderTextViewError

serialiseToTextEnvelope :: forall a. HasTextEnvelope a
                        => Maybe TextEnvelopeDescr -> a -> TextEnvelope
serialiseToTextEnvelope mbDescr a =
    TextView.TextView {
      TextView.tvType    = textEnvelopeType ttoken
    , TextView.tvDescription   = fromMaybe (textEnvelopeDefaultDescr a) mbDescr
    , TextView.tvRawCBOR = serialiseToCBOR a
    }
  where
    ttoken :: AsType a
    ttoken = proxyToAsType Proxy


deserialiseFromTextEnvelope :: HasTextEnvelope a
                            => AsType a
                            -> TextEnvelope
                            -> Either TextEnvelopeError a
deserialiseFromTextEnvelope ttoken te = do
    TextView.expectTextViewOfType (textEnvelopeType ttoken) te
    first TextView.TextViewDecodeError $
      deserialiseFromCBOR ttoken (TextView.tvRawCBOR te) --TODO: You have switched from CBOR to JSON

data FromSomeType (c :: Type -> Constraint) b where
     FromSomeType :: c a => AsType a -> (a -> b) -> FromSomeType c b


deserialiseFromTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                                 -> TextEnvelope
                                 -> Either TextEnvelopeError b
deserialiseFromTextEnvelopeAnyOf types te =
    case List.find matching types of
      Nothing ->
        Left (TextView.TextViewTypeError expectedTypes actualType)

      Just (FromSomeType ttoken f) ->
        first TextView.TextViewDecodeError $
          f <$> deserialiseFromCBOR ttoken (TextView.tvRawCBOR te)
  where
    actualType    = TextView.tvType te
    expectedTypes = [ textEnvelopeType ttoken
                    | FromSomeType ttoken _f <- types ]

    matching (FromSomeType ttoken _f) = actualType == textEnvelopeType ttoken


writeFileTextEnvelope :: HasTextEnvelope a
                      => FilePath
                      -> Maybe TextEnvelopeDescr
                      -> a
                      -> IO (Either (FileError ()) ())
writeFileTextEnvelope path mbDescr a =
    runExceptT $ do
      handleIOExceptT (FileIOError path) $ BS.writeFile path content
  where
    content = LBS.toStrict $ encodePretty' TextView.textViewJSONConfig (serialiseToTextEnvelope mbDescr a) <> "\n"

readFileTextEnvelope :: HasTextEnvelope a
                     => AsType a
                     -> FilePath
                     -> IO (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope ttoken path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextView.TextViewAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelope ttoken te


readFileTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                          -> FilePath
                          -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf types path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextView.TextViewAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelopeAnyOf types te

readTextEnvelopeFromFile :: FilePath
                         -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeFromFile path =
  runExceptT $ do
    bs <- handleIOExceptT (FileIOError path) $
            BS.readFile path
    firstExceptT (FileError path . TextView.TextViewAesonDecodeError)
      . hoistEither $ Aeson.eitherDecodeStrict' bs

readTextEnvelopeOfTypeFromFile
  :: TextEnvelopeType
  -> FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeOfTypeFromFile expectedType path =
  runExceptT $ do
    te <- ExceptT (readTextEnvelopeFromFile path)
    firstExceptT (FileError path) $ hoistEither $
      TextView.expectTextViewOfType expectedType te
    return te


-- ----------------------------------------------------------------------------
-- Error reporting
--

class Show e => Error e where

    displayError :: e -> String

instance Error () where
    displayError () = ""

-- | The preferred approach is to use 'Except' or 'ExceptT', but you can if
-- necessary use IO exceptions.
--
throwErrorAsException :: Error e => e -> IO a
throwErrorAsException e = throwIO (ErrorAsException e)

data ErrorAsException where
     ErrorAsException :: Error e => e -> ErrorAsException

instance Show ErrorAsException where
    show (ErrorAsException e) = show e

instance Exception ErrorAsException where
    displayException (ErrorAsException e) = displayError e

-- ----------------------------------------------------------------------------
-- Key instances
--

instance HasTypeProxy a => HasTypeProxy (VerificationKey a) where
    data AsType (VerificationKey a) = AsVerificationKey (AsType a)
    proxyToAsType _ = AsVerificationKey (proxyToAsType (Proxy :: Proxy a))

instance HasTypeProxy a => HasTypeProxy (SigningKey a) where
    data AsType (SigningKey a) = AsSigningKey (AsType a)
    proxyToAsType _ = AsSigningKey (proxyToAsType (Proxy :: Proxy a))

instance HasTypeProxy a => HasTypeProxy (Hash a) where
    data AsType (Hash a) = AsHash (AsType a)
    proxyToAsType _ = AsHash (proxyToAsType (Proxy :: Proxy a))

-- | Map the various Shelley key role types into corresponding 'Shelley.KeyRole'
-- types.
--
type family ShelleyKeyRole (keyrole :: Type) :: Shelley.KeyRole

type instance ShelleyKeyRole PaymentKey         = Shelley.Payment
type instance ShelleyKeyRole GenesisKey         = Shelley.Genesis
type instance ShelleyKeyRole GenesisUTxOKey     = Shelley.Payment
type instance ShelleyKeyRole GenesisDelegateKey = Shelley.GenesisDelegate
type instance ShelleyKeyRole StakeKey           = Shelley.Staking
type instance ShelleyKeyRole StakePoolKey       = Shelley.StakePool


--
-- Byron keys
--

-- | Byron-era payment keys. Used for Byron addresses and witnessing
-- transactions that spend from these addresses.
--
-- These use Ed25519 but with a 32byte \"chaincode\" used in HD derivation.
-- The inclusion of the chaincode is a design mistake but one that cannot
-- be corrected for the Byron era. The Shelley era 'PaymentKey's do not include
-- a chaincode. It is safe to use a zero or random chaincode for new Byron keys.
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data ByronKey

instance HasTypeProxy ByronKey where
    data AsType ByronKey = AsByronKey
    proxyToAsType _ = AsByronKey

instance Key ByronKey where

    newtype VerificationKey ByronKey =
           ByronVerificationKey Byron.VerificationKey
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey ByronKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey ByronKey =
           ByronSigningKey Byron.SigningKey
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey ByronKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType ByronKey -> Crypto.Seed -> SigningKey ByronKey
    deterministicSigningKey AsByronKey seed =
       ByronSigningKey (snd (Crypto.runMonadRandomWithSeed seed Byron.keyGen))

    deterministicSigningKeySeedSize :: AsType ByronKey -> Word
    deterministicSigningKeySeedSize AsByronKey = 32

    getVerificationKey :: SigningKey ByronKey -> VerificationKey ByronKey
    getVerificationKey (ByronSigningKey sk) =
      ByronVerificationKey (Byron.toVerification sk)

    verificationKeyHash :: VerificationKey ByronKey -> Hash ByronKey
    verificationKeyHash (ByronVerificationKey vkey) =
      ByronKeyHash (Byron.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey ByronKey) where
    serialiseToRawBytes (ByronVerificationKey (Byron.VerificationKey xvk)) =
      Crypto.HD.unXPub xvk

    deserialiseFromRawBytes (AsVerificationKey AsByronKey) bs =
      either (const Nothing) (Just . ByronVerificationKey . Byron.VerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey ByronKey) where
    serialiseToRawBytes (ByronSigningKey (Byron.SigningKey xsk)) =
      Crypto.HD.unXPrv xsk

    deserialiseFromRawBytes (AsSigningKey AsByronKey) bs =
      either (const Nothing) (Just . ByronSigningKey . Byron.SigningKey)
             (Crypto.HD.xprv bs)

instance SerialiseAsBech32 (VerificationKey ByronKey) where
    bech32PrefixFor         _ =  "addr_xvk"
    bech32PrefixesPermitted _ = ["addr_xvk"]

instance SerialiseAsBech32 (SigningKey ByronKey) where
    bech32PrefixFor         _ =  "addr_xsk"
    bech32PrefixesPermitted _ = ["addr_xsk"]


newtype instance Hash ByronKey = ByronKeyHash Byron.KeyHash
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash ByronKey) where
    serialiseToRawBytes (ByronKeyHash (Byron.KeyHash vkh)) =
      Byron.abstractHashToBytes vkh

    deserialiseFromRawBytes (AsHash AsByronKey) bs =
      ByronKeyHash . Byron.KeyHash <$> Byron.abstractHashFromBytes bs

instance HasTextEnvelope (VerificationKey ByronKey) where
    textEnvelopeType _ = "PaymentVerificationKeyByron_ed25519_bip32"

instance HasTextEnvelope (SigningKey ByronKey) where
    textEnvelopeType _ = "PaymentSigningKeyByron_ed25519_bip32"

instance CastVerificationKeyRole ByronKey PaymentExtendedKey where
    castVerificationKey (ByronVerificationKey vk) =
        PaymentExtendedVerificationKey
          (Byron.unVerificationKey vk)

instance CastVerificationKeyRole ByronKey PaymentKey where
    castVerificationKey =
        (castVerificationKey :: VerificationKey PaymentExtendedKey
                             -> VerificationKey PaymentKey)
      . (castVerificationKey :: VerificationKey ByronKey
                             -> VerificationKey PaymentExtendedKey)


--
-- Shelley payment keys
--

-- | Shelley-era payment keys. Used for Shelley payment addresses and witnessing
-- transactions that spend from these addresses.
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data PaymentKey

instance HasTypeProxy PaymentKey where
    data AsType PaymentKey = AsPaymentKey
    proxyToAsType _ = AsPaymentKey

instance Key PaymentKey where

    newtype VerificationKey PaymentKey =
        PaymentVerificationKey (Shelley.VKey Shelley.Payment StandardShelley)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey PaymentKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey PaymentKey =
        PaymentSigningKey (Shelley.SignKeyDSIGN StandardShelley)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey PaymentKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType PaymentKey -> Crypto.Seed -> SigningKey PaymentKey
    deterministicSigningKey AsPaymentKey seed =
        PaymentSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType PaymentKey -> Word
    deterministicSigningKeySeedSize AsPaymentKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey PaymentKey -> VerificationKey PaymentKey
    getVerificationKey (PaymentSigningKey sk) =
        PaymentVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey PaymentKey -> Hash PaymentKey
    verificationKeyHash (PaymentVerificationKey vkey) =
        PaymentKeyHash (Shelley.hashKey vkey)

instance SerialiseAsRawBytes (VerificationKey PaymentKey) where
    serialiseToRawBytes (PaymentVerificationKey (Shelley.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsPaymentKey) bs =
      PaymentVerificationKey . Shelley.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey PaymentKey) where
    serialiseToRawBytes (PaymentSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsPaymentKey) bs =
      PaymentSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs

instance SerialiseAsBech32 (VerificationKey PaymentKey) where
    bech32PrefixFor         _ =  "addr_vk"
    bech32PrefixesPermitted _ = ["addr_vk"]

instance SerialiseAsBech32 (SigningKey PaymentKey) where
    bech32PrefixFor         _ =  "addr_sk"
    bech32PrefixesPermitted _ = ["addr_sk"]

newtype instance Hash PaymentKey =
    PaymentKeyHash (Shelley.KeyHash Shelley.Payment StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash PaymentKey) where
    serialiseToRawBytes (PaymentKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsPaymentKey) bs =
      PaymentKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey PaymentKey) where
    textEnvelopeType _ = "PaymentVerificationKeyShelley_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey PaymentKey) where
    textEnvelopeType _ = "PaymentSigningKeyShelley_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy


--
-- Shelley payment extended ed25519 keys
--

-- | Shelley-era payment keys using extended ed25519 cryptographic keys.
--
-- They can be used for Shelley payment addresses and witnessing
-- transactions that spend from these addresses.
--
-- These extended keys are used by HD wallets. So this type provides
-- interoperability with HD wallets. The ITN CLI also supported this key type.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'PaymentKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'PaymentKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data PaymentExtendedKey

instance HasTypeProxy PaymentExtendedKey where
    data AsType PaymentExtendedKey = AsPaymentExtendedKey
    proxyToAsType _ = AsPaymentExtendedKey

instance Key PaymentExtendedKey where

    newtype VerificationKey PaymentExtendedKey =
        PaymentExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey PaymentExtendedKey)

    newtype SigningKey PaymentExtendedKey =
        PaymentExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey PaymentExtendedKey)

    deterministicSigningKey :: AsType PaymentExtendedKey
                            -> Crypto.Seed
                            -> SigningKey PaymentExtendedKey
    deterministicSigningKey AsPaymentExtendedKey seed =
        PaymentExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType PaymentExtendedKey -> Word
    deterministicSigningKeySeedSize AsPaymentExtendedKey = 32

    getVerificationKey :: SigningKey PaymentExtendedKey
                       -> VerificationKey PaymentExtendedKey
    getVerificationKey (PaymentExtendedSigningKey sk) =
        PaymentExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey PaymentExtendedKey
                        -> Hash PaymentExtendedKey
    verificationKeyHash (PaymentExtendedVerificationKey vk) =
        PaymentExtendedKeyHash
      . Shelley.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey PaymentExtendedKey) where
    toCBOR (PaymentExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey PaymentExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . PaymentExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey PaymentExtendedKey) where
    toCBOR (PaymentExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey PaymentExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . PaymentExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey PaymentExtendedKey) where
    serialiseToRawBytes (PaymentExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsPaymentExtendedKey) bs =
      either (const Nothing) (Just . PaymentExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey PaymentExtendedKey) where
    serialiseToRawBytes (PaymentExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsPaymentExtendedKey) bs =
      either (const Nothing) (Just . PaymentExtendedSigningKey)
             (Crypto.HD.xprv bs)

instance SerialiseAsBech32 (VerificationKey PaymentExtendedKey) where
    bech32PrefixFor         _ =  "addr_xvk"
    bech32PrefixesPermitted _ = ["addr_xvk"]

instance SerialiseAsBech32 (SigningKey PaymentExtendedKey) where
    bech32PrefixFor         _ =  "addr_xsk"
    bech32PrefixesPermitted _ = ["addr_xsk"]


newtype instance Hash PaymentExtendedKey =
    PaymentExtendedKeyHash (Shelley.KeyHash Shelley.Payment StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash PaymentExtendedKey) where
    serialiseToRawBytes (PaymentExtendedKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsPaymentExtendedKey) bs =
      PaymentExtendedKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey PaymentExtendedKey) where
    textEnvelopeType _ = "PaymentExtendedVerificationKeyShelley_ed25519_bip32"

instance HasTextEnvelope (SigningKey PaymentExtendedKey) where
    textEnvelopeType _ = "PaymentExtendedSigningKeyShelley_ed25519_bip32"

instance CastVerificationKeyRole PaymentExtendedKey PaymentKey where
    castVerificationKey (PaymentExtendedVerificationKey vk) =
        PaymentVerificationKey
      . Shelley.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: byron and shelley key sizes do not match!"


--
-- Stake keys
--

data StakeKey

instance HasTypeProxy StakeKey where
    data AsType StakeKey = AsStakeKey
    proxyToAsType _ = AsStakeKey

instance Key StakeKey where

    newtype VerificationKey StakeKey =
        StakeVerificationKey (Shelley.VKey Shelley.Staking StandardShelley)
      deriving stock (Eq)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey StakeKey)

    newtype SigningKey StakeKey =
        StakeSigningKey (Shelley.SignKeyDSIGN StandardShelley)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey StakeKey)

    deterministicSigningKey :: AsType StakeKey -> Crypto.Seed -> SigningKey StakeKey
    deterministicSigningKey AsStakeKey seed =
        StakeSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType StakeKey -> Word
    deterministicSigningKeySeedSize AsStakeKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey StakeKey -> VerificationKey StakeKey
    getVerificationKey (StakeSigningKey sk) =
        StakeVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey StakeKey -> Hash StakeKey
    verificationKeyHash (StakeVerificationKey vkey) =
        StakeKeyHash (Shelley.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey StakeKey) where
    serialiseToRawBytes (StakeVerificationKey (Shelley.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsStakeKey) bs =
      StakeVerificationKey . Shelley.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey StakeKey) where
    serialiseToRawBytes (StakeSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsStakeKey) bs =
      StakeSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs

instance SerialiseAsBech32 (VerificationKey StakeKey) where
    bech32PrefixFor         _ =  "stake_vk"
    bech32PrefixesPermitted _ = ["stake_vk"]

instance SerialiseAsBech32 (SigningKey StakeKey) where
    bech32PrefixFor         _ =  "stake_sk"
    bech32PrefixesPermitted _ = ["stake_sk"]


newtype instance Hash StakeKey =
    StakeKeyHash (Shelley.KeyHash Shelley.Staking StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash StakeKey) where
    serialiseToRawBytes (StakeKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsStakeKey) bs =
      StakeKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey StakeKey) where
    textEnvelopeType _ = "StakeVerificationKeyShelley_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey StakeKey) where
    textEnvelopeType _ = "StakeSigningKeyShelley_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy


--
-- Shelley stake extended ed25519 keys
--

-- | Shelley-era stake keys using extended ed25519 cryptographic keys.
--
-- They can be used for Shelley stake addresses and witnessing transactions
-- that use stake addresses.
--
-- These extended keys are used by HD wallets. So this type provides
-- interoperability with HD wallets. The ITN CLI also supported this key type.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'StakeKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'StakeKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data StakeExtendedKey

instance HasTypeProxy StakeExtendedKey where
    data AsType StakeExtendedKey = AsStakeExtendedKey
    proxyToAsType _ = AsStakeExtendedKey

instance Key StakeExtendedKey where

    newtype VerificationKey StakeExtendedKey =
        StakeExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey StakeExtendedKey)

    newtype SigningKey StakeExtendedKey =
        StakeExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey StakeExtendedKey)

    deterministicSigningKey :: AsType StakeExtendedKey
                            -> Crypto.Seed
                            -> SigningKey StakeExtendedKey
    deterministicSigningKey AsStakeExtendedKey seed =
        StakeExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType StakeExtendedKey -> Word
    deterministicSigningKeySeedSize AsStakeExtendedKey = 32

    getVerificationKey :: SigningKey StakeExtendedKey
                       -> VerificationKey StakeExtendedKey
    getVerificationKey (StakeExtendedSigningKey sk) =
        StakeExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey StakeExtendedKey
                        -> Hash StakeExtendedKey
    verificationKeyHash (StakeExtendedVerificationKey vk) =
        StakeExtendedKeyHash
      . Shelley.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey StakeExtendedKey) where
    toCBOR (StakeExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey StakeExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . StakeExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey StakeExtendedKey) where
    toCBOR (StakeExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey StakeExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . StakeExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey StakeExtendedKey) where
    serialiseToRawBytes (StakeExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsStakeExtendedKey) bs =
      either (const Nothing) (Just . StakeExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey StakeExtendedKey) where
    serialiseToRawBytes (StakeExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsStakeExtendedKey) bs =
      either (const Nothing) (Just . StakeExtendedSigningKey)
             (Crypto.HD.xprv bs)

instance SerialiseAsBech32 (VerificationKey StakeExtendedKey) where
    bech32PrefixFor         _ =  "stake_xvk"
    bech32PrefixesPermitted _ = ["stake_xvk"]

instance SerialiseAsBech32 (SigningKey StakeExtendedKey) where
    bech32PrefixFor         _ =  "stake_xsk"
    bech32PrefixesPermitted _ = ["stake_xsk"]


newtype instance Hash StakeExtendedKey =
    StakeExtendedKeyHash (Shelley.KeyHash Shelley.Staking StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash StakeExtendedKey) where
    serialiseToRawBytes (StakeExtendedKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsStakeExtendedKey) bs =
      StakeExtendedKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey StakeExtendedKey) where
    textEnvelopeType _ = "StakeExtendedVerificationKeyShelley_ed25519_bip32"

instance HasTextEnvelope (SigningKey StakeExtendedKey) where
    textEnvelopeType _ = "StakeExtendedSigningKeyShelley_ed25519_bip32"

instance CastVerificationKeyRole StakeExtendedKey StakeKey where
    castVerificationKey (StakeExtendedVerificationKey vk) =
        StakeVerificationKey
      . Shelley.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: byron and shelley key sizes do not match!"


--
-- Genesis keys
--

data GenesisKey

instance HasTypeProxy GenesisKey where
    data AsType GenesisKey = AsGenesisKey
    proxyToAsType _ = AsGenesisKey

instance Key GenesisKey where

    newtype VerificationKey GenesisKey =
        GenesisVerificationKey (Shelley.VKey Shelley.Genesis StandardShelley)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisKey =
        GenesisSigningKey (Shelley.SignKeyDSIGN StandardShelley)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisKey -> Crypto.Seed -> SigningKey GenesisKey
    deterministicSigningKey AsGenesisKey seed =
        GenesisSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisKey -> Word
    deterministicSigningKeySeedSize AsGenesisKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisKey -> VerificationKey GenesisKey
    getVerificationKey (GenesisSigningKey sk) =
        GenesisVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisKey -> Hash GenesisKey
    verificationKeyHash (GenesisVerificationKey vkey) =
        GenesisKeyHash (Shelley.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey GenesisKey) where
    serialiseToRawBytes (GenesisVerificationKey (Shelley.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsGenesisKey) bs =
      GenesisVerificationKey . Shelley.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey GenesisKey) where
    serialiseToRawBytes (GenesisSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsGenesisKey) bs =
      GenesisSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash GenesisKey =
    GenesisKeyHash (Shelley.KeyHash Shelley.Genesis StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash GenesisKey) where
    serialiseToRawBytes (GenesisKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisKey) bs =
      GenesisKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisKey) where
    textEnvelopeType _ = "GenesisVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey GenesisKey) where
    textEnvelopeType _ = "GenesisSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy


--
-- Shelley genesis extended ed25519 keys
--

-- | Shelley-era genesis keys using extended ed25519 cryptographic keys.
--
-- These serve the same role as normal genesis keys, but are here to support
-- legacy Byron genesis keys which used extended keys.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'GenesisKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'GenesisKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data GenesisExtendedKey

instance HasTypeProxy GenesisExtendedKey where
    data AsType GenesisExtendedKey = AsGenesisExtendedKey
    proxyToAsType _ = AsGenesisExtendedKey

instance Key GenesisExtendedKey where

    newtype VerificationKey GenesisExtendedKey =
        GenesisExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisExtendedKey)

    newtype SigningKey GenesisExtendedKey =
        GenesisExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisExtendedKey)

    deterministicSigningKey :: AsType GenesisExtendedKey
                            -> Crypto.Seed
                            -> SigningKey GenesisExtendedKey
    deterministicSigningKey AsGenesisExtendedKey seed =
        GenesisExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType GenesisExtendedKey -> Word
    deterministicSigningKeySeedSize AsGenesisExtendedKey = 32

    getVerificationKey :: SigningKey GenesisExtendedKey
                       -> VerificationKey GenesisExtendedKey
    getVerificationKey (GenesisExtendedSigningKey sk) =
        GenesisExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey GenesisExtendedKey
                        -> Hash GenesisExtendedKey
    verificationKeyHash (GenesisExtendedVerificationKey vk) =
        GenesisExtendedKeyHash
      . Shelley.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey GenesisExtendedKey) where
    toCBOR (GenesisExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey GenesisExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey GenesisExtendedKey) where
    toCBOR (GenesisExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey GenesisExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey GenesisExtendedKey) where
    serialiseToRawBytes (GenesisExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsGenesisExtendedKey) bs =
      either (const Nothing) (Just . GenesisExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey GenesisExtendedKey) where
    serialiseToRawBytes (GenesisExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsGenesisExtendedKey) bs =
      either (const Nothing) (Just . GenesisExtendedSigningKey)
             (Crypto.HD.xprv bs)


newtype instance Hash GenesisExtendedKey =
    GenesisExtendedKeyHash (Shelley.KeyHash Shelley.Staking StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash GenesisExtendedKey) where
    serialiseToRawBytes (GenesisExtendedKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisExtendedKey) bs =
      GenesisExtendedKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisExtendedKey) where
    textEnvelopeType _ = "GenesisExtendedVerificationKey_ed25519_bip32"

instance HasTextEnvelope (SigningKey GenesisExtendedKey) where
    textEnvelopeType _ = "GenesisExtendedSigningKey_ed25519_bip32"

instance CastVerificationKeyRole GenesisExtendedKey GenesisKey where
    castVerificationKey (GenesisExtendedVerificationKey vk) =
        GenesisVerificationKey
      . Shelley.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: byron and shelley key sizes do not match!"


--
-- Genesis delegate keys
--

data GenesisDelegateKey

instance HasTypeProxy GenesisDelegateKey where
    data AsType GenesisDelegateKey = AsGenesisDelegateKey
    proxyToAsType _ = AsGenesisDelegateKey


instance Key GenesisDelegateKey where

    newtype VerificationKey GenesisDelegateKey =
        GenesisDelegateVerificationKey (Shelley.VKey Shelley.GenesisDelegate StandardShelley)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisDelegateKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisDelegateKey =
        GenesisDelegateSigningKey (Shelley.SignKeyDSIGN StandardShelley)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisDelegateKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisDelegateKey -> Crypto.Seed -> SigningKey GenesisDelegateKey
    deterministicSigningKey AsGenesisDelegateKey seed =
        GenesisDelegateSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisDelegateKey -> Word
    deterministicSigningKeySeedSize AsGenesisDelegateKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisDelegateKey -> VerificationKey GenesisDelegateKey
    getVerificationKey (GenesisDelegateSigningKey sk) =
        GenesisDelegateVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisDelegateKey -> Hash GenesisDelegateKey
    verificationKeyHash (GenesisDelegateVerificationKey vkey) =
        GenesisDelegateKeyHash (Shelley.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey GenesisDelegateKey) where
    serialiseToRawBytes (GenesisDelegateVerificationKey (Shelley.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsGenesisDelegateKey) bs =
      GenesisDelegateVerificationKey . Shelley.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey GenesisDelegateKey) where
    serialiseToRawBytes (GenesisDelegateSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsGenesisDelegateKey) bs =
      GenesisDelegateSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash GenesisDelegateKey =
    GenesisDelegateKeyHash (Shelley.KeyHash Shelley.GenesisDelegate StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash GenesisDelegateKey) where
    serialiseToRawBytes (GenesisDelegateKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisDelegateKey) bs =
      GenesisDelegateKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisDelegateKey) where
    textEnvelopeType _ = "GenesisDelegateVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey GenesisDelegateKey) where
    textEnvelopeType _ = "GenesisDelegateSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

instance CastVerificationKeyRole GenesisDelegateKey StakePoolKey where
    castVerificationKey (GenesisDelegateVerificationKey (Shelley.VKey vkey)) =
      StakePoolVerificationKey (Shelley.VKey vkey)

instance CastSigningKeyRole GenesisDelegateKey StakePoolKey where
    castSigningKey (GenesisDelegateSigningKey skey) =
      StakePoolSigningKey skey


--
-- Shelley genesis delegate extended ed25519 keys
--

-- | Shelley-era genesis keys using extended ed25519 cryptographic keys.
--
-- These serve the same role as normal genesis keys, but are here to support
-- legacy Byron genesis keys which used extended keys.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'GenesisKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'GenesisKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data GenesisDelegateExtendedKey

instance HasTypeProxy GenesisDelegateExtendedKey where
    data AsType GenesisDelegateExtendedKey = AsGenesisDelegateExtendedKey
    proxyToAsType _ = AsGenesisDelegateExtendedKey

instance Key GenesisDelegateExtendedKey where

    newtype VerificationKey GenesisDelegateExtendedKey =
        GenesisDelegateExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisDelegateExtendedKey)

    newtype SigningKey GenesisDelegateExtendedKey =
        GenesisDelegateExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisDelegateExtendedKey)

    deterministicSigningKey :: AsType GenesisDelegateExtendedKey
                            -> Crypto.Seed
                            -> SigningKey GenesisDelegateExtendedKey
    deterministicSigningKey AsGenesisDelegateExtendedKey seed =
        GenesisDelegateExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType GenesisDelegateExtendedKey -> Word
    deterministicSigningKeySeedSize AsGenesisDelegateExtendedKey = 32

    getVerificationKey :: SigningKey GenesisDelegateExtendedKey
                       -> VerificationKey GenesisDelegateExtendedKey
    getVerificationKey (GenesisDelegateExtendedSigningKey sk) =
        GenesisDelegateExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey GenesisDelegateExtendedKey
                        -> Hash GenesisDelegateExtendedKey
    verificationKeyHash (GenesisDelegateExtendedVerificationKey vk) =
        GenesisDelegateExtendedKeyHash
      . Shelley.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey GenesisDelegateExtendedKey) where
    toCBOR (GenesisDelegateExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey GenesisDelegateExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisDelegateExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey GenesisDelegateExtendedKey) where
    toCBOR (GenesisDelegateExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey GenesisDelegateExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisDelegateExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey GenesisDelegateExtendedKey) where
    serialiseToRawBytes (GenesisDelegateExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsGenesisDelegateExtendedKey) bs =
      either (const Nothing) (Just . GenesisDelegateExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey GenesisDelegateExtendedKey) where
    serialiseToRawBytes (GenesisDelegateExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsGenesisDelegateExtendedKey) bs =
      either (const Nothing) (Just . GenesisDelegateExtendedSigningKey)
             (Crypto.HD.xprv bs)


newtype instance Hash GenesisDelegateExtendedKey =
    GenesisDelegateExtendedKeyHash (Shelley.KeyHash Shelley.Staking StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash GenesisDelegateExtendedKey) where
    serialiseToRawBytes (GenesisDelegateExtendedKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisDelegateExtendedKey) bs =
      GenesisDelegateExtendedKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisDelegateExtendedKey) where
    textEnvelopeType _ = "GenesisDelegateExtendedVerificationKey_ed25519_bip32"

instance HasTextEnvelope (SigningKey GenesisDelegateExtendedKey) where
    textEnvelopeType _ = "GenesisDelegateExtendedSigningKey_ed25519_bip32"

instance CastVerificationKeyRole GenesisDelegateExtendedKey GenesisDelegateKey where
    castVerificationKey (GenesisDelegateExtendedVerificationKey vk) =
        GenesisDelegateVerificationKey
      . Shelley.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: byron and shelley key sizes do not match!"


--
-- Genesis UTxO keys
--

data GenesisUTxOKey

instance HasTypeProxy GenesisUTxOKey where
    data AsType GenesisUTxOKey = AsGenesisUTxOKey
    proxyToAsType _ = AsGenesisUTxOKey


instance Key GenesisUTxOKey where

    newtype VerificationKey GenesisUTxOKey =
        GenesisUTxOVerificationKey (Shelley.VKey Shelley.Payment StandardShelley)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisUTxOKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisUTxOKey =
        GenesisUTxOSigningKey (Shelley.SignKeyDSIGN StandardShelley)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisUTxOKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisUTxOKey -> Crypto.Seed -> SigningKey GenesisUTxOKey
    deterministicSigningKey AsGenesisUTxOKey seed =
        GenesisUTxOSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisUTxOKey -> Word
    deterministicSigningKeySeedSize AsGenesisUTxOKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisUTxOKey -> VerificationKey GenesisUTxOKey
    getVerificationKey (GenesisUTxOSigningKey sk) =
        GenesisUTxOVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisUTxOKey -> Hash GenesisUTxOKey
    verificationKeyHash (GenesisUTxOVerificationKey vkey) =
        GenesisUTxOKeyHash (Shelley.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey GenesisUTxOKey) where
    serialiseToRawBytes (GenesisUTxOVerificationKey (Shelley.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsGenesisUTxOKey) bs =
      GenesisUTxOVerificationKey . Shelley.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey GenesisUTxOKey) where
    serialiseToRawBytes (GenesisUTxOSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsGenesisUTxOKey) bs =
      GenesisUTxOSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash GenesisUTxOKey =
    GenesisUTxOKeyHash (Shelley.KeyHash Shelley.Payment StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash GenesisUTxOKey) where
    serialiseToRawBytes (GenesisUTxOKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisUTxOKey) bs =
      GenesisUTxOKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisUTxOKey) where
    textEnvelopeType _ = "GenesisUTxOVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey GenesisUTxOKey) where
    textEnvelopeType _ = "GenesisUTxOSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy
    -- TODO: use a different type from the stake pool key, since some operations
    -- need a genesis key specifically

instance CastVerificationKeyRole GenesisUTxOKey PaymentKey where
    castVerificationKey (GenesisUTxOVerificationKey (Shelley.VKey vkey)) =
      PaymentVerificationKey (Shelley.VKey vkey)

instance CastSigningKeyRole GenesisUTxOKey PaymentKey where
    castSigningKey (GenesisUTxOSigningKey skey) =
      PaymentSigningKey skey

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
--
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
    --TODO: should handle Byron UTxO case too.
    fromShelleyTxIn (Shelley.initialFundsPseudoTxIn addr)
  where
    addr = Shelley.Addr
             (toShelleyNetwork nw)
             (Shelley.KeyHashObj kh)
             Shelley.StakeRefNull

    fromShelleyTxIn  :: Shelley.TxIn StandardShelley -> TxIn
    fromShelleyTxIn (Shelley.TxIn txid txix) =
        TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))

    fromShelleyTxId :: Shelley.TxId StandardShelley -> TxId
    fromShelleyTxId (Shelley.TxId h) =
        TxId (Crypto.castHash h)


--
-- stake pool keys
--

data StakePoolKey

instance HasTypeProxy StakePoolKey where
    data AsType StakePoolKey = AsStakePoolKey
    proxyToAsType _ = AsStakePoolKey

instance Key StakePoolKey where

    newtype VerificationKey StakePoolKey =
        StakePoolVerificationKey (Shelley.VKey Shelley.StakePool StandardShelley)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey StakePoolKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey StakePoolKey =
        StakePoolSigningKey (Shelley.SignKeyDSIGN StandardShelley)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey StakePoolKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType StakePoolKey -> Crypto.Seed -> SigningKey StakePoolKey
    deterministicSigningKey AsStakePoolKey seed =
        StakePoolSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType StakePoolKey -> Word
    deterministicSigningKeySeedSize AsStakePoolKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey StakePoolKey -> VerificationKey StakePoolKey
    getVerificationKey (StakePoolSigningKey sk) =
        StakePoolVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey StakePoolKey -> Hash StakePoolKey
    verificationKeyHash (StakePoolVerificationKey vkey) =
        StakePoolKeyHash (Shelley.hashKey vkey)

instance SerialiseAsRawBytes (VerificationKey StakePoolKey) where
    serialiseToRawBytes (StakePoolVerificationKey (Shelley.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsStakePoolKey) bs =
      StakePoolVerificationKey . Shelley.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey StakePoolKey) where
    serialiseToRawBytes (StakePoolSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsStakePoolKey) bs =
      StakePoolSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs

instance SerialiseAsBech32 (VerificationKey StakePoolKey) where
    bech32PrefixFor         _ =  "pool_vk"
    bech32PrefixesPermitted _ = ["pool_vk"]

instance SerialiseAsBech32 (SigningKey StakePoolKey) where
    bech32PrefixFor         _ =  "pool_sk"
    bech32PrefixesPermitted _ = ["pool_sk"]

newtype instance Hash StakePoolKey =
    StakePoolKeyHash (Shelley.KeyHash Shelley.StakePool StandardShelley)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash StakePoolKey) where
    serialiseToRawBytes (StakePoolKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsStakePoolKey) bs =
      StakePoolKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance SerialiseAsBech32 (Hash StakePoolKey) where
    bech32PrefixFor         _ =  "pool"
    bech32PrefixesPermitted _ = ["pool"]

instance ToJSON (Hash StakePoolKey) where
    toJSON = toJSON . serialiseToBech32

instance HasTextEnvelope (VerificationKey StakePoolKey) where
    textEnvelopeType _ = "StakePoolVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey StakePoolKey) where
    textEnvelopeType _ = "StakePoolSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN StandardCrypto)
        proxy = Proxy


--
-- KES keys
--

data KesKey

instance HasTypeProxy KesKey where
    data AsType KesKey = AsKesKey
    proxyToAsType _ = AsKesKey

instance Key KesKey where

    newtype VerificationKey KesKey =
        KesVerificationKey (Shelley.VerKeyKES StandardShelley)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey KesKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey KesKey =
        KesSigningKey (Shelley.SignKeyKES StandardShelley)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey KesKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType KesKey -> Crypto.Seed -> SigningKey KesKey
    deterministicSigningKey AsKesKey seed =
        KesSigningKey (Crypto.genKeyKES seed)

    deterministicSigningKeySeedSize :: AsType KesKey -> Word
    deterministicSigningKeySeedSize AsKesKey =
        Crypto.seedSizeKES proxy
      where
        proxy :: Proxy (Shelley.KES StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey KesKey -> VerificationKey KesKey
    getVerificationKey (KesSigningKey sk) =
        KesVerificationKey (Crypto.deriveVerKeyKES sk)

    verificationKeyHash :: VerificationKey KesKey -> Hash KesKey
    verificationKeyHash (KesVerificationKey vkey) =
        KesKeyHash (Crypto.hashVerKeyKES vkey)


instance SerialiseAsRawBytes (VerificationKey KesKey) where
    serialiseToRawBytes (KesVerificationKey vk) =
      Crypto.rawSerialiseVerKeyKES vk

    deserialiseFromRawBytes (AsVerificationKey AsKesKey) bs =
      KesVerificationKey <$>
        Crypto.rawDeserialiseVerKeyKES bs

instance SerialiseAsRawBytes (SigningKey KesKey) where
    serialiseToRawBytes (KesSigningKey sk) =
      Crypto.rawSerialiseSignKeyKES sk

    deserialiseFromRawBytes (AsSigningKey AsKesKey) bs =
      KesSigningKey <$> Crypto.rawDeserialiseSignKeyKES bs

instance SerialiseAsBech32 (VerificationKey KesKey) where
    bech32PrefixFor         _ =  "kes_vk"
    bech32PrefixesPermitted _ = ["kes_vk"]

instance SerialiseAsBech32 (SigningKey KesKey) where
    bech32PrefixFor         _ =  "kes_sk"
    bech32PrefixesPermitted _ = ["kes_sk"]


newtype instance Hash KesKey =
    KesKeyHash (Shelley.Hash StandardShelley
                             (Shelley.VerKeyKES StandardShelley))
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash KesKey) where
    serialiseToRawBytes (KesKeyHash vkh) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsKesKey) bs =
      KesKeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey KesKey) where
    textEnvelopeType _ = "KesVerificationKey_"
                      <> fromString (Crypto.algorithmNameKES proxy)
      where
        proxy :: Proxy (Shelley.KES StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey KesKey) where
    textEnvelopeType _ = "KesSigningKey_"
                      <> fromString (Crypto.algorithmNameKES proxy)
      where
        proxy :: Proxy (Shelley.KES StandardCrypto)
        proxy = Proxy


--
-- VRF keys
--

data VrfKey

instance HasTypeProxy VrfKey where
    data AsType VrfKey = AsVrfKey
    proxyToAsType _ = AsVrfKey

instance Key VrfKey where

    newtype VerificationKey VrfKey =
        VrfVerificationKey (Shelley.VerKeyVRF StandardShelley)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey VrfKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey VrfKey =
        VrfSigningKey (Shelley.SignKeyVRF StandardShelley)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey VrfKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType VrfKey -> Crypto.Seed -> SigningKey VrfKey
    deterministicSigningKey AsVrfKey seed =
        VrfSigningKey (Crypto.genKeyVRF seed)

    deterministicSigningKeySeedSize :: AsType VrfKey -> Word
    deterministicSigningKeySeedSize AsVrfKey =
        Crypto.seedSizeVRF proxy
      where
        proxy :: Proxy (Shelley.VRF StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey VrfKey -> VerificationKey VrfKey
    getVerificationKey (VrfSigningKey sk) =
        VrfVerificationKey (Crypto.deriveVerKeyVRF sk)

    verificationKeyHash :: VerificationKey VrfKey -> Hash VrfKey
    verificationKeyHash (VrfVerificationKey vkey) =
        VrfKeyHash (Shelley.hashVerKeyVRF vkey)

instance SerialiseAsRawBytes (VerificationKey VrfKey) where
    serialiseToRawBytes (VrfVerificationKey vk) =
      Crypto.rawSerialiseVerKeyVRF vk

    deserialiseFromRawBytes (AsVerificationKey AsVrfKey) bs =
      VrfVerificationKey <$> Crypto.rawDeserialiseVerKeyVRF bs

instance SerialiseAsRawBytes (SigningKey VrfKey) where
    serialiseToRawBytes (VrfSigningKey sk) =
      Crypto.rawSerialiseSignKeyVRF sk

    deserialiseFromRawBytes (AsSigningKey AsVrfKey) bs =
      VrfSigningKey <$> Crypto.rawDeserialiseSignKeyVRF bs

instance SerialiseAsBech32 (VerificationKey VrfKey) where
    bech32PrefixFor         _ =  "vrf_vk"
    bech32PrefixesPermitted _ = ["vrf_vk"]

instance SerialiseAsBech32 (SigningKey VrfKey) where
    bech32PrefixFor         _ =  "vrf_sk"
    bech32PrefixesPermitted _ = ["vrf_sk"]

newtype instance Hash VrfKey =
    VrfKeyHash (Shelley.Hash StandardShelley
                             (Shelley.VerKeyVRF StandardShelley))
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash VrfKey) where
    serialiseToRawBytes (VrfKeyHash vkh) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsVrfKey) bs =
      VrfKeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey VrfKey) where
    textEnvelopeType _ = "VrfVerificationKey_" <> fromString (Crypto.algorithmNameVRF proxy)
      where
        proxy :: Proxy (Shelley.VRF StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey VrfKey) where
    textEnvelopeType _ = "VrfSigningKey_" <> fromString (Crypto.algorithmNameVRF proxy)
      where
        proxy :: Proxy (Shelley.VRF StandardCrypto)
        proxy = Proxy

--
-- Utils
--

(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x  ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!. f = Left (f e)
Right x ?!. _ = Right x
