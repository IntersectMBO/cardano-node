{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}

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

    -- * Stake addresses
    -- | Constructing and inspecting stake addresses
    StakeAddress(..),
    StakeCredential(..),
    makeStakeAddress,
    StakeKey,

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody(..),
    TxId,
    getTxId,
    TxIn(..),
    TxOut(..),
    TxIx,
    Lovelace,
    makeByronTransaction,
    makeShelleyTransaction,
    SlotNo,
    TxOptional(..),

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
    byronKeyWitness,
    shelleyKeyWitness,
    shelleyScriptWitness,

    -- * Fee calculation

    -- * Registering stake address and delegating
    -- | Certificates that are embedded in transactions for registering and
    -- unregistering stake address, and for setting the stake pool delegation
    -- choice for a stake address.

    -- * Registering stake pools
    -- | Certificates that are embedded in transactions for registering and
    -- retiring stake pools. This incldes updating the stake pool parameters.

    -- * Scripts
    -- | Both 'PaymentCredential's and 'StakeCredential's can use scripts.
    -- Shelley supports multi-signatures via scripts.

    -- ** Script addresses
    -- | Making addresses from scripts.

    -- ** Multi-sig scripts
    -- | Making multi-signature scripts.

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
    HasTextEnvelope,
    TextEnvelope,
    TextEnvelopeType,
    TextEnvelopeDescr,
    TextEnvelopeError,
    serialiseToTextEnvelope,
    deserialiseFromTextEnvelope,
    readFileTextEnvelope,
    writeFileTextEnvelope,
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
    -- ** Protocol parameters
    -- ** Submitting transactions

    -- * Node operation
    -- | Support for the steps needed to operate a node, including the
    -- operator's offline keys, operational KES and VRF keys, and operational
    -- certificates.

    -- ** Stake pool operator's keys
    StakePoolKey,

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
    GenesisDelegateKey,
    GenesisUTxOKey,

    -- * Special transactions
    -- | There are various additional things that can be embedded in a
    -- transaction for special operations.

  ) where


import Prelude

import           Data.Proxy (Proxy(..))
import           Data.Kind (Constraint)
import           Data.Maybe
import           Data.Bifunctor (first)
import           Data.List as List
--import           Data.Either
import           Data.String (IsString(fromString))
import qualified Data.Text as Text (unpack)
import           Numeric.Natural

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16

--import Control.Monad
--import Control.Monad.IO.Class
--import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Exception (Exception(..), IOException, throwIO)

import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON(..), FromJSON(..))


--
-- Common types, consensus, network
--
import qualified Cardano.Binary as CBOR
import           Cardano.Binary (ToCBOR(toCBOR), FromCBOR(fromCBOR))
import qualified Cardano.Prelude as CBOR (cborError)
import           Shelley.Spec.Ledger.Serialization (CBORGroup(..))
import           Cardano.Slotting.Slot (SlotNo)
import           Ouroboros.Network.Magic (NetworkMagic(..))

--
-- Crypto API used by consensus and Shelley (and should be used by Byron)
--
import qualified Cardano.Crypto.Seed        as Crypto
import qualified Cardano.Crypto.Hash.Class  as Crypto
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.KES.Class   as Crypto
import qualified Cardano.Crypto.VRF.Class   as Crypto

--
-- Byron imports
--
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO   as Byron

--
-- Shelley imports
--
import qualified Ouroboros.Consensus.Shelley.Protocol.Crypto as Shelley
import qualified Shelley.Spec.Ledger.Address                 as Shelley
import qualified Shelley.Spec.Ledger.Credential              as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes               as Shelley
--import qualified Shelley.Spec.Ledger.Coin                    as Shelley
--import qualified Shelley.Spec.Ledger.Delegation.Certificates as Shelley
import qualified Shelley.Spec.Ledger.Keys                    as Shelley
import qualified Shelley.Spec.Ledger.PParams                 as Shelley
import qualified Shelley.Spec.Ledger.OCert                   as Shelley
import qualified Shelley.Spec.Ledger.Scripts                 as Shelley
import qualified Shelley.Spec.Ledger.TxData                  as Shelley
import qualified Shelley.Spec.Ledger.Tx                      as Shelley
import qualified Shelley.Spec.Ledger.UTxO                    as Shelley

-- TODO: replace the above with
--import qualified Cardano.Api.Byron   as Byron
--import qualified Cardano.Api.Shelley as Shelley

--
-- Other config and common types
--
import qualified Cardano.Config.TextView as TextView


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
       Show (SigningKey keyrole),
       SerialiseAsRawBytes (Hash keyrole),
       HasTextEnvelope (VerificationKey keyrole),
       HasTextEnvelope (SigningKey keyrole))
    => Key keyrole where

    -- | The type of cryptographic verification key, for each key role.
    data VerificationKey keyrole :: *

    -- | The type of cryptographic signing key, for each key role.
    data SigningKey keyrole :: *

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
class CastKeyRole keyroleA keyroleB where

    -- | Change the role of a 'VerificationKey', if the representation permits.
    castVerificationKey :: VerificationKey keyroleA -> VerificationKey keyroleB

    -- | Change the role of a 'SigningKey', if the representation permits.
    castSigningKey :: SigningKey keyroleA -> SigningKey keyroleB


data family Hash keyrole :: *

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
       -> Shelley.PaymentCredential ShelleyCrypto
       -> Shelley.StakeReference    ShelleyCrypto
       -> Address Shelley

data StakeAddress where

     StakeAddress
       :: Shelley.Network
       -> Shelley.StakeCredential ShelleyCrypto
       -> StakeAddress

data NetworkId
       = Mainnet
       | Testnet !NetworkMagic

data PaymentCredential
       = PaymentCredentialByKey    (Hash PaymentKey)
       | PaymentCredentialByScript (Hash Script)

data StakeCredential
       = StakeCredentialByKey    (Hash StakeKey)
       | StakeCredentialByScript (Hash Script)

data StakeAddressReference
       = StakeAddressByValue   StakeCredential
       | StakeAddressByPointer StakeAddressPointer
       | NoStakeAddress

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
        serialiseRewardAcnt (Shelley.RewardAcnt nw sc)
      where
        serialiseRewardAcnt =
          error "TODO: Use Shelley.serialiseRewardAcnt when it is available"

    deserialiseFromRawBytes AsStakeAddress bs =
        case deserialiseRewardAcnt bs of
          Nothing -> Nothing
          Just (Shelley.RewardAcnt nw sc) -> Just (StakeAddress nw sc)
      where
        deserialiseRewardAcnt =
          error "TODO: Use Shelley.deserialiseRewardAcnt when it is available"


makeByronAddress :: VerificationKey ByronKey
                 -> NetworkId
                 -> Address era
makeByronAddress (ByronVerificationKey vk) nid =
    ByronAddress $
      Byron.makeVerKeyAddress
        (toByronNetworkMagic nid)
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


toByronNetworkMagic :: NetworkId -> Byron.NetworkMagic
toByronNetworkMagic Mainnet                     = Byron.NetworkMainOrStage
toByronNetworkMagic (Testnet (NetworkMagic nm)) = Byron.NetworkTestnet nm

toShelleyNetwork :: NetworkId -> Shelley.Network
toShelleyNetwork  Mainnet    = Shelley.Mainnet
toShelleyNetwork (Testnet _) = Shelley.Testnet


toShelleyPaymentCredential :: PaymentCredential
                           -> Shelley.PaymentCredential ShelleyCrypto
toShelleyPaymentCredential (PaymentCredentialByKey (PaymentKeyHash kh)) =
    Shelley.KeyHashObj kh
toShelleyPaymentCredential (PaymentCredentialByScript (ScriptHash sh)) =
    Shelley.ScriptHashObj sh

toShelleyStakeCredential :: StakeCredential
                         -> Shelley.StakeCredential ShelleyCrypto
toShelleyStakeCredential (StakeCredentialByKey (StakeKeyHash kh)) =
    Shelley.KeyHashObj kh
toShelleyStakeCredential (StakeCredentialByScript (ScriptHash kh)) =
    Shelley.ScriptHashObj kh

toShelleyStakeReference :: StakeAddressReference
                        -> Shelley.StakeReference ShelleyCrypto
toShelleyStakeReference (StakeAddressByValue stakecred) =
    Shelley.StakeRefBase (toShelleyStakeCredential stakecred)
toShelleyStakeReference (StakeAddressByPointer ptr) =
    Shelley.StakeRefPtr ptr
toShelleyStakeReference  NoStakeAddress =
    Shelley.StakeRefNull


-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Shelley.Hash ShelleyCrypto ())
               -- We use the Shelley representation and convert the Byron one

instance HasTypeProxy TxId where
    data AsType TxId = AsTxId
    proxyToAsType _ = AsTxId

instance SerialiseAsRawBytes TxId where
    serialiseToRawBytes (TxId h) = Crypto.getHash h
    deserialiseFromRawBytes AsTxId bs = TxId <$> Crypto.hashFromBytes bs

-- | Calculate the transaction identifier for a 'TxBody'.
--
getTxId :: TxBody era -> TxId
getTxId (ByronTxBody tx) =
    TxId
  . Crypto.UnsafeHash
  . Byron.hashToBytes
  . Byron.serializeCborHash
  $ tx

getTxId (ShelleyTxBody tx) =
    TxId
  . Crypto.castHash
  . (\(Shelley.TxId txhash) -> txhash)
  . Shelley.txid
  $ tx


-- ----------------------------------------------------------------------------
-- Transaction constituent types
--

data TxIn = TxIn TxId TxIx
--TODO  deriving (Show)

newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

data TxOut era = TxOut (Address era) Lovelace
--TODO  deriving (Show)

newtype Lovelace = Lovelace Integer
  deriving (Eq, Ord, Enum, Show)


-- ----------------------------------------------------------------------------
-- Unsigned transactions
--

data TxBody era where

     ByronTxBody
       :: Byron.Tx
       -> TxBody Byron

     ShelleyTxBody
       :: Shelley.TxBody ShelleyCrypto
       -> TxBody Shelley


instance HasTypeProxy (TxBody Byron) where
    data AsType (TxBody Byron) = AsByronTxBody
    proxyToAsType _ = AsByronTxBody

instance HasTypeProxy (TxBody Shelley) where
    data AsType (TxBody Shelley) = AsShelleyTxBody
    proxyToAsType _ = AsShelleyTxBody


instance SerialiseAsCBOR (TxBody Byron) where
    serialiseToCBOR (ByronTxBody txbody) = CBOR.serialize' txbody

    deserialiseFromCBOR AsByronTxBody bs =
      ByronTxBody <$> CBOR.decodeFull' bs

instance SerialiseAsCBOR (TxBody Shelley) where
    serialiseToCBOR (ShelleyTxBody txbody) =
      CBOR.serialize' txbody

    deserialiseFromCBOR AsShelleyTxBody bs =
      ShelleyTxBody <$>
        CBOR.decodeAnnotator "Shelley TxBody" fromCBOR (LBS.fromStrict bs)


instance HasTextEnvelope (TxBody Byron) where
    textEnvelopeType _ = "TxUnsignedByron"

instance HasTextEnvelope (TxBody Shelley) where
    textEnvelopeType _ = "TxUnsignedShelley"


makeByronTransaction :: [TxIn] -> [TxOut Byron] -> TxBody Byron
makeByronTransaction = undefined

data TxOptional =
     TxOptional {
       txMetadata        :: Maybe TxMetadata,
       txWithdrawals     :: [(StakeAddress, Lovelace)],
       txCertificates    :: [Certificate],
       txProtocolUpdates :: Maybe ProtocolUpdates
     }

data TxMetadata
data Certificate
type ProtocolUpdates = Shelley.ProposedPPUpdates ShelleyCrypto

makeShelleyTransaction :: TxOptional
                       -> SlotNo
                       -> Lovelace
                       -> [TxIn]
                       -> [TxOut anyera]
                       -> TxBody Shelley
makeShelleyTransaction = undefined


-- ----------------------------------------------------------------------------
-- Signed transactions
--

data Tx era where

     ByronTx
       :: Byron.ATxAux ByteString
       -> Tx Byron

     ShelleyTx
       :: Shelley.Tx ShelleyCrypto
       -> Tx Shelley


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

     ShelleyKeyWitness
       :: Shelley.WitVKey ShelleyCrypto Shelley.Witness
       -> Witness Shelley

     ShelleyScriptWitness
       :: Shelley.MultiSig ShelleyCrypto
       -> Witness Shelley


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
        encodeShelleyWitness (ShelleyScriptWitness wit) =
            CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> toCBOR wit

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
            1 -> fmap (pure . ShelleyScriptWitness) fromCBOR
            _ -> CBOR.cborError $ CBOR.DecoderErrorUnknownTag
                                    "Shelley Witness" (fromIntegral t)

instance HasTextEnvelope (Witness Byron) where
    textEnvelopeType _ = "TxWinessByron"

instance HasTextEnvelope (Witness Shelley) where
    textEnvelopeType _ = "TxWinessShelley"


getTxBody :: Tx era -> TxBody era
getTxBody (ByronTx _tx) = undefined
getTxBody (ShelleyTx _tx) = undefined


getTxWitnesses :: Tx era -> [Witness era]
getTxWitnesses (ByronTx _tx) = undefined
getTxWitnesses (ShelleyTx _tx) = undefined


makeSignedTransaction :: [Witness era]
                      -> TxBody era
                      -> Tx era
makeSignedTransaction = undefined


byronKeyWitness :: NetworkId -> SigningKey ByronKey -> TxBody era -> Witness Byron
byronKeyWitness = undefined


shelleyKeyWitness :: SigningKey keyrole -> TxBody era -> Witness Shelley
shelleyKeyWitness = undefined
-- this may need some class constraint on the keyrole, we can sign with:
--
--  ByronKey     -- for utxo inputs at byron addresses
--  PaymentKey   -- for utxo inputs, including multi-sig
--  StakeKey     -- for stake addr withdrawals and retiring and pool owners
--  StakePoolKey -- for stake pool ops
--  GenesisDelegateKey -- for update proposals, MIR etc


shelleyScriptWitness :: Script -> TxId -> Witness Shelley
shelleyScriptWitness = undefined


-- order of signing keys must match txins
signByronTransaction :: NetworkId
                     -> TxBody Byron
                     -> [SigningKey ByronKey]
                     -> Tx Byron
signByronTransaction = undefined

-- signing keys is a set
signShelleyTransaction :: TxBody Shelley
                       -> [SigningKey Shelley]
                       -> Tx Shelley
signShelleyTransaction = undefined


-- ----------------------------------------------------------------------------
-- Scripts
--

data Script

newtype instance Hash Script = ScriptHash (Shelley.ScriptHash ShelleyCrypto)

-- ----------------------------------------------------------------------------
-- Operational certificates
--

data OperationalCertificate =
     OperationalCertificate
       !(Shelley.OCert ShelleyCrypto)
       !(VerificationKey StakePoolKey)
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR

data OperationalCertificateIssueCounter =
     OperationalCertificateIssueCounter
       !Natural
       -- TODO: Commenting this out as we're temporarily supporting the old op
       -- cert issue counter format.
       -- !(VerificationKey StakePoolKey) -- For consistency checking
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance ToCBOR OperationalCertificate where
    toCBOR (OperationalCertificate ocert vkey) =
      toCBOR (CBORGroup ocert, vkey)

instance FromCBOR OperationalCertificate where
    fromCBOR = do
      (CBORGroup ocert, vkey) <- fromCBOR
      return (OperationalCertificate ocert vkey)

instance ToCBOR OperationalCertificateIssueCounter where
    -- TODO: Commenting this out as we're temporarily supporting the old op
    -- cert issue counter format.
    -- toCBOR (OperationalCertificateIssueCounter counter vkey) =
    --   toCBOR (counter, vkey)
    toCBOR (OperationalCertificateIssueCounter counter) =
      toCBOR counter

instance FromCBOR OperationalCertificateIssueCounter where
    -- TODO: Commenting this out as we're temporarily supporting the old op
    -- cert issue counter format.
    -- fromCBOR = do
    --   (counter, vkey) <- fromCBOR
    --   return (OperationalCertificateIssueCounter counter vkey)
    fromCBOR = OperationalCertificateIssueCounter <$> fromCBOR

instance HasTypeProxy OperationalCertificate where
    data AsType OperationalCertificate = AsOperationalCertificate
    proxyToAsType _ = AsOperationalCertificate

instance HasTypeProxy OperationalCertificateIssueCounter where
    data AsType OperationalCertificateIssueCounter = AsOperationalCertificateIssueCounter
    proxyToAsType _ = AsOperationalCertificateIssueCounter

instance HasTextEnvelope OperationalCertificate where
    textEnvelopeType _ = "Node operational certificate"

instance HasTextEnvelope OperationalCertificateIssueCounter where
    textEnvelopeType _ = "Node operational certificate issue counter"

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
                            -> SigningKey StakePoolKey
                            -> Shelley.KESPeriod
                            -> OperationalCertificateIssueCounter
                            -> Either OperationalCertIssueError
                                      (OperationalCertificate,
                                      OperationalCertificateIssueCounter)
issueOperationalCertificate (KesVerificationKey kesVKey)
                            (StakePoolSigningKey poolSKey)
                            kesPeriod
                            -- TODO: Commenting this out as we're temporarily supporting the old op
                            -- cert issue counter format.
                            -- (OperationalCertificateIssueCounter counter poolVKey)
                            (OperationalCertificateIssueCounter counter)
  -- TODO: Commenting this out as we're temporarily supporting the old op
  -- cert issue counter format.
  -- \| poolVKey /= poolVKey'
  -- = Left (OperationalCertKeyMismatch poolVKey poolVKey')
  --
  -- \| otherwise
  -- = Right (OperationalCertificate ocert poolVKey,
  --          OperationalCertificateIssueCounter (succ counter) poolVKey)
    = Right (OperationalCertificate ocert poolVKey',
             OperationalCertificateIssueCounter (succ counter))
  where
    poolVKey' = getVerificationKey (StakePoolSigningKey poolSKey)

    ocert     :: Shelley.OCert ShelleyCrypto
    ocert     = Shelley.OCert kesVKey counter kesPeriod signature

    signature :: Crypto.SignedDSIGN
                   (Shelley.DSIGN ShelleyCrypto)
                   (Shelley.VerKeyKES ShelleyCrypto,
                    Natural,
                    Shelley.KESPeriod)
    signature = Crypto.signedDSIGN ()
                  (kesVKey, counter, kesPeriod)
                  poolSKey


-- ----------------------------------------------------------------------------
-- CBOR and JSON Serialisation
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

newtype JsonDecodeError = JsonDecodeError String

serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

deserialiseFromJSON :: FromJSON a
                    => AsType a
                    -> ByteString
                    -> Either JsonDecodeError a
deserialiseFromJSON _proxy = either (Left . JsonDecodeError) Right
                           . Aeson.eitherDecodeStrict'

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


-- ----------------------------------------------------------------------------
-- TextEnvelope Serialisation
--

type TextEnvelope = TextView.TextView
type TextEnvelopeType = TextView.TextViewType
type TextEnvelopeDescr = TextView.TextViewTitle

class SerialiseAsCBOR a => HasTextEnvelope a where
    textEnvelopeType :: AsType a -> TextEnvelopeType

    textEnvelopeDefaultDescr :: AsType a -> TextEnvelopeDescr
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
    , TextView.tvTitle   = fromMaybe (textEnvelopeDefaultDescr ttoken) mbDescr
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
      deserialiseFromCBOR ttoken (TextView.tvRawCBOR te)

data FromSomeType (c :: * -> Constraint) b where
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
    content = TextView.renderTextView (serialiseToTextEnvelope mbDescr a)


readFileTextEnvelope :: HasTextEnvelope a
                     => AsType a
                     -> FilePath
                     -> IO (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope ttoken path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- TextView.parseTextView content
        deserialiseFromTextEnvelope ttoken te


readFileTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                          -> FilePath
                          -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf types path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- TextView.parseTextView content
        deserialiseFromTextEnvelopeAnyOf types te


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
-- Shelley type aliases
--

type ShelleyCrypto = Shelley.TPraosStandardCrypto


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
type family ShelleyKeyRole (keyrole :: *) :: Shelley.KeyRole

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
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey ByronKey =
           ByronSigningKey Byron.SigningKey
      deriving stock (Show)
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

newtype instance Hash ByronKey = ByronKeyHash Byron.KeyHash

instance SerialiseAsRawBytes (Hash ByronKey) where
    serialiseToRawBytes (ByronKeyHash (Byron.KeyHash vkh)) =
      Byron.abstractHashToBytes vkh

    deserialiseFromRawBytes (AsHash AsByronKey) bs =
      ByronKeyHash . Byron.KeyHash <$> Byron.abstractHashFromBytes bs

instance HasTextEnvelope (VerificationKey ByronKey) where
    textEnvelopeType _ = "PaymentVerificationKeyByron"

instance HasTextEnvelope (SigningKey ByronKey) where
    textEnvelopeType _ = "SigningKeyByron"
    -- TODO: fix these inconsistent names for the public testnet re-spin


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
        PaymentVerificationKey (Shelley.VKey Shelley.Payment ShelleyCrypto)
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey PaymentKey =
        PaymentSigningKey (Shelley.SignKeyDSIGN ShelleyCrypto)
      deriving stock (Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType PaymentKey -> Crypto.Seed -> SigningKey PaymentKey
    deterministicSigningKey AsPaymentKey seed =
        PaymentSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType PaymentKey -> Word
    deterministicSigningKeySeedSize AsPaymentKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN ShelleyCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey PaymentKey -> VerificationKey PaymentKey
    getVerificationKey (PaymentSigningKey sk) =
        PaymentVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey PaymentKey -> Hash PaymentKey
    verificationKeyHash (PaymentVerificationKey vkey) =
        PaymentKeyHash (Shelley.hashKey vkey)

newtype instance Hash PaymentKey =
    PaymentKeyHash (Shelley.KeyHash Shelley.Payment ShelleyCrypto)

instance SerialiseAsRawBytes (Hash PaymentKey) where
    serialiseToRawBytes (PaymentKeyHash (Shelley.KeyHash vkh)) =
      Crypto.getHash vkh

    deserialiseFromRawBytes (AsHash AsPaymentKey) bs =
      PaymentKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey PaymentKey) where
    textEnvelopeType _ = "PaymentVerificationKeyShelley"
    -- TODO: include the actual crypto algorithm name, to catch changes:
{-
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Shelley.DSIGN ShelleyCrypto)
        proxy = Proxy
-}

instance HasTextEnvelope (SigningKey PaymentKey) where
    textEnvelopeType _ = "SigningKeyShelley"
    -- TODO: include the actual crypto algorithm name, to catch changes
    -- TODO: fix these inconsistent names for the public testnet re-spin


--
-- Stake keys
--

data StakeKey

instance HasTypeProxy StakeKey where
    data AsType StakeKey = AsStakeKey
    proxyToAsType _ = AsStakeKey

instance Key StakeKey where

    newtype VerificationKey StakeKey =
        StakeVerificationKey (Shelley.VKey Shelley.Staking ShelleyCrypto)
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey StakeKey =
        StakeSigningKey (Shelley.SignKeyDSIGN ShelleyCrypto)
      deriving stock (Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType StakeKey -> Crypto.Seed -> SigningKey StakeKey
    deterministicSigningKey AsStakeKey seed =
        StakeSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType StakeKey -> Word
    deterministicSigningKeySeedSize AsStakeKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN ShelleyCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey StakeKey -> VerificationKey StakeKey
    getVerificationKey (StakeSigningKey sk) =
        StakeVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey StakeKey -> Hash StakeKey
    verificationKeyHash (StakeVerificationKey vkey) =
        StakeKeyHash (Shelley.hashKey vkey)

newtype instance Hash StakeKey =
    StakeKeyHash (Shelley.KeyHash Shelley.Staking ShelleyCrypto)

instance SerialiseAsRawBytes (Hash StakeKey) where
    serialiseToRawBytes (StakeKeyHash (Shelley.KeyHash vkh)) =
      Crypto.getHash vkh

    deserialiseFromRawBytes (AsHash AsStakeKey) bs =
      StakeKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey StakeKey) where
    textEnvelopeType _ = "StakingVerificationKeyShelley"
    -- TODO: include the actual crypto algorithm name, to catch changes

instance HasTextEnvelope (SigningKey StakeKey) where
    textEnvelopeType _ = "SigningKeyShelley"
    -- TODO: include the actual crypto algorithm name, to catch changes
    -- TODO: fix these inconsistent names for the public testnet re-spin


--
-- Genesis keys
--

data GenesisKey

instance HasTypeProxy GenesisKey where
    data AsType GenesisKey = AsGenesisKey
    proxyToAsType _ = AsGenesisKey

instance Key GenesisKey where

    newtype VerificationKey GenesisKey =
        GenesisVerificationKey (Shelley.VKey Shelley.Genesis ShelleyCrypto)
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisKey =
        GenesisSigningKey (Shelley.SignKeyDSIGN ShelleyCrypto)
      deriving stock (Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisKey -> Crypto.Seed -> SigningKey GenesisKey
    deterministicSigningKey AsGenesisKey seed =
        GenesisSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisKey -> Word
    deterministicSigningKeySeedSize AsGenesisKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN ShelleyCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisKey -> VerificationKey GenesisKey
    getVerificationKey (GenesisSigningKey sk) =
        GenesisVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisKey -> Hash GenesisKey
    verificationKeyHash (GenesisVerificationKey vkey) =
        GenesisKeyHash (Shelley.hashKey vkey)

newtype instance Hash GenesisKey =
    GenesisKeyHash (Shelley.KeyHash Shelley.Genesis ShelleyCrypto)

instance SerialiseAsRawBytes (Hash GenesisKey) where
    serialiseToRawBytes (GenesisKeyHash (Shelley.KeyHash vkh)) =
      Crypto.getHash vkh

    deserialiseFromRawBytes (AsHash AsGenesisKey) bs =
      GenesisKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisKey) where
    textEnvelopeType _ = "Genesis verification key"
    -- TODO: include the actual crypto algorithm name, to catch changes

instance HasTextEnvelope (SigningKey GenesisKey) where
    textEnvelopeType _ = "Genesis signing key"
    -- TODO: include the actual crypto algorithm name, to catch changes


--
-- Genesis delegate keys
--

data GenesisDelegateKey

instance HasTypeProxy GenesisDelegateKey where
    data AsType GenesisDelegateKey = AsGenesisDelegateKey
    proxyToAsType _ = AsGenesisDelegateKey


instance Key GenesisDelegateKey where

    newtype VerificationKey GenesisDelegateKey =
        GenesisDelegateVerificationKey (Shelley.VKey Shelley.GenesisDelegate ShelleyCrypto)
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisDelegateKey =
        GenesisDelegateSigningKey (Shelley.SignKeyDSIGN ShelleyCrypto)
      deriving stock (Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisDelegateKey -> Crypto.Seed -> SigningKey GenesisDelegateKey
    deterministicSigningKey AsGenesisDelegateKey seed =
        GenesisDelegateSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisDelegateKey -> Word
    deterministicSigningKeySeedSize AsGenesisDelegateKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN ShelleyCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisDelegateKey -> VerificationKey GenesisDelegateKey
    getVerificationKey (GenesisDelegateSigningKey sk) =
        GenesisDelegateVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisDelegateKey -> Hash GenesisDelegateKey
    verificationKeyHash (GenesisDelegateVerificationKey vkey) =
        GenesisDelegateKeyHash (Shelley.hashKey vkey)

newtype instance Hash GenesisDelegateKey =
    GenesisDelegateKeyHash (Shelley.KeyHash Shelley.GenesisDelegate ShelleyCrypto)

instance SerialiseAsRawBytes (Hash GenesisDelegateKey) where
    serialiseToRawBytes (GenesisDelegateKeyHash (Shelley.KeyHash vkh)) =
      Crypto.getHash vkh

    deserialiseFromRawBytes (AsHash AsGenesisDelegateKey) bs =
      GenesisDelegateKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisDelegateKey) where
    textEnvelopeType _ = "Node operator verification key"
    -- TODO: include the actual crypto algorithm name, to catch changes

instance HasTextEnvelope (SigningKey GenesisDelegateKey) where
    textEnvelopeType _ = "Node operator signing key"
    -- TODO: include the actual crypto algorithm name, to catch changes
    -- TODO: use a different type from the stake pool key, since some operations
    -- need a genesis key specifically

instance CastKeyRole GenesisDelegateKey StakePoolKey where
    castVerificationKey (GenesisDelegateVerificationKey (Shelley.VKey vkey)) =
      StakePoolVerificationKey (Shelley.VKey vkey)

    castSigningKey (GenesisDelegateSigningKey skey) =
      StakePoolSigningKey skey


--
-- Genesis UTxO keys
--

data GenesisUTxOKey

instance HasTypeProxy GenesisUTxOKey where
    data AsType GenesisUTxOKey = AsGenesisUTxOKey
    proxyToAsType _ = AsGenesisUTxOKey


instance Key GenesisUTxOKey where

    newtype VerificationKey GenesisUTxOKey =
        GenesisUTxOVerificationKey (Shelley.VKey Shelley.Payment ShelleyCrypto)
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisUTxOKey =
        GenesisUTxOSigningKey (Shelley.SignKeyDSIGN ShelleyCrypto)
      deriving stock (Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisUTxOKey -> Crypto.Seed -> SigningKey GenesisUTxOKey
    deterministicSigningKey AsGenesisUTxOKey seed =
        GenesisUTxOSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisUTxOKey -> Word
    deterministicSigningKeySeedSize AsGenesisUTxOKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN ShelleyCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisUTxOKey -> VerificationKey GenesisUTxOKey
    getVerificationKey (GenesisUTxOSigningKey sk) =
        GenesisUTxOVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisUTxOKey -> Hash GenesisUTxOKey
    verificationKeyHash (GenesisUTxOVerificationKey vkey) =
        GenesisUTxOKeyHash (Shelley.hashKey vkey)

newtype instance Hash GenesisUTxOKey =
    GenesisUTxOKeyHash (Shelley.KeyHash Shelley.Payment ShelleyCrypto)

instance SerialiseAsRawBytes (Hash GenesisUTxOKey) where
    serialiseToRawBytes (GenesisUTxOKeyHash (Shelley.KeyHash vkh)) =
      Crypto.getHash vkh

    deserialiseFromRawBytes (AsHash AsGenesisUTxOKey) bs =
      GenesisUTxOKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisUTxOKey) where
    textEnvelopeType _ = "Genesis UTxO verification key"
    -- TODO: include the actual crypto algorithm name, to catch changes

instance HasTextEnvelope (SigningKey GenesisUTxOKey) where
    textEnvelopeType _ = "Genesis UTxO signing key"
    -- TODO: include the actual crypto algorithm name, to catch changes
    -- TODO: use a different type from the stake pool key, since some operations
    -- need a genesis key specifically


--
-- stake pool keys
--

data StakePoolKey

instance HasTypeProxy StakePoolKey where
    data AsType StakePoolKey = AsStakePoolKey
    proxyToAsType _ = AsStakePoolKey

instance Key StakePoolKey where

    newtype VerificationKey StakePoolKey =
        StakePoolVerificationKey (Shelley.VKey Shelley.StakePool ShelleyCrypto)
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey StakePoolKey =
        StakePoolSigningKey (Shelley.SignKeyDSIGN ShelleyCrypto)
      deriving stock (Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType StakePoolKey -> Crypto.Seed -> SigningKey StakePoolKey
    deterministicSigningKey AsStakePoolKey seed =
        StakePoolSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType StakePoolKey -> Word
    deterministicSigningKeySeedSize AsStakePoolKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Shelley.DSIGN ShelleyCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey StakePoolKey -> VerificationKey StakePoolKey
    getVerificationKey (StakePoolSigningKey sk) =
        StakePoolVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey StakePoolKey -> Hash StakePoolKey
    verificationKeyHash (StakePoolVerificationKey vkey) =
        StakePoolKeyHash (Shelley.hashKey vkey)

newtype instance Hash StakePoolKey =
    StakePoolKeyHash (Shelley.KeyHash Shelley.StakePool ShelleyCrypto)

instance SerialiseAsRawBytes (Hash StakePoolKey) where
    serialiseToRawBytes (StakePoolKeyHash (Shelley.KeyHash vkh)) =
      Crypto.getHash vkh

    deserialiseFromRawBytes (AsHash AsStakePoolKey) bs =
      StakePoolKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey StakePoolKey) where
    textEnvelopeType _ = "Node operator verification key"
    -- TODO: include the actual crypto algorithm name, to catch changes

instance HasTextEnvelope (SigningKey StakePoolKey) where
    textEnvelopeType _ = "Node operator signing key"
    -- TODO: include the actual crypto algorithm name, to catch changes


--
-- KES keys
--

data KesKey

instance HasTypeProxy KesKey where
    data AsType KesKey = AsKesKey
    proxyToAsType _ = AsKesKey

instance Key KesKey where

    newtype VerificationKey KesKey =
        KesVerificationKey (Shelley.VerKeyKES ShelleyCrypto)
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey KesKey =
        KesSigningKey (Shelley.SignKeyKES ShelleyCrypto)
      deriving stock (Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType KesKey -> Crypto.Seed -> SigningKey KesKey
    deterministicSigningKey AsKesKey seed =
        KesSigningKey (Crypto.genKeyKES seed)

    deterministicSigningKeySeedSize :: AsType KesKey -> Word
    deterministicSigningKeySeedSize AsKesKey =
        Crypto.seedSizeKES proxy
      where
        proxy :: Proxy (Shelley.KES ShelleyCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey KesKey -> VerificationKey KesKey
    getVerificationKey (KesSigningKey sk) =
        KesVerificationKey (Crypto.deriveVerKeyKES sk)

    verificationKeyHash :: VerificationKey KesKey -> Hash KesKey
    verificationKeyHash (KesVerificationKey vkey) =
        KesKeyHash (Crypto.hashVerKeyKES vkey)

newtype instance Hash KesKey =
    KesKeyHash (Crypto.Hash (Shelley.HASH ShelleyCrypto)
                            (Shelley.VerKeyKES ShelleyCrypto))

instance SerialiseAsRawBytes (Hash KesKey) where
    serialiseToRawBytes (KesKeyHash vkh) =
      Crypto.getHash vkh

    deserialiseFromRawBytes (AsHash AsKesKey) bs =
      KesKeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey KesKey) where
    textEnvelopeType _ = "VKeyES TPraosStandardCrypto"
    -- TODO: include the actual crypto algorithm name, to catch changes

instance HasTextEnvelope (SigningKey KesKey) where
    textEnvelopeType _ = "SKeyES TPraosStandardCrypto"
    -- TODO: include the actual crypto algorithm name, to catch changes


--
-- VRF keys
--

data VrfKey

instance HasTypeProxy VrfKey where
    data AsType VrfKey = AsVrfKey
    proxyToAsType _ = AsVrfKey

instance Key VrfKey where

    newtype VerificationKey VrfKey =
        VrfVerificationKey (Shelley.VerKeyVRF ShelleyCrypto)
      deriving stock (Eq, Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey VrfKey =
        VrfSigningKey (Shelley.SignKeyVRF ShelleyCrypto)
      deriving stock (Show)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType VrfKey -> Crypto.Seed -> SigningKey VrfKey
    deterministicSigningKey AsVrfKey seed =
        VrfSigningKey (Crypto.genKeyVRF seed)

    deterministicSigningKeySeedSize :: AsType VrfKey -> Word
    deterministicSigningKeySeedSize AsVrfKey =
        Crypto.seedSizeVRF proxy
      where
        proxy :: Proxy (Shelley.VRF ShelleyCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey VrfKey -> VerificationKey VrfKey
    getVerificationKey (VrfSigningKey sk) =
        VrfVerificationKey (Crypto.deriveVerKeyVRF sk)

    verificationKeyHash :: VerificationKey VrfKey -> Hash VrfKey
    verificationKeyHash (VrfVerificationKey vkey) =
        VrfKeyHash (Crypto.hashVerKeyVRF vkey)

newtype instance Hash VrfKey =
    VrfKeyHash (Crypto.Hash (Shelley.HASH ShelleyCrypto)
                            (Shelley.VerKeyVRF ShelleyCrypto))

instance SerialiseAsRawBytes (Hash VrfKey) where
    serialiseToRawBytes (VrfKeyHash vkh) =
      Crypto.getHash vkh

    deserialiseFromRawBytes (AsHash AsVrfKey) bs =
      VrfKeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey VrfKey) where
    textEnvelopeType _ = "VerKeyVRF " <> fromString (backCompatAlgorithmNameVrf proxy)
      where
        proxy :: Proxy (Shelley.VRF ShelleyCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey VrfKey) where
    textEnvelopeType _ = "SignKeyVRF " <> fromString (backCompatAlgorithmNameVrf proxy)
      where
        proxy :: Proxy (Shelley.VRF ShelleyCrypto)
        proxy = Proxy

-- | Temporary solution for maintaining backward compatibility with the output
-- of 'Cardano.Config.Shelley.VRF.encodeVRFVerificationKey'.
backCompatAlgorithmNameVrf :: Proxy (Shelley.VRF ShelleyCrypto) -> String
backCompatAlgorithmNameVrf p =
  let algoName = Crypto.algorithmNameVRF p
  in if algoName == "simple" then "SimpleVRF" else algoName
