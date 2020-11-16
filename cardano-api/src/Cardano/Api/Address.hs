{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Cardano addresses: payment and stake addresses.
--
module Cardano.Api.Address (
    -- * Payment addresses
    -- | Constructing and inspecting normal payment addresses
    Address(..),
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

    -- * Internal conversion functions
    toShelleyAddr,
    toShelleyStakeAddr,
    toShelleyStakeCredential,

    -- * Serialising addresses
    SerialiseAddress(..),

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Base58 as Base58

import           Control.Applicative

import qualified Cardano.Binary as CBOR

import qualified Cardano.Chain.Common as Byron

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley

import           Cardano.Api.Eras
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Key
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysPraos
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.Script
import           Cardano.Api.SerialiseBech32
import           Cardano.Api.SerialiseRaw


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
-- Payment addresses
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

deriving instance Eq (Address era)
deriving instance Ord (Address era)
deriving instance Show (Address era)


instance HasTypeProxy (Address Byron) where
    data AsType (Address Byron) = AsByronAddress
    proxyToAsType _ = AsByronAddress

instance HasTypeProxy (Address Shelley) where
    data AsType (Address Shelley) = AsShelleyAddress
    proxyToAsType _ = AsShelleyAddress


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

instance SerialiseAsBech32 (Address Shelley) where
    bech32PrefixFor (ShelleyAddress Shelley.Mainnet _ _) = "addr"
    bech32PrefixFor (ShelleyAddress Shelley.Testnet _ _) = "addr_test"
    bech32PrefixFor (ByronAddress _)                     = "addr"

    bech32PrefixesPermitted AsShelleyAddress = ["addr", "addr_test"]


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


-- ----------------------------------------------------------------------------
-- Stake addresses
--

data StakeAddress where

     StakeAddress
       :: Shelley.Network
       -> Shelley.StakeCredential StandardShelley
       -> StakeAddress
  deriving (Eq, Ord, Show)

data PaymentCredential
       = PaymentCredentialByKey    (Hash PaymentKey)
       | PaymentCredentialByScript  ScriptHash
  deriving (Eq, Show)

data StakeCredential
       = StakeCredentialByKey    (Hash StakeKey)
       | StakeCredentialByScript  ScriptHash
  deriving (Eq, Show)

data StakeAddressReference
       = StakeAddressByValue   StakeCredential
       | StakeAddressByPointer StakeAddressPointer
       | NoStakeAddress
  deriving (Eq, Show)

--TODO: wrap this type properly and export it
type StakeAddressPointer = Shelley.Ptr


instance HasTypeProxy StakeAddress where
    data AsType StakeAddress = AsStakeAddress
    proxyToAsType _ = AsStakeAddress


instance SerialiseAsRawBytes StakeAddress where
    serialiseToRawBytes (StakeAddress nw sc) =
        Shelley.serialiseRewardAcnt (Shelley.RewardAcnt nw sc)

    deserialiseFromRawBytes AsStakeAddress bs =
        case Shelley.deserialiseRewardAcnt bs of
          Nothing -> Nothing
          Just (Shelley.RewardAcnt nw sc) -> Just (StakeAddress nw sc)


instance SerialiseAsBech32 StakeAddress where
    bech32PrefixFor (StakeAddress Shelley.Mainnet _) = "stake"
    bech32PrefixFor (StakeAddress Shelley.Testnet _) = "stake_test"

    bech32PrefixesPermitted AsStakeAddress = ["stake", "stake_test"]


instance SerialiseAddress StakeAddress where
    serialiseAddress addr@StakeAddress{} =
      serialiseToBech32 addr

    deserialiseAddress AsStakeAddress t =
      either (const Nothing) Just $
      deserialiseFromBech32 AsStakeAddress t


makeStakeAddress :: NetworkId
                 -> StakeCredential
                 -> StakeAddress
makeStakeAddress nw sc =
    StakeAddress
      (toShelleyNetwork nw)
      (toShelleyStakeCredential sc)


-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

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
