{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Api.Types
  ( Address (..)
  , KeyPair (..)
  , Network (..)
  , PublicKey (..)
  , ShelleyKeyDiscriminator (..)
  , ShelleyVerificationKey (..)
  , TxSigned (..)
  , TxUnsigned (..)
  , TxWitness (..)
  ) where

import           Cardano.Prelude

import           Cardano.Api.Orphans ()

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import           Cardano.Config.Orphanage ()
import qualified Cardano.Crypto as Crypto

import           Data.Vector (Vector)

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import           Shelley.Spec.Ledger.Keys (KeyDiscriminator, SKey, VKey, VKeyGenesis)

-- The 'Address' data type in 'cardano-sl' is a design train wreck.
-- We need something that is compatible and discard as much of the insanity as possible.
data Address
  = AddressByron !Byron.Address
  | AddressShelley
  deriving (Eq, Generic, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm Address

data KeyPair
  -- The Byron key pair use newtype wrappers around 'XPriv'/'Xpub' keys.
  -- An 'XPub' is 32 bytes of public key and 32 bytes of Chaincode which is used in the
  -- Byron address derivation scheme.
  = KeyPairByron !Crypto.VerificationKey !Crypto.SigningKey
  | KeyPairShelley !ShelleyVerificationKey !(SKey TPraosStandardCrypto)
  deriving (Generic, Show)
  deriving anyclass NoUnexpectedThunks

newtype ShelleyKeyDiscriminator = ShelleyKeyDiscriminator KeyDiscriminator
  deriving (Generic, Show)
  deriving newtype NoUnexpectedThunks

data ShelleyVerificationKey
  = GenesisShelleyVerificationKey !(VKeyGenesis TPraosStandardCrypto)
  | RegularShelleyVerificationKey !(VKey TPraosStandardCrypto)
  deriving (Generic, Show)
  deriving anyclass NoUnexpectedThunks

data PublicKey
  = PubKeyByron !Network !Crypto.VerificationKey
  | PubKeyShelley !Network !ShelleyVerificationKey
  deriving (Generic, Show)
  deriving anyclass NoUnexpectedThunks

-- The cardano-sl codebase (and cardano-ledger) has something a little like
-- this (actually isomorphic with Maybe):
data Network
  = Mainnet
  | Testnet !Crypto.ProtocolMagicId
  deriving (Eq, Generic, Show)
  deriving anyclass NoUnexpectedThunks

data TxSigned
  = TxSignedByron !Byron.Tx !ByteString !(Crypto.Hash Byron.Tx) !(Vector Byron.TxInWitness)
  | TxSignedShelley
  deriving (Eq, Generic, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxSigned

data TxUnsigned
  = TxUnsignedByron !Byron.Tx !ByteString !(Crypto.Hash Byron.Tx)
  | TxUnsignedShelley
  deriving (Eq, Generic, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxUnsigned

data TxWitness
  = TxWitByron !Byron.TxInWitness
  | TxWitShelley
  deriving (Generic)
  deriving NoUnexpectedThunks via UseIsNormalForm TxWitness
