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

import           Shelley.Spec.Ledger.Keys (SKey, VKey, VKeyGenesis)

-- The 'Address' data type in 'cardano-sl' is a design train wreck.
-- We need something that is compatible and discard as much of the insanity as possible.
data Address
  = AddressByron !Byron.Address
  | AddressShelley
  deriving (Eq, Generic , NFData, Show)  -- Byron.Address' needs NFData
  deriving NoUnexpectedThunks via UseIsNormalForm Address

data KeyPair
  -- The Byron key pair use newtype wrappers around 'XPriv'/'Xpub' keys.
  -- An 'XPub' is 32 bytes of public key and 32 bytes of Chaincode which is used in the
  -- Byron address derivation scheme.
  = KeyPairByron !Crypto.VerificationKey !Crypto.SigningKey
  | KeyPairShelley !ShelleyVerificationKey !(SKey TPraosStandardCrypto)
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- | A means of discriminating between different kinds of Shelley keys.
data ShelleyKeyDiscriminator
  = GenesisShelleyKey
  | RegularShelleyKey
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

data ShelleyVerificationKey
  = GenesisShelleyVerificationKey !(VKeyGenesis TPraosStandardCrypto)
  | RegularShelleyVerificationKey !(VKey TPraosStandardCrypto)
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

data PublicKey
  = PubKeyByron !Crypto.VerificationKey
  | PubKeyShelley !ShelleyVerificationKey
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- The cardano-sl codebase (and cardano-ledger) has something a little like
-- this (actually isomorphic with Maybe):
data Network
  = Mainnet
  | Testnet !Crypto.ProtocolMagicId
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

data TxSigned
  = TxSignedByron !Byron.Tx !ByteString !(Crypto.Hash Byron.Tx) !(Vector Byron.TxInWitness)
  | TxSignedShelley
  deriving (Eq, Generic, NFData, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxSigned

data TxUnsigned
  = TxUnsignedByron !Byron.Tx !ByteString !(Crypto.Hash Byron.Tx)
  | TxUnsignedShelley
  deriving (Eq, Generic, NFData, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxUnsigned

data TxWitness
  = TxWitByron !Byron.TxInWitness
  | TxWitShelley
  deriving (Generic, NFData)
  deriving NoUnexpectedThunks via UseIsNormalForm TxWitness
