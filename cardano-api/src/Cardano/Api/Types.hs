{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Api.Types
  (
    -- * Common types across all eras.
    Address (..)
  , KeyPair (..)
  , Network (..)
  , VerificationKey (..)
  , TxSigned (..)
  , TxUnsigned (..)
  , TxWitness (..)

    -- * Era-specific type aliases
  , ByronVerificationKey
  , ByronSigningKey
  , ShelleyVerificationKey
  , ShelleySigningKey
  ) where

import           Cardano.Prelude

import           Data.Vector (Vector)

import           Cardano.Api.Orphans ()
import           Cardano.Config.Orphanage ()

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO   as Byron
import qualified Cardano.Crypto       as Byron

import qualified Ouroboros.Consensus.Shelley.Protocol.Crypto as Shelley
import qualified Shelley.Spec.Ledger.Keys                    as Shelley


type ByronVerificationKey = Byron.VerificationKey
type ByronSigningKey      = Byron.SigningKey

type ShelleyVerificationKey = Shelley.VKey Shelley.TPraosStandardCrypto
type ShelleySigningKey      = Shelley.SKey Shelley.TPraosStandardCrypto


-- The 'Address' data type in 'cardano-sl' is a design train wreck.
-- We need something that is compatible and discard as much of the insanity as possible.
data Address
  = AddressByron !Byron.Address
  | AddressShelley
  deriving (Eq, Generic , NFData, Show)  -- Byron.Address' needs NFData
  deriving NoUnexpectedThunks via UseIsNormalForm Address

-- | The combination of a verification key and a signing key.
--
-- Verification keys are also commonly known as \"public keys\".
--
-- Signing keys are also commonly known as \"private keys\" or \"secret keys\".
--
data KeyPair
  -- The Byron key pair use newtype wrappers around 'XPriv'/'Xpub' keys.
  -- An 'XPub' is 32 bytes of public key and 32 bytes of Chaincode which is used in the
  -- Byron address derivation scheme.
  = KeyPairByron !ByronVerificationKey !ByronSigningKey
  | KeyPairShelley !ShelleyVerificationKey !ShelleySigningKey
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- | A verification key for use in addresses (payment and stake).
--
-- Verification keys are also commonly known as \"public keys\".
--
data VerificationKey
  = VerificationKeyByron !ByronVerificationKey
  | VerificationKeyShelley !ShelleyVerificationKey
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- The cardano-sl codebase (and cardano-ledger) has something a little like
-- this (actually isomorphic with Maybe):
data Network
  = Mainnet
  | Testnet !Byron.ProtocolMagicId
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

data TxSigned
  = TxSignedByron !Byron.Tx !ByteString !(Byron.Hash Byron.Tx) !(Vector Byron.TxInWitness)
  | TxSignedShelley
  deriving (Eq, Generic, NFData, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxSigned

data TxUnsigned
  = TxUnsignedByron !Byron.Tx !ByteString !(Byron.Hash Byron.Tx)
  | TxUnsignedShelley
  deriving (Eq, Generic, NFData, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxUnsigned

data TxWitness
  = TxWitByron !Byron.TxInWitness
  | TxWitShelley
  deriving (Generic, NFData)
  deriving NoUnexpectedThunks via UseIsNormalForm TxWitness
