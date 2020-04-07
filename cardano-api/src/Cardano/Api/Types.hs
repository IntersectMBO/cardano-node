{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Api.Types
  ( Address (..)
  , KeyPair (..)
  , Network (..)
  , PublicKey (..)
  , Transaction (..)
  , TxSigned (..)
  , TxUnsigned (..)
  , TxWitness (..)
  ) where

import           Cardano.Prelude

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto

import           Data.Vector (Vector)


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
  | KeyPairShelley -- Exact crypto not ready yet.
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

data PublicKey
  = PubKeyByron !Network !Crypto.VerificationKey
  | PubKeyShelley -- Placeholder.
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- The cardano-sl codebase (and cardano-ledger) has something a little like
-- this (actually isomorphic with Maybe):
data Network
  = Mainnet
  | Testnet !Crypto.ProtocolMagicId
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- This will probably be dropped.
data Transaction status
  = TransactionByron status
  | TransactionShelley status
  deriving (Generic, NFData)
  deriving anyclass NoUnexpectedThunks

data TxSigned
  = TxSignedByron !Byron.Tx !ByteString !(Crypto.Hash Byron.Tx) !(Vector Byron.TxInWitness)
  | TxSignedShelley
  deriving (Generic, NFData)
  deriving NoUnexpectedThunks via UseIsNormalForm TxSigned

data TxUnsigned
  = TxUnsignedByron !Byron.Tx !ByteString !(Crypto.Hash Byron.Tx)
  | TxUnsignedShelley
  deriving (Generic, NFData)
  deriving NoUnexpectedThunks via UseIsNormalForm TxUnsigned

data TxWitness
  = TxWitByron !Byron.TxInWitness
  | TxWitShelley
  deriving (Generic, NFData)
  deriving NoUnexpectedThunks via UseIsNormalForm TxWitness
