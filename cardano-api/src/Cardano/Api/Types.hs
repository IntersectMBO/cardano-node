{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Api.Types
  ( ByronAddress (..)
  , ByronKeyPair (..)
  , ByronPublicKey (..)
  , Network (..)
  , ShelleyAddress (..)
  , ShelleyKeyDiscriminator (..)
  , ShelleyKeyPair (..)
  , ShelleyPublicKey (..)
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
import           Shelley.Spec.Ledger.TxData (Addr(..))

-- The 'Address' data type in 'cardano-sl' is a design train wreck.
-- We need something that is compatible and discard as much of the insanity as possible.
data ByronAddress = ByronAddress !Byron.Address
                  deriving (Eq, Generic , NFData, Show)  -- Byron.Address' needs NFData
                  deriving NoUnexpectedThunks via UseIsNormalForm ByronAddress

data ShelleyAddress = BootStrapAddressShelley !(Addr TPraosStandardCrypto)
                    deriving (Eq, Generic , NFData, Show)
                    deriving NoUnexpectedThunks via UseIsNormalForm ShelleyAddress


-- The Byron key pair use newtype wrappers around 'XPriv'/'Xpub' keys.
-- An 'XPub' is 32 bytes of public key and 32 bytes of Chaincode which is used in the
-- Byron address derivation scheme.
data ByronKeyPair = KeyPairByron !Crypto.VerificationKey !Crypto.SigningKey
                  deriving (Generic, NFData, Show)
                  deriving anyclass NoUnexpectedThunks

data ShelleyKeyPair = KeyPairShelley !(VKey TPraosStandardCrypto) !(SKey TPraosStandardCrypto)
                    | GenesisKeyPairShelley !(VKeyGenesis TPraosStandardCrypto) !(SKey TPraosStandardCrypto)
                    deriving (Eq, Generic, NFData, Show)
                    deriving anyclass NoUnexpectedThunks

-- | A means of discriminating between different kinds of Shelley keys.
data ShelleyKeyDiscriminator
  = GenesisShelleyKey
  | RegularShelleyKey
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks


data ByronPublicKey = PubKeyByron' !Network !Crypto.VerificationKey
                    deriving Show


data ShelleyPublicKey = BootStrapPubKeyShelley !(VKey TPraosStandardCrypto)
                      | GenesisPubKeyShelley !(VKeyGenesis TPraosStandardCrypto)
                      deriving (Eq, Show)


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
