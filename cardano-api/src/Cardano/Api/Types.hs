{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--TODO: eliminate partial conversions:
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Types
  (
    -- * Common types across all eras.
    Address (..)
  , Network (..)
  , NetworkMagic (..)
  , toNetworkMagic
  , toByronNetworkMagic
  , toByronRequiresNetworkMagic
  , toByronProtocolMagic
  , toShelleyNetwork
  , SigningKey (..)
  , GenesisVerificationKey (..)
  , PaymentVerificationKey (..)
  , StakingVerificationKey (..)
  , Certificate (..)
  , TxSigned (..)
  , TxUnsigned (..)
  , TxWitness (..)
  , TxIn (..)
  , TxId (..)
  , TxIx
  , TxOut (..)
  , Withdrawals (..)
  , HasMetaData (..)
  , Update (..)
  , EpochNo (..)
  , SlotNo (..)
  , Lovelace (..)

    -- * Era-specific type aliases and conversions
    -- ** Byron
  , ByronVerificationKey
  , ByronSigningKey
  , ByronAddress
  , ByronTxBody
  , ByronTx
  , ByronTxId
  , ByronTxIn
  , ByronTxOut
  , ByronWitness
  , toByronTxIn
  , toByronTxOut
  , toByronLovelace

    -- ** Shelley
  , ShelleyCertificate
  , ShelleyCoin
  , ShelleyGenesisDelegationCertificate
  , ShelleyGenesisDelegateVerKeyHash
  , ShelleyGenesisVerificationKey
  , ShelleyGenesisVerificationHash
  , ShelleyMIRCertificate
  , ShelleyMIRMap
  , ShelleyPParamsUpdate
  , ShelleyStakePoolMargin
  , ShelleyStakePoolCertificate
  , ShelleyStakePoolMetaData
  , ShelleyStakePoolOwners
  , ShelleyStakePoolRelay
  , ShelleyUpdate
  , ShelleyVerificationKeyPayment
  , ShelleyVerificationKeyHashStaking
  , ShelleyVerificationKeyHashStakePool
  , ShelleyVerificationKeyStakePool
  , ShelleyVerificationKeyStaking
  , ShelleySigningKey
  , ShelleyVRFVerificationKeyHash
  , ShelleyVRFVerificationKey
  , ShelleyAddress
  , ShelleyCredentialStaking
  , ShelleyCredentialStakePool
  , ShelleyDelegationCertificate
  , ShelleyRewardAccount
  , ShelleyWithdrawals
  , ShelleyTxBody
  , ShelleyTx
  , ShelleyTxId
  , ShelleyTxIn
  , ShelleyTxOut
  , ShelleyMetaDataHash
  , ShelleyWitnessVKey
  , toShelleyTxIn
  , toShelleyTxOut
  , toShelleyLovelace
  , fromShelleyTxId
  , fromShelleyTxIn
  , fromShelleyTxOut
  , fromShelleyAddress
  , fromShelleyCoin
  ) where

import           Cardano.Prelude

import           Data.Vector (Vector)

import           Cardano.Slotting.Slot (SlotNo (..), EpochNo (..))
import           Ouroboros.Network.Magic (NetworkMagic(..))

import qualified Cardano.Crypto.Hash.Class   as Crypto
import qualified Cardano.Crypto.Hash.Blake2b as Crypto

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO   as Byron
import qualified Cardano.Crypto       as Byron

--import qualified Cardano.Crypto.Hash                         as HASH
--import qualified Cardano.Crypto.VRF                          as VRF
import qualified Ouroboros.Consensus.Shelley.Protocol.Crypto as Shelley
import qualified Shelley.Spec.Ledger.Address                 as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes               as Shelley
import qualified Shelley.Spec.Ledger.Coin                    as Shelley
import qualified Shelley.Spec.Ledger.Credential              as Shelley
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Shelley
import qualified Shelley.Spec.Ledger.Keys                    as Shelley
import qualified Shelley.Spec.Ledger.MetaData                as Shelley
import qualified Shelley.Spec.Ledger.PParams                 as Shelley
import qualified Shelley.Spec.Ledger.TxData                  as Shelley
import qualified Shelley.Spec.Ledger.Tx                      as Shelley


type ByronVerificationKey = Byron.VerificationKey
type ByronSigningKey      = Byron.SigningKey
type ByronAddress         = Byron.Address
type ByronTxIn            = Byron.TxIn
type ByronTxOut           = Byron.TxOut
type ByronTxBody          = Byron.Tx
type ByronTx              = Byron.TxAux
type ByronTxId            = Byron.TxId
type ByronWitness         = Byron.TxInWitness

type ShelleyCertificate                  = Shelley.DCert Shelley.TPraosStandardCrypto
type ShelleyCoin                         = Shelley.Coin
type ShelleyCredentialStaking            = Shelley.Credential Shelley.Staking Shelley.TPraosStandardCrypto
type ShelleyCredentialStakePool          = Shelley.Credential Shelley.StakePool Shelley.TPraosStandardCrypto
type ShelleyWithdrawals                  = Shelley.Wdrl Shelley.TPraosStandardCrypto
type ShelleyUpdate                       = Shelley.Update Shelley.TPraosStandardCrypto
type ShelleyPParamsUpdate                = Shelley.PParamsUpdate
type ShelleyVerificationKeyPayment       = Shelley.VKey Shelley.Payment Shelley.TPraosStandardCrypto
type ShelleyVerificationKeyStaking       = Shelley.VKey Shelley.Staking Shelley.TPraosStandardCrypto
type ShelleyVerificationKeyStakePool     = Shelley.VKey Shelley.StakePool Shelley.TPraosStandardCrypto
type ShelleyVerificationKeyHashStaking   = Shelley.KeyHash Shelley.Staking Shelley.TPraosStandardCrypto
type ShelleyVerificationKeyHashStakePool = Shelley.KeyHash Shelley.StakePool Shelley.TPraosStandardCrypto
type ShelleyVRFVerificationKeyHash       = Shelley.Hash   Shelley.TPraosStandardCrypto (Shelley.VerKeyVRF Shelley.TPraosStandardCrypto)
type ShelleyVRFVerificationKey           = Shelley.VerKeyVRF Shelley.TPraosStandardCrypto
type ShelleySigningKey                   = Shelley.SignKeyDSIGN Shelley.TPraosStandardCrypto
type ShelleyAddress                      = Shelley.Addr   Shelley.TPraosStandardCrypto
type ShelleyTxIn                         = Shelley.TxIn   Shelley.TPraosStandardCrypto
type ShelleyTxOut                        = Shelley.TxOut  Shelley.TPraosStandardCrypto
type ShelleyTxBody                       = Shelley.TxBody Shelley.TPraosStandardCrypto
type ShelleyTx                           = Shelley.Tx     Shelley.TPraosStandardCrypto
type ShelleyTxId                         = Shelley.TxId   Shelley.TPraosStandardCrypto
type ShelleyWitnessVKey                  = Shelley.WitVKey Shelley.TPraosStandardCrypto Shelley.Witness
type ShelleyDelegationCertificate        = Shelley.DCert      Shelley.TPraosStandardCrypto

type ShelleyStakePoolCertificate         = Shelley.PoolCert Shelley.TPraosStandardCrypto
type ShelleyStakePoolOwners              = Set (Shelley.KeyHash Shelley.Staking Shelley.TPraosStandardCrypto)

type ShelleyGenesisVerificationKey       = Shelley.VKey Shelley.Genesis Shelley.TPraosStandardCrypto
type ShelleyGenesisVerificationHash      = Shelley.KeyHash Shelley.Genesis Shelley.TPraosStandardCrypto
type ShelleyGenesisDelegateVerKeyHash    = Shelley.KeyHash Shelley.GenesisDelegate Shelley.TPraosStandardCrypto
type ShelleyGenesisDelegationCertificate = Shelley.GenesisDelegCert Shelley.TPraosStandardCrypto
type ShelleyMIRCertificate               = Shelley.MIRCert Shelley.TPraosStandardCrypto
type ShelleyMIRMap                       = Map (Shelley.Credential Shelley.Staking Shelley.TPraosStandardCrypto) Shelley.Coin
type ShelleyStakePoolMargin              = Shelley.UnitInterval
type ShelleyStakePoolMetaData            = Shelley.PoolMetaData
type ShelleyStakePoolRelay               = Shelley.StakePoolRelay
type ShelleyRewardAccount                = Shelley.RewardAcnt Shelley.TPraosStandardCrypto
type ShelleyMetaDataHash                 = Shelley.MetaDataHash Shelley.TPraosStandardCrypto

-- The 'Address' data type in 'cardano-sl' is a design train wreck.
-- We need something that is compatible and discard as much of the insanity as possible.
data Address
  = AddressByron !ByronAddress
  | AddressShelley !ShelleyAddress
  | AddressShelleyReward !ShelleyRewardAccount
    -- ^ The Shelley reward account is not a UTxO
  deriving (Eq, Generic , Ord, NFData, Show)  -- Byron.Address' needs NFData
  deriving NoUnexpectedThunks via UseIsNormalForm Address


data Certificate
  = ShelleyDelegationCertificate !ShelleyCertificate
  | ShelleyStakePoolCertificate !ShelleyCertificate
  | ShelleyGenesisDelegationCertificate !ShelleyCertificate
  | ShelleyMIRCertificate !ShelleyCertificate
  deriving (Eq, Show)



-- | The combination of a verification key and a signing key.
--
-- Verification keys are also commonly known as \"public keys\".
--
-- Signing keys are also commonly known as \"private keys\" or \"secret keys\".
--
data SigningKey
  -- The Byron key pair use newtype wrappers around 'XPriv'/'Xpub' keys.
  -- An 'XPub' is 32 bytes of public key and 32 bytes of Chaincode which is used in the
  -- Byron address derivation scheme.
  = SigningKeyByron   !ByronSigningKey
  | SigningKeyShelley !ShelleySigningKey
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- | A verification key for use in genesis.
--
data GenesisVerificationKey
  = GenesisVerificationKeyShelley !ShelleyGenesisVerificationKey
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- | A verification key for use in addresses (payment).
--
-- Verification keys are also commonly known as \"public keys\".
--
data PaymentVerificationKey
  = PaymentVerificationKeyByron !ByronVerificationKey
  | PaymentVerificationKeyShelley !ShelleyVerificationKeyPayment
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- | A verification key for use in addresses (staking).
--
-- Verification keys are also commonly known as \"public keys\".
--
data StakingVerificationKey
  = StakingVerificationKeyShelley !ShelleyVerificationKeyStaking
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

data Withdrawals
  = WithdrawalsShelley !ShelleyWithdrawals
  deriving (Eq, Generic, {-NFData, TODO-} Show)
  deriving anyclass NoUnexpectedThunks

data HasMetaData
  = HasMetaData
  | HasNoMetaData
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

data Update = ShelleyUpdate (Shelley.Update Shelley.TPraosStandardCrypto)
  deriving (Eq, Generic, Show)
  deriving anyclass NoUnexpectedThunks

-- The cardano-sl codebase (and cardano-ledger) has something a little like
-- this (actually isomorphic with Maybe):
data Network
  = Mainnet
  | Testnet !NetworkMagic
  deriving (Eq, Generic, {-NFData, TODO-} Show)
  deriving anyclass NoUnexpectedThunks

data TxIn = TxIn !TxId !TxIx
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

newtype TxId = TxId (Crypto.Hash Crypto.Blake2b_256 ())
  deriving (Eq, Generic, Show)
  deriving newtype (NFData, NoUnexpectedThunks)

type TxIx = Word

data TxOut = TxOut !Address !Lovelace
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

newtype Lovelace
  = Lovelace { unLoveLace :: Integer }
  deriving (Eq, Generic)
  deriving newtype (Read, Show)
  deriving anyclass (NFData, NoUnexpectedThunks)

toNetworkMagic :: Network -> NetworkMagic
toNetworkMagic nw =
  case nw of
    Mainnet    -> NetworkMagic 764824073 -- The network magic for mainnet
    Testnet nm -> nm

toByronNetworkMagic :: Network -> Byron.NetworkMagic
toByronNetworkMagic nw =
  case nw of
    Mainnet                   -> Byron.NetworkMainOrStage
    Testnet (NetworkMagic nm) -> Byron.NetworkTestnet nm

toByronRequiresNetworkMagic :: Network -> Byron.RequiresNetworkMagic
toByronRequiresNetworkMagic Mainnet   = Byron.RequiresNoMagic
toByronRequiresNetworkMagic Testnet{} = Byron.RequiresMagic

toByronProtocolMagic :: Network -> Byron.ProtocolMagicId
toByronProtocolMagic Mainnet = Byron.mainnetProtocolMagicId
toByronProtocolMagic (Testnet (NetworkMagic pm)) = Byron.ProtocolMagicId pm

toShelleyNetwork :: Network -> Shelley.Network
toShelleyNetwork  Mainnet    = Shelley.Mainnet
toShelleyNetwork (Testnet _) = Shelley.Testnet

toByronTxIn  :: TxIn  -> ByronTxIn
toByronTxIn (TxIn txid txix) =
    Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toByronTxOut :: TxOut -> ByronTxOut
toByronTxOut (TxOut (AddressByron addr) value) =
    Byron.TxOut addr (toByronLovelace value)
toByronTxOut (TxOut (AddressShelley _) _) =
    panic "TODO: toByronTxOut AddressShelley"
toByronTxOut (TxOut (AddressShelleyReward _) _) =
    panic "TODO: toByronTxOut AddressShelleyReward"

toShelleyTxIn :: TxIn -> ShelleyTxIn
toShelleyTxIn (TxIn txid txix) =
    Shelley.TxIn (toShelleyTxId txid) (fromIntegral txix)

toShelleyTxOut :: TxOut -> ShelleyTxOut
toShelleyTxOut (TxOut (AddressShelley addr) value) =
    Shelley.TxOut addr (toShelleyLovelace value)
toShelleyTxOut (TxOut (AddressShelleyReward _) _) =
    panic "toShelleyTxOut AddressShelleyReward - Reward addresses are not UTxO addresses"
toShelleyTxOut (TxOut (AddressByron _) _) =
    panic "TODO: toShelleyTxOut convert byron address to Shelley bootstrap address"

toByronTxId :: TxId -> ByronTxId
toByronTxId (TxId (Crypto.UnsafeHash h)) =
    Byron.unsafeHashFromBytes h

toShelleyTxId :: TxId -> ShelleyTxId
toShelleyTxId (TxId (Crypto.UnsafeHash h)) =
    Shelley.TxId (Crypto.UnsafeHash h)

toByronLovelace :: Lovelace -> Byron.Lovelace
toByronLovelace (Lovelace x) = x' where Right x' = Byron.integerToLovelace x
                  --TODO: deal with partial conversion

toShelleyLovelace :: Lovelace -> Shelley.Coin
toShelleyLovelace (Lovelace l) = Shelley.Coin l

fromShelleyAddress :: ShelleyAddress -> Address
fromShelleyAddress = AddressShelley

fromShelleyCoin :: ShelleyCoin -> Lovelace
fromShelleyCoin (Shelley.Coin amount) = Lovelace amount

fromShelleyTxId :: Shelley.TxId crypto -> TxId
fromShelleyTxId (Shelley.TxId (Crypto.UnsafeHash h)) = TxId (Crypto.UnsafeHash h)

fromShelleyTxIn :: Shelley.TxIn crypto -> TxIn
fromShelleyTxIn (Shelley.TxIn txid ix) = TxIn (fromShelleyTxId txid) (fromIntegral ix)

fromShelleyTxOut :: ShelleyTxOut -> TxOut
fromShelleyTxOut (Shelley.TxOut addr amount) =
  TxOut (fromShelleyAddress addr) (fromShelleyCoin amount)


data TxSigned
  = TxSignedByron !ByronTxBody !ByteString !(Byron.Hash ByronTxBody) !(Vector Byron.TxInWitness)
  | TxSignedShelley !ShelleyTx
  deriving (Eq, Generic, {-NFData, TODO -} Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxSigned

data TxUnsigned
  = TxUnsignedByron !ByronTxBody !ByteString !(Byron.Hash ByronTxBody)
  | TxUnsignedShelley !ShelleyTxBody
  deriving (Eq, Generic, {-NFData, TODO -} Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxUnsigned

data TxWitness
  = TxWitByron   !ByronWitness
  | TxWitShelley !ShelleyWitnessVKey
--    | TxWitShelleyScript !ShelleyWitnessScript
  deriving (Generic{-, NFData TODO -})
  deriving NoUnexpectedThunks via UseIsNormalForm TxWitness
