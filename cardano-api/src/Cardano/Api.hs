{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api
  ( module X

    -- * Keys
  , SigningKey (..)
  , VerificationKey (..)
  , getVerificationKey
  , byronGenSigningKey
  , shelleyGenSigningKey

    -- * Addresses
  , Address (..)
  , Network (..)
  , NetworkMagic (..)
  , byronVerificationKeyAddress
  , shelleyVerificationKeyAddress

    -- * Transactions
  , TxSigned (..)
  , TxUnsigned (..)
  , TxWitness (..)
  , TxIn (..)
  , TxOut (..)
  , TxId (..)
  , TxIx
  , Lovelace (..)
  , SlotNo (..)

  , ShelleyTxBody

  , getTxSignedBody
  , getTxSignedHash
  , getTxSignedWitnesses
  , getTxUnsignedBody
  , getTxUnsignedHash

  , buildByronTransaction
  , buildShelleyTransaction
  , signTransaction
  , witnessTransaction
  , signTransactionWithWitness

  -- * Node local state queries
  , LocalStateQueryError (..)
  , renderLocalStateQueryError
  , queryFilteredUTxOFromLocalState
  , queryPParamsFromLocalState

  , ShelleyCredentialStaking
  , ShelleyRewardAccount
  , ShelleyStakePoolMetaData
  , ShelleyStakePoolOwners
  , ShelleyStakePoolRelay
  , ShelleyVerificationKeyHashStaking
  , ShelleyVerificationKeyHashStakePool
  , ShelleyVerificationKeyStakePool
  , ShelleyVerificationKeyStaking
  , ShelleyVRFVerificationKeyHash
  , mkShelleyStakingCredential

  -- * Shelley Delegation Certificate Related
  , Certificate(..)
  , shelleyDeregisterStakingAddress
  , shelleyDelegateStake
  , shelleyGenesisDelegateStake
  , shelleyMIRCertificate
  , shelleyRegisterStakingAddress
  , shelleyRegisterStakePool
  , shelleyRetireStakePool
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Sequence.Strict as Seq

import qualified Cardano.Binary as Binary

import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import           Cardano.Crypto.Seed (readSeedFromSystemEntropy)

import qualified Cardano.Crypto.Hashing as Crypto
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import           Cardano.Crypto.Random (runSecureRandom)
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Api.Types
import           Cardano.Api.CBOR as X
import           Cardano.Api.Convert as X
import           Cardano.Api.Error as X
import           Cardano.Api.View as X
import           Cardano.Api.TxSubmitChairman as X
import           Cardano.Api.TxSubmit as X
import           Cardano.Api.LocalStateQuery

import qualified Cardano.Chain.Common  as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO    as Byron

import qualified Ouroboros.Consensus.Shelley.Protocol.Crypto as Shelley
import qualified Shelley.Spec.Ledger.Keys      as Shelley
import qualified Shelley.Spec.Ledger.Slot      as Shelley
import qualified Shelley.Spec.Ledger.TxData    as Shelley
import qualified Shelley.Spec.Ledger.Tx        as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley


byronGenSigningKey :: IO SigningKey
byronGenSigningKey =
    SigningKeyByron . snd <$> runSecureRandom Crypto.keyGen

shelleyGenSigningKey :: IO SigningKey
shelleyGenSigningKey = do
    seed <- readSeedFromSystemEntropy (seedSizeDSIGN dsignProxy)
    let sk = genKeyDSIGN seed
    return (SigningKeyShelley sk)
  where
    dsignProxy :: Proxy (Shelley.DSIGN Shelley.TPraosStandardCrypto)
    dsignProxy = Proxy

-- | Register a shelley staking pool.
shelleyRegisterStakePool
  :: ShelleyVerificationKeyHashStakePool
  -- ^ Pool public key hash.
  -> ShelleyVRFVerificationKeyHash
  -- ^ VRF verification key hash.
  -> ShelleyCoin
  -- ^ Pool pledge.
  -> ShelleyCoin
  -- ^ Pool cost.
  -> ShelleyStakePoolMargin
  -- ^ Pool margin.
  -> ShelleyRewardAccount
  -- ^ Pool reward account.
  -> ShelleyStakePoolOwners
  -- ^ Pool owners.
  -> [Shelley.StakePoolRelay]
  -- ^ Pool relays.
  -> Maybe Shelley.PoolMetaData
  -> Certificate
shelleyRegisterStakePool poolVkeyHash vrfVkeyHash pldg cst
                          mrgn rwdact ownrs relays md = do
  let poolPubKeyHash = poolVkeyHash
      poolVRFkeyHash = vrfVkeyHash
      poolPledge = pldg
      poolCost = cst
      poolMargin = mrgn
      poolRewardAcnt = rwdact
      poolOwners = ownrs
      poolRelays = Seq.fromList relays
      poolMetaData = Shelley.maybeToStrictMaybe md

  let poolParams = Shelley.PoolParams
                     { Shelley._poolPubKey = poolPubKeyHash
                     , Shelley._poolVrf = poolVRFkeyHash
                     , Shelley._poolPledge = poolPledge
                     , Shelley._poolCost = poolCost
                     , Shelley._poolMargin = poolMargin
                     , Shelley._poolRAcnt = poolRewardAcnt
                     , Shelley._poolOwners = poolOwners
                     , Shelley._poolRelays = poolRelays
                     , Shelley._poolMD = poolMetaData
                     }
  ShelleyStakePoolCertificate . Shelley.DCertPool . Shelley.RegPool $ poolParams

-- | Retire a shelley staking pool.
shelleyRetireStakePool :: ShelleyVerificationKeyStakePool -> Shelley.EpochNo -> Certificate
shelleyRetireStakePool vKey eNo =
  ShelleyStakePoolCertificate . Shelley.DCertPool $ Shelley.RetirePool (Shelley.hashKey vKey) eNo


-- | Register a shelley staking key.
shelleyRegisterStakingAddress
  :: ShelleyVerificationKeyHashStaking
  -> Certificate
shelleyRegisterStakingAddress vKeyHash= do
  let cred = mkShelleyStakingCredential vKeyHash
  ShelleyDelegationCertificate $ Shelley.DCertDeleg $ Shelley.RegKey cred

-- | Deregister a shelley staking key.
shelleyDeregisterStakingAddress
  :: ShelleyVerificationKeyHashStaking
  -> Certificate
shelleyDeregisterStakingAddress vKeyHash = do
  let cred = mkShelleyStakingCredential vKeyHash
  ShelleyDelegationCertificate $ Shelley.DCertDeleg $ Shelley.DeRegKey cred

-- | Delegate your stake (as the delegator) to a specified delegatee.
shelleyDelegateStake
  :: ShelleyVerificationKeyHashStaking
  -- ^ Delegator verification key hash
  -> ShelleyVerificationKeyHashStakePool
  -- ^ Delegatee verification key hash
  -> Certificate
shelleyDelegateStake delegatorKeyHash delegateeKeyHash = do
  let cred = mkShelleyStakingCredential delegatorKeyHash
  ShelleyDelegationCertificate $ Shelley.DCertDeleg $ Shelley.Delegate $ Shelley.Delegation cred delegateeKeyHash

mkShelleyStakingCredential :: ShelleyVerificationKeyHashStaking -> ShelleyCredentialStaking
mkShelleyStakingCredential vKey =
  Shelley.KeyHashObj vKey

-- | Delegate your genesis key's stake
shelleyGenesisDelegateStake
  :: ShelleyGenesisVerificationHash
  -> ShelleyGenesisDelegateVerKeyHash
  -> Certificate
shelleyGenesisDelegateStake genDelegatorHash genDelegateeHash = do
  ShelleyGenesisDelegationCertificate . Shelley.DCertGenesis $ Shelley.GenesisDelegCert genDelegatorHash genDelegateeHash

-- | Move instantaneous rewards.
shelleyMIRCertificate
  :: ShelleyMIRMap
  -> Certificate
shelleyMIRCertificate mirMap =
  ShelleyMIRCertificate . Shelley.DCertMir $ Shelley.MIRCert mirMap


-- Given key information (public key, and other network parameters), generate an Address.
-- Originally: mkAddress :: Network -> VerificationKey -> VerificationKeyInfo -> Address
-- but since VerificationKeyInfo already has the VerificationKey and Network, it can be simplified.
-- This is true for Byron, but for Shelley there’s also an optional StakeAddressRef as input to
-- Address generation

byronVerificationKeyAddress :: VerificationKey -> Network -> Address
byronVerificationKeyAddress vkey nw =
  case vkey of
    VerificationKeyByron vk -> AddressByron $ Byron.makeVerKeyAddress (byronNetworkMagic nw) vk
    VerificationKeyShelley _ -> panic "Cardano.Api.byronVerificationKeyAddress: VerificationKeyInfoShelley"

shelleyVerificationKeyAddress :: VerificationKey -> Network -> Address
shelleyVerificationKeyAddress vkey _nw =
  case vkey of
    VerificationKeyByron _ -> panic "Cardano.Api.shelleyVerificationKeyAddress: VerificationKeyByron"
    VerificationKeyShelley vk ->
      AddressShelley $
        --TODO: we cannot use toAddr or toCred here because they unnecessarily
        -- require a full key pair, when only the pub key is needed, and that
        -- is all we have here
        Shelley.Addr (Shelley.KeyHashObj (Shelley.hashKey vk))
                     Shelley.StakeRefNull

getVerificationKey :: SigningKey -> VerificationKey
getVerificationKey kp =
  case kp of
    SigningKeyByron sk -> VerificationKeyByron vk
      where
        vk = Crypto.toVerification sk

    SigningKeyShelley sk -> VerificationKeyShelley (Shelley.VKey vk)
      where
        vk = deriveVerKeyDSIGN sk

byronNetworkMagic :: Network -> Byron.NetworkMagic
byronNetworkMagic nw =
  case nw of
    Mainnet -> Byron.NetworkMainOrStage
    Testnet (NetworkMagic nm) -> Byron.NetworkTestnet nm

-- Create new Transaction
-- ledger creates transaction and serialises it as CBOR - txBuilder
-- fine for Byron

-- Currently this is only for Byron transactions.
-- Shelly transactions will take lots of extra parameters.
-- For Shelley, transactions can be constructed from any combination of Byron and Shelley
-- transactions.
-- Any set ot inputs/outputs that only contain Byron versions should generate a Byron transaction.
-- Any set ot inputs/outputs that contain any Shelley versions should generate a Shelley transaction.
buildByronTransaction :: [TxIn] -> [TxOut] -> TxUnsigned
buildByronTransaction ins outs =
    TxUnsignedByron bTx bTxCbor bTxHash
  where
    bTx :: Byron.Tx
    bTx = Byron.UnsafeTx (NonEmpty.fromList (map toByronTxIn ins))
                         (NonEmpty.fromList (map toByronTxOut outs))
                         (Byron.mkAttributes ())
                         --TODO: handle partial conversions (non-empty)

    bTxCbor :: ByteString
    bTxCbor = Binary.serialize' bTx

    bTxHash :: Crypto.Hash Byron.Tx
    bTxHash = coerce $ Crypto.hashRaw (LBS.fromStrict bTxCbor)


buildShelleyTransaction
  :: [TxIn]
  -> [TxOut]
  -> SlotNo
  -> Lovelace
  -> [Certificate]
  -> TxUnsigned
buildShelleyTransaction txins txouts ttl fee certs = do
  let relevantCerts = [ certDiscrim c | c <- certs ]
  TxUnsignedShelley $
    Shelley.TxBody
      (Set.fromList (map toShelleyTxIn  txins))
      (Seq.fromList (map toShelleyTxOut txouts))
      (Seq.fromList relevantCerts)  -- certificates
      (Shelley.Wdrl Map.empty)      -- withdrawals
      (toShelleyLovelace fee)
      ttl
      Shelley.SNothing              -- update proposals
      Shelley.SNothing              -- metadata hash
 where
   certDiscrim :: Certificate -> ShelleyCertificate
   certDiscrim (ShelleyDelegationCertificate delegCert) = delegCert
   certDiscrim (ShelleyStakePoolCertificate sPoolCert) = sPoolCert
   certDiscrim (ShelleyGenesisDelegationCertificate sGenDelegCert) = sGenDelegCert
   certDiscrim (ShelleyMIRCertificate mirCert) = mirCert
{-
inputs outputs, attributes:
ATxAux { Tx TxWiness Annotation }

Unsigned is just a Tx

no representation difference for Signed and Checked

mkTxAux

node: signTxId

cardano-node/cardano-node/src/Cardano/CLI/Tx.hs:txSpendUTxOByronPBFT
txSpendUTxOByronPBFT (PBFT is void)
  which calls signTxId

cardano-node/cardano-node/src/Cardano/CLI/Tx.hs:txSpendGenesisUTxOByronPBFT ???

cardano-ledger/crypto/src/Cardano/Crypto/Signing/Signature.hs:sign

dont need support Redeem, do need to support Proposal and Votes (possibly Del Certs)




-}


-- Use the private key to give one witness to a transaction
-- (TxInWirtness is fine for Byron on shelley, need a TxWitness type with Byron/Shelley ctors)
witnessTransaction :: TxUnsigned -> Network -> SigningKey -> TxWitness
witnessTransaction txu nw signKey =
    case txu of
      TxUnsignedByron _tx _txcbor txHash -> TxWitByron $ byronWitnessTransaction txHash nw signKey
      TxUnsignedShelley tx ->
        TxWitShelley $ shelleyWitnessTransaction tx signKey

byronWitnessTransaction :: Crypto.Hash Byron.Tx -> Network -> SigningKey -> ByronWitness
byronWitnessTransaction _ _ (SigningKeyShelley _) = panic "Cardano.Api.byronWitnessTransaction: Please use a byron signing key"
byronWitnessTransaction txHash nw (SigningKeyByron signKey) =
    Byron.VKWitness
      (Crypto.toVerification signKey)
      (Crypto.sign protocolMagic Crypto.SignTx signKey (Byron.TxSigData txHash))
  where
    -- This is unlikely to be specific to Byron or Shelley
    protocolMagic :: ProtocolMagicId
    protocolMagic =
      case nw of
        Mainnet -> Byron.mainnetProtocolMagicId
        Testnet (NetworkMagic pm) -> ProtocolMagicId pm

shelleyWitnessTransaction :: ShelleyTxBody -> SigningKey -> ShelleyWitnessVKey
shelleyWitnessTransaction _ (SigningKeyByron _) = panic "Cardano.Api.shelleyWitnessTransaction: Please use a shelley signing key"
shelleyWitnessTransaction txbody (SigningKeyShelley sk) =
    Shelley.WitVKey vk sig
  where
    vk  = Shelley.VKey (deriveVerKeyDSIGN sk)
    sig = Shelley.signedDSIGN @Shelley.TPraosStandardCrypto sk txbody


-- Sign Transaction - signTransaction is built over witnesseTransaction/signTransactionWithWitness
-- we could have this fail if the wrong (or too many/few) keys are provided, in which case it’d
-- return Transaction Checked
-- either [PrivKey] have to be in the right order, or more usable we check and reorder them get
-- them to be the right ones, since in Byron txs, witnesses are a list that has match up with the
-- tx inputs, i.e same number and in the right order. In Shelley they’re a set, so don’t need to
-- provide duplicate sigs for multiple inputs that share the same input address.
signTransaction :: TxUnsigned -> Network -> [SigningKey] -> TxSigned
signTransaction txu nw sks =
  case txu of
    TxUnsignedByron tx txcbor txHash ->
      TxSignedByron tx txcbor txHash (Vector.fromList $ map (byronWitnessTransaction txHash nw) sks)

    TxUnsignedShelley txbody ->
        TxSignedShelley $
          Shelley.Tx
            txbody
            keyWitnesses
            Map.empty         -- script witnesses
            Shelley.SNothing  -- metadata
      where
        keyWitnesses = Set.fromList (map (shelleyWitnessTransaction txbody) sks)


-- Verify that the transaction has been fully witnessed
-- same decision about checking or not, that all witnesses are the right ones and in the right order etc
signTransactionWithWitness :: TxUnsigned -> [Byron.TxInWitness] -> TxSigned
signTransactionWithWitness txu ws =
  case txu of
    TxUnsignedByron tx txcbor txHash ->
      TxSignedByron tx txcbor txHash (Vector.fromList ws)
    TxUnsignedShelley _tx ->
      panic "Cardano.Api.signTransactionWithWitness: TxUnsignedShelley"


-- Verify that Transaction is Complete (fully signed)
-- part of TxBuilder
-- Or we might not have this separate step at all if we bundle checking into the earlier steps of
-- tx construction, it’s a choice we have
-- For Shelley, checking that we have provided the right set of witnesses is more complicated due
-- to multisig, involves evaluating the multisig scripts to see if the necessary sigs are present.
-- Would be more complicated to check there are not too many.
--
-- It is not actually possible to implement checkTransaction because that would
-- require access to the UTxO set.
--
-- checkTransaction :: Transaction TxSigned -> Maybe (Transaction TxChecked)
-- checkTransaction = panic "Cardano.Api.checkTransaction"



-- Extract transaction information - getTransactionId may be redundant
-- part of TxBuilder
getTxSignedBody :: TxSigned -> ByteString
getTxSignedBody txs =
  case txs of
    TxSignedByron _tx txCbor _txHash _txWit -> txCbor
    TxSignedShelley _tx -> panic "Cardano.Api.getTxSignedBody: TxUnsignedShelley"

getTxSignedHash :: TxSigned -> Crypto.Hash TxSigned
getTxSignedHash txs =
  case txs of
    TxSignedByron _tx _txCbor txHash _txWit -> coerce txHash
    TxSignedShelley _tx -> panic "Cardano.Api.getSignedHash: TxSignedShelley"

getTxSignedWitnesses :: TxSigned -> [TxWitness]
getTxSignedWitnesses txs =
  case txs of
    TxSignedByron _tx _txCbor _txHash txWit -> map TxWitByron (Vector.toList txWit)
    TxSignedShelley _tx -> panic "Cardano.Api.getTxSignedWitnesses: TxUnsignedShelley"

getTxUnsignedHash :: TxUnsigned -> Crypto.Hash TxUnsigned
getTxUnsignedHash txu =
  case txu of
    TxUnsignedByron _tx _txCbor txHash -> coerce txHash
    TxUnsignedShelley _tx -> panic "Cardano.Api.getTxUnsignedHash: TxUnsignedShelley"

getTxUnsignedBody :: TxUnsigned -> ByteString
getTxUnsignedBody txu =
  case txu of
    TxUnsignedByron _tx txCbor _txHash -> txCbor
    TxUnsignedShelley _tx -> panic "Cardano.Api.getTxUnsignedHash: TxUnsignedShelley"


-- Separate functons for TxUnsigned/TxSigned etc


-- getTransactionBody
-- getTransactionWitnesses
-- or separate accessor functions
-- the txid should be cached, it might be already. There was a ticket about doing that in the ledger
-- so consensus doesn’t have to do it elsewhere
