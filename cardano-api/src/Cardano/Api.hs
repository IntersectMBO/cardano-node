{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api
  ( module X

    -- * Keys
  , SigningKey (..)
  , GenesisVerificationKey (..)
  , PaymentVerificationKey (..)
  , StakingVerificationKey (..)
  , getGenesisVerificationKey
  , getPaymentVerificationKey
  , getStakingVerificationKey
  , byronGenSigningKey
  , shelleyGenSigningKey

    -- * Addresses
  , Address (..)
  , byronVerificationKeyAddress
  , shelleyVerificationKeyAddress
  , shelleyVerificationKeyRewardAddress

    -- ** Network identifiers
  , Network (..)
  , NetworkMagic (..)
  , toNetworkMagic
  , toByronNetworkMagic
  , toShelleyNetwork

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
  , Update (..)
  , ShelleyTxBody

  , getTxSignedBody
  , getTxSignedHash
  , getTxSignedWitnesses
  , getTxUnsignedBody
  , getTxUnsignedHash

  , buildByronTransaction
  , buildShelleyTransaction
  , buildDummyShelleyTxForFeeCalc
  , calculateShelleyMinFee
  , signTransaction
  , witnessTransaction
  , signTransactionWithWitness

    -- * Slot related
  , EpochNo

    -- * Node local state queries
  , LocalStateQueryError (..)
  , QueryFilter (..)
  , renderLocalStateQueryError
  , queryUTxOFromLocalState
  , queryLocalLedgerState
  , queryPParamsFromLocalState
  , queryStakeDistributionFromLocalState

    -- * Node local chain sync related
  , getLocalTip

  , ShelleyCoin
  , ShelleyCredentialStaking
  , ShelleyGenesisVerificationHash
  , ShelleyPParamsUpdate
  , ShelleyRewardAccount
  , ShelleyStakePoolMargin
  , ShelleyStakePoolMetaData
  , ShelleyStakePoolOwners
  , ShelleyStakePoolRelay
  , ShelleyUpdate
  , ShelleyVerificationKeyHashStaking
  , ShelleyVerificationKeyHashStakePool
  , ShelleyVerificationKeyStakePool
  , ShelleyVerificationKeyStaking
  , ShelleyVRFVerificationKeyHash
  , ShelleyVRFVerificationKey
  , mkShelleyStakingCredential

  -- * Shelley delegation certificate related
  , Certificate(..)
  , shelleyDeregisterStakingAddress
  , shelleyDelegateStake
  , shelleyGenesisDelegateStake
  , shelleyMIRCertificate
  , shelleyRegisterStakingAddress
  , shelleyRegisterStakePool
  , shelleyRetireStakePool

  -- * Shelley update proposal related
  , Shelley.Nonce (..)
  , Shelley.PParams'(..)
  , Shelley.PParamsUpdate
  , Shelley.ProtVer (..)
  , createShelleyUpdateProposal

  -- * Hashing
  , Shelley.hashKey

  -- * General helpers
  , Shelley.StrictMaybe(..)
  , Shelley.UnitInterval
  , Shelley.mkUnitInterval
  , Shelley.maybeToStrictMaybe
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

import qualified Cardano.Crypto.Hash as Crypto (Blake2b_256, hash)
import qualified Cardano.Crypto.Hashing as Crypto hiding (hash)
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import           Cardano.Crypto.Random (runSecureRandom)
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Api.Types
import           Cardano.Api.CBOR as X
import           Cardano.Api.Convert as X
import           Cardano.Api.Error as X
import           Cardano.Api.MetaData as X
import           Cardano.Api.View as X
import           Cardano.Api.TxSubmit as X
import           Cardano.Api.LocalChainSync
import           Cardano.Api.LocalStateQuery

import qualified Cardano.Chain.Common  as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO    as Byron

import qualified Ouroboros.Consensus.Shelley.Protocol.Crypto as Shelley
import qualified Shelley.Spec.Ledger.Address     as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes   as Shelley
import qualified Shelley.Spec.Ledger.Credential  as Shelley
import qualified Shelley.Spec.Ledger.Keys        as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley (minfee)
import qualified Shelley.Spec.Ledger.PParams     as Shelley
import qualified Shelley.Spec.Ledger.Slot        as Shelley
import qualified Shelley.Spec.Ledger.TxData      as Shelley
import qualified Shelley.Spec.Ledger.Tx          as Shelley
import qualified Shelley.Spec.Ledger.UTxO        as Shelley (hashTxBody)


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


---
-- Shelley staking related
---

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
  ShelleyStakePoolCertificate
    . Shelley.DCertPool
    . Shelley.RegPool
    $ Shelley.PoolParams
        { Shelley._poolPubKey = poolVkeyHash
        , Shelley._poolVrf = vrfVkeyHash
        , Shelley._poolPledge = pldg
        , Shelley._poolCost = cst
        , Shelley._poolMargin = mrgn
        , Shelley._poolRAcnt = rwdact
        , Shelley._poolOwners = ownrs
        , Shelley._poolRelays = Seq.fromList relays
        , Shelley._poolMD = Shelley.maybeToStrictMaybe md
        }


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

---
-- Shelley update related
---

createShelleyUpdateProposal
  :: Shelley.EpochNo
  -- ^ Epoch in which the proposal is valid.
  -> [ShelleyGenesisVerificationHash]
  -> ShelleyPParamsUpdate
  -> Shelley.Update Shelley.TPraosStandardCrypto
createShelleyUpdateProposal epNo genKeyHashes pParamsUpdate = do
  let propUpdates = Shelley.ProposedPPUpdates . Map.fromList $ zip genKeyHashes (repeat pParamsUpdate)
  Shelley.Update propUpdates epNo


-- Given key information (public key, and other network parameters), generate an Address.
-- Originally: mkAddress :: Network -> VerificationKey -> VerificationKeyInfo -> Address
-- but since VerificationKeyInfo already has the VerificationKey and Network, it can be simplified.
-- This is true for Byron, but for Shelley there’s also an optional StakeAddressRef as input to
-- Address generation

byronVerificationKeyAddress :: PaymentVerificationKey -> Network -> Address
byronVerificationKeyAddress vkey nw =
  case vkey of
    PaymentVerificationKeyByron vk -> AddressByron $ Byron.makeVerKeyAddress (byronNetworkMagic nw) vk
    PaymentVerificationKeyShelley _ -> panic "Cardano.Api.byronVerificationKeyAddress: VerificationKeyInfoShelley"

shelleyVerificationKeyAddress
  :: Network
  -> PaymentVerificationKey
  -> Maybe StakingVerificationKey
  -> Address
shelleyVerificationKeyAddress _ (PaymentVerificationKeyByron _) _ =
  panic "Cardano.Api.shelleyVerificationKeyAddress: PaymentVerificationKeyByron"
shelleyVerificationKeyAddress nw (PaymentVerificationKeyShelley payVKey) mbStkVKey = do
  case mbStkVKey of
    -- Build a Shelley base address.
    Just (StakingVerificationKeyShelley stkVKey) -> do
      -- TODO: we cannot use toAddr or toCred here because they unnecessarily
      -- require a full key pair, when only the pub key is needed, and that
      -- is all we have here
      let paymentCredential = Shelley.KeyHashObj $ Shelley.hashKey payVKey
          stakingCredential = Shelley.StakeRefBase . Shelley.KeyHashObj $ Shelley.hashKey stkVKey
      AddressShelley $ Shelley.Addr (toShelleyNetwork nw) paymentCredential stakingCredential

    -- Build a Shelley enterprise address.
    Nothing ->
      AddressShelley $
        -- TODO: we cannot use toAddr or toCred here because they unnecessarily
        -- require a full key pair, when only the pub key is needed, and that
        -- is all we have here
        Shelley.Addr (toShelleyNetwork nw)
                     (Shelley.KeyHashObj (Shelley.hashKey payVKey))
                     Shelley.StakeRefNull

-- | Shelley reward accounts are not UTxO addresses so they are handled differently.
shelleyVerificationKeyRewardAddress :: Network -> StakingVerificationKey -> ShelleyRewardAccount
shelleyVerificationKeyRewardAddress nw (StakingVerificationKeyShelley stkVKey) = do
  let stakingCredential = Shelley.KeyHashObj $ Shelley.hashKey stkVKey
  Shelley.mkRwdAcnt (toShelleyNetwork nw) stakingCredential

getGenesisVerificationKey :: SigningKey -> GenesisVerificationKey
getGenesisVerificationKey kp =
  case kp of
    -- TODO: Implement this
    SigningKeyByron _ -> panic "getGenesisVerificationKey SigningKeyByron"

    SigningKeyShelley sk -> GenesisVerificationKeyShelley (Shelley.VKey vk)
      where
        vk = deriveVerKeyDSIGN sk

getPaymentVerificationKey :: SigningKey -> PaymentVerificationKey
getPaymentVerificationKey kp =
  case kp of
    SigningKeyByron sk -> PaymentVerificationKeyByron vk
      where
        vk = Crypto.toVerification sk

    SigningKeyShelley sk -> PaymentVerificationKeyShelley (Shelley.VKey vk)
      where
        vk = deriveVerKeyDSIGN sk

getStakingVerificationKey :: SigningKey -> StakingVerificationKey
getStakingVerificationKey sk =
  case sk of
    SigningKeyByron _ -> panic "Cardano.Api.getPaymentVerificationKey: SigningKeyByron"

    SigningKeyShelley sks -> StakingVerificationKeyShelley (Shelley.VKey vk)
      where
        vk = deriveVerKeyDSIGN sks

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
  -> Maybe Update
  -- ^ Update proposals
  -> TxUnsigned
buildShelleyTransaction txins txouts ttl fee certs pParamUpdate = do
  let relevantCerts = [ certDiscrim c | c <- certs ]
  TxUnsignedShelley $
    Shelley.TxBody
      (Set.fromList (map toShelleyTxIn  txins))
      (Seq.fromList (map toShelleyTxOut txouts))
      (Seq.fromList relevantCerts)  -- certificates
      (Shelley.Wdrl Map.empty)      -- withdrawals
      (toShelleyLovelace fee)
      ttl
      (toStrictMaybe pParamUpdate)
      Shelley.SNothing              -- metadata hash
 where
   certDiscrim :: Certificate -> ShelleyCertificate
   certDiscrim (ShelleyDelegationCertificate delegCert) = delegCert
   certDiscrim (ShelleyStakePoolCertificate sPoolCert) = sPoolCert
   certDiscrim (ShelleyGenesisDelegationCertificate sGenDelegCert) = sGenDelegCert
   certDiscrim (ShelleyMIRCertificate mirCert) = mirCert

   toStrictMaybe :: Maybe Update ->  Shelley.StrictMaybe ShelleyUpdate
   toStrictMaybe (Just (ShelleyUpdate shellyUp)) = Shelley.SJust shellyUp
   toStrictMaybe Nothing = Shelley.SNothing

-- | Calculate the minimum fee of a Shelley transaction.
calculateShelleyMinFee :: Shelley.PParams -> TxSigned -> Lovelace
calculateShelleyMinFee _ (TxSignedByron _ _ _ _) =
  panic "calculateShelleyMinFee: TxSignedByron"
calculateShelleyMinFee pparams (TxSignedShelley tx) =
  Lovelace . fromIntegral $ Shelley.minfee pparams tx

-- | Build a dummy Shelley transaction to be used for the minimum fee
-- calculation.
buildDummyShelleyTxForFeeCalc
  :: Int
  -- ^ The number of dummy transaction inputs to use.
  -> Int
  -- ^ The number of dummy transaction outputs to use.
  -> SlotNo
  -- ^ The transaction TTL.
  -> Network
  -> [SigningKey]
  -> [Certificate]
  -> TxSigned
buildDummyShelleyTxForFeeCalc txInCount txOutCount ttl network skeys certs =
    signTransaction (buildShelleyTransaction txIns txOuts ttl fee certs Nothing) network skeys
  where
    vkey :: PaymentVerificationKey
    vkey =
      maybe
        (panic "buildDummyShelleyTxForFeeCalc: No signing keys provided.")
        getPaymentVerificationKey
        (headMay skeys)

    addr :: Address
    addr = shelleyVerificationKeyAddress network vkey Nothing

    txIns :: [TxIn]
    txIns = map (mkTxIn . mkTxId) [0..txInCount - 1]

    mkTxIn :: TxId -> TxIn
    mkTxIn = (flip TxIn) 0

    mkTxId :: Int -> TxId
    mkTxId = TxId . coerce . Crypto.hash @Crypto.Blake2b_256

    txOuts :: [TxOut]
    txOuts = replicate txOutCount (TxOut addr (Lovelace 0))

    fee :: Lovelace
    fee = Lovelace 0

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
    sig = Shelley.signedDSIGN @Shelley.TPraosStandardCrypto sk (Shelley.hashTxBody txbody)


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

