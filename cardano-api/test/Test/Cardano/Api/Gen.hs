{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Api.Gen
  ( genAddress
  , genCertificate
  , genGenesisVerificationKey
  , genNetwork
  , genNetworkMagic
  , genPaymentVerificationKey
  , genSigningKey
  , genSigningKeyByron
  , genSigningKeyShelley
  , genTxIn
  , genTxOut
  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  , genStakingVerificationKey
  , genUpdate
  , genVerificationKeyAddressByron
  , genVerificationKeyAddressShelley
  , genVerificationKeyShelleyStakePool
  , genVerificationKeyShelleyStaking
  , genVRFKeyPair
  , genSeed
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import qualified Cardano.Crypto as Byron
import           Cardano.Crypto.DSIGN
import           Cardano.Crypto.VRF.Class (deriveVerKeyVRF, genKeyVRF, seedSizeVRF)
import           Cardano.Prelude

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import           Cardano.Crypto.Seed as Crypto

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Ratio (approxRational)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set


import           Shelley.Spec.Ledger.BaseTypes (Nonce (..),StrictMaybe (..),
                    UnitInterval(..), maybeToStrictMaybe, mkNonce,
                    mkUnitInterval, textToDns, textToUrl)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Crypto hiding (Network (..))
import           Shelley.Spec.Ledger.Keys (KeyHash, SignKeyVRF, VerKeyVRF,
                   VKey(..), hash, hashKey)
import           Shelley.Spec.Ledger.PParams (PParamsUpdate,
                   PParams'(..), ProtVer(..))
import           Shelley.Spec.Ledger.Slot (EpochNo(..))
import           Shelley.Spec.Ledger.TxData
                   (PoolMetaData (..), RewardAcnt (..), StakePoolRelay (..),
                    pattern TxBody, Wdrl (..))

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Test.Cardano.Chain.UTxO.Gen (genTx)
import qualified Test.Cardano.Crypto.Gen as Byron

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genAddress :: Gen Address
genAddress =
  Gen.choice
    [ genVerificationKeyAddressShelley
    -- TODO: Uncomment the line below once `Shelley.Spec.Ledger.Address.getByron`
    --       has been implemented and doesn't just `panic`.
    -- , genVerificationKeyAddressByron
    ]

genCertificate :: Gen Certificate
genCertificate = do
  Gen.choice
    [ -- Stake Key related
      genStakingKeyRegistrationCert
    , genStakingKeyDeregistrationCert
    , genStakingKeyDelegationCert
      -- Stake pool related
    , genShelleyStakePoolRegistrationCertificate
    , genShelleyStakePoolRetirementCertificate
    ]

genCredentialShelley :: Gen ShelleyCredentialStaking
genCredentialShelley = do
  vKeyHash <- genVerificationKeyHashStakingShelley
  return $ mkShelleyStakingCredential vKeyHash

genEpochNoShelly :: Gen EpochNo
genEpochNoShelly = do
  slot <- Gen.word64 (Range.linear minBound maxBound)
  return $ EpochNo slot

genKeyHash :: Gen (KeyHash krole TPraosStandardCrypto)
genKeyHash = hashKey . snd <$> genKeyPair

-- | Generate a deterministic key pair given a seed.
genKeyPair :: Gen (SignKeyDSIGN (DSIGN TPraosStandardCrypto),
                   VKey krole TPraosStandardCrypto)
genKeyPair = do
    seed <- genSeed seedSize
    let sk = genKeyDSIGN seed
        vk = deriveVerKeyDSIGN sk
    pure (sk, VKey vk)
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeDSIGN (Proxy :: Proxy (DSIGN TPraosStandardCrypto)))


genStakingKeyRegistrationCert :: Gen Certificate
genStakingKeyRegistrationCert = do
  vKeyHash <- genVerificationKeyHashStakingShelley
  pure $ shelleyRegisterStakingAddress vKeyHash

genStakingKeyDeregistrationCert :: Gen Certificate
genStakingKeyDeregistrationCert = do
  vKeyHash <- genVerificationKeyHashStakingShelley
  pure $ shelleyDeregisterStakingAddress vKeyHash

genStakingKeyDelegationCert :: Gen Certificate
genStakingKeyDelegationCert = do
  delegator_vKeyHash <- genVerificationKeyHashStakingShelley
  delegatee_vKeyHash <- genVerificationKeyHashStakePoolShelley

  pure $ shelleyDelegateStake delegator_vKeyHash delegatee_vKeyHash

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genNetworkMagic
    ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic =
  NetworkMagic <$> Gen.word32 Range.constantBounded

genRewardAccountShelley :: Gen ShelleyRewardAccount
genRewardAccountShelley = do
  cred <- genCredentialShelley
  return $ RewardAcnt cred

genSigningKey :: Gen SigningKey
genSigningKey =
  Gen.choice
    [ genSigningKeyByron
    , genSigningKeyShelley
    ]

genSigningKeyByron :: Gen SigningKey
genSigningKeyByron =
  SigningKeyByron <$> Byron.genSigningKey

genSigningKeyShelley :: Gen SigningKey
genSigningKeyShelley = do
  seed <- genSeed seedSize
  let sk = genKeyDSIGN seed
  return (SigningKeyShelley sk)
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeDSIGN (Proxy :: Proxy (DSIGN TPraosStandardCrypto)))

genShelleyStakePoolRegistrationCertificate :: Gen Certificate
genShelleyStakePoolRegistrationCertificate =
  shelleyRegisterStakePool
    <$> genVerificationKeyHashStakePoolShelley
    <*> genVRFVerificationKeyHashShelley
    <*> (Coin <$> Gen.integral (Range.linear 0 10000000000))
    <*> (Coin <$> Gen.integral (Range.linear 0 10000000000))
    <*> genStakePoolMarginShelley
    <*> genRewardAccountShelley
    <*> genStakePoolOwnersShelley
    <*> Gen.list (Range.linear 1 5) genStakePoolRelayShelley
    <*> Gen.maybe genPoolMetaDataShelley

-- TODO: Improve this generator.
genPoolMetaDataShelley :: Gen ShelleyStakePoolMetaData
genPoolMetaDataShelley = do
  let textGen = Gen.text (Range.constant 1 20) Gen.alpha
  url <- Gen.just (textToUrl <$> textGen)
  return $ PoolMetaData url "Test"

-- TODO: Cover SingleHostName and MultiHostName data constructors
-- It also seems impossible to construct the `Port` type as the
-- constructors aren't exposed nor a function to create one.
genStakePoolRelayShelley :: Gen ShelleyStakePoolRelay
genStakePoolRelayShelley = do
  --port <- Gen.word16 (Range.linear minBound maxBound)
  let textGen = Gen.text (Range.constant 1 20) Gen.alpha
  dns <- Gen.just (textToDns <$> textGen)
  return $ SingleHostName SNothing dns

genShelleyStakePoolRetirementCertificate :: Gen Certificate
genShelleyStakePoolRetirementCertificate = do
  vKey <- genVerificationKeyShelleyStakePool
  epochNo <- genEpochNoShelly
  return $ shelleyRetireStakePool vKey epochNo

genStakePoolOwnersShelley :: Gen ShelleyStakePoolOwners
genStakePoolOwnersShelley = do
  keyHashes <- Gen.list (Range.linear 1 5) genVerificationKeyHashStakingShelley
  return $ Set.fromList keyHashes

-- TODO: Should make sure this covers
-- the 0 to 1 range adequately.
genStakePoolMarginShelley :: Gen UnitInterval
genStakePoolMarginShelley = do
  numerator' <- Gen.double (Range.constantFrom 0 0 1)
  Gen.just . pure $ mkUnitInterval $ approxRational numerator' 1

genTxIn :: Gen TxIn
genTxIn =
  TxIn <$> genFakeTxId <*> Gen.word (Range.linear 0 10000)

genTxOut :: Gen TxOut
genTxOut =
  TxOut <$> genAddress <*> genLovelace

genTxSigned :: Gen TxSigned
genTxSigned =
  Gen.choice
    [ genTxSignedByron
    , genTxSignedShelley
    ]

genTxSignedByron :: Gen TxSigned
genTxSignedByron =
  signTransaction
    <$> genTxUnsignedByron
    <*> genNetwork
    <*> Gen.list (Range.linear 1 5) genSigningKeyByron

genTxSignedShelley :: Gen TxSigned
genTxSignedShelley =
  signTransaction
    <$> genTxUnsignedShelley
    <*> genNetwork
    <*> Gen.list (Range.linear 1 5) genSigningKeyShelley

genTxUnsigned :: Gen TxUnsigned
genTxUnsigned =
  Gen.choice
    [ genTxUnsignedByron
    , genTxUnsignedShelley
    ]

genTxUnsignedByron :: Gen TxUnsigned
genTxUnsignedByron = do
  tx <- genTx
  let cbor = serialize tx
  pure $ TxUnsignedByron tx (LBS.toStrict cbor) (coerce $ Byron.hashRaw cbor)

genTxUnsignedShelley :: Gen TxUnsigned
genTxUnsignedShelley =
    TxUnsignedShelley <$> genTxBodyShelley
  where
    -- TODO: Improve this incredibly naive generator.
    genTxBodyShelley :: Gen ShelleyTxBody
    genTxBodyShelley = do
      coin <- Gen.integral (Range.linear 0 10000000000)
      slot <- Gen.word64 (Range.linear minBound maxBound)
      pure $ TxBody (Set.fromList []) (StrictSeq.fromList []) (StrictSeq.fromList [])
                (Wdrl $ Map.fromList []) (Coin coin) (SlotNo slot) SNothing SNothing


genUpdate :: Gen Update
genUpdate = do
  eNo <- genEpochNoShelly
  pps <- genPParamsUpdate
  gKeyHashes <- Gen.list (Range.linear 1 8) genKeyHash

  return . ShelleyUpdate $ createShelleyUpdateProposal eNo gKeyHashes pps

genNonce :: Gen Nonce
genNonce =
  Gen.choice
    [ mkNonce <$> genNatural (Range.linear 1 123)
    , pure NeutralNonce
    ]

genPParamsUpdate :: Gen PParamsUpdate
genPParamsUpdate =
  PParams
    <$> (genStrictMaybe $ genNatural (Range.linear 0 1000))
    <*> (genStrictMaybe $ genNatural (Range.linear 0 3))
    <*> (genStrictMaybe $ fmap fromIntegral (Gen.word $ Range.linear 100 1000000))
    <*> (genStrictMaybe $ fmap fromIntegral (Gen.word $ Range.linear 100 1000000))
    <*> (genStrictMaybe $ fmap fromIntegral (Gen.word $ Range.linear 100 1000000))
    <*> genStrictMaybe genShelleyCoin
    <*> genStrictMaybe genUnitInterval
    <*> genStrictMaybe genRational
    <*> genStrictMaybe genShelleyCoin
    <*> genStrictMaybe genUnitInterval
    <*> genStrictMaybe genRational
    <*> genStrictMaybe genEpochNoShelly
    <*> (genStrictMaybe $ genNatural (Range.linear 0 10))
    <*> genStrictMaybe genRational
    <*> genStrictMaybe genUnitInterval
    <*> genStrictMaybe genUnitInterval
    <*> genStrictMaybe genUnitInterval
    <*> genStrictMaybe genNonce
    <*> genStrictMaybe genProtVer
    <*> genStrictMaybe genMinUTxOValue

genMinUTxOValue :: Gen Natural
genMinUTxOValue = Gen.integral (Range.linear 1 1000)

genStrictMaybe :: Gen a -> Gen (StrictMaybe a)
genStrictMaybe gen = maybeToStrictMaybe <$> Gen.maybe gen

genNatural :: Range Natural -> Gen Natural
genNatural range = Gen.integral range

genProtVer :: Gen ProtVer
genProtVer =
  ProtVer
    <$> genNatural (Range.linear 0 1000)
    <*> genNatural (Range.linear 0 1000)

genRational :: Gen Rational
genRational = Gen.realFrac_ (Range.linearFrac 0 10000)

genShelleyCoin :: Gen Coin
genShelleyCoin =  Coin <$> Gen.integral (Range.linear 1 1000000000)

genUnitInterval :: Gen UnitInterval
genUnitInterval =
  UnsafeUnitInterval
    <$> Gen.realFrac_ (Range.linearFrac 0 1)

genGenesisVerificationKey :: Gen GenesisVerificationKey
genGenesisVerificationKey = genGenesisVerificationKeyShelley

genGenesisVerificationKeyShelley :: Gen GenesisVerificationKey
genGenesisVerificationKeyShelley =
  getGenesisVerificationKey <$> genSigningKeyShelley

genPaymentVerificationKey :: Gen PaymentVerificationKey
genPaymentVerificationKey =
  Gen.choice
    [ genPaymentVerificationKeyByron
    , genPaymentVerificationKeyShelley
    ]

genStakingVerificationKey :: Gen StakingVerificationKey
genStakingVerificationKey =
  getStakingVerificationKey <$> genSigningKeyShelley

genVerificationKeyAddressByron :: Gen Address
genVerificationKeyAddressByron =
  byronVerificationKeyAddress <$> genPaymentVerificationKeyByron <*> genNetwork

genVerificationKeyAddressShelley :: Gen Address
genVerificationKeyAddressShelley =
  shelleyVerificationKeyAddress
    <$> genPaymentVerificationKeyShelley
    <*> Gen.choice
          [ pure Nothing
          , Just <$> genStakingVerificationKey
          ]

genPaymentVerificationKeyByron :: Gen PaymentVerificationKey
genPaymentVerificationKeyByron =
  getPaymentVerificationKey <$> genSigningKeyByron

genPaymentVerificationKeyShelley :: Gen PaymentVerificationKey
genPaymentVerificationKeyShelley =
  getPaymentVerificationKey <$> genSigningKeyShelley

genVerificationKeyShelleyStakePool :: Gen ShelleyVerificationKeyStakePool
genVerificationKeyShelleyStakePool = do
  (SigningKeyShelley sKey) <- genSigningKeyShelley
  return . VKey $ deriveVerKeyDSIGN sKey

genVerificationKeyShelleyStaking :: Gen ShelleyVerificationKeyStaking
genVerificationKeyShelleyStaking = do
  (SigningKeyShelley sKey) <- genSigningKeyShelley
  return . VKey $ deriveVerKeyDSIGN sKey

genVerificationKeyHashStakePoolShelley :: Gen ShelleyVerificationKeyHashStakePool
genVerificationKeyHashStakePoolShelley = do
  vk <- genVerificationKeyShelleyStakePool
  return $ hashKey vk

genVerificationKeyHashStakingShelley :: Gen ShelleyVerificationKeyHashStaking
genVerificationKeyHashStakingShelley = do
  vk <- genVerificationKeyShelleyStaking
  return $ hashKey vk

genVRFKeyPair :: Gen (SignKeyVRF TPraosStandardCrypto, VerKeyVRF TPraosStandardCrypto)
genVRFKeyPair = do
    seed <- genSeed seedSize
    let sk = genKeyVRF seed
        vk = deriveVerKeyVRF sk
    return (sk, vk)
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeVRF (Proxy :: Proxy (VRF TPraosStandardCrypto)))


genVRFVerificationKeyHashShelley :: Gen ShelleyVRFVerificationKeyHash
genVRFVerificationKeyHashShelley = do
  (_, vrfVKey) <- genVRFKeyPair
  return $ hash vrfVKey

-- -------------------------------------------------------------------------------------------------

-- Generates a fake TxId by applying the right hashing function to a random ByteString.
genFakeTxId :: Gen TxId
genFakeTxId =
  TxId . {- Crypto. -} hashRaw identity <$> Gen.bytes (Range.linear 10 50)

-- This name will clash with one in Cardano.Crypto.Hash.Class.
-- This should be removed (or maybe specialized) then the one in Cardano.Crypto.Hash.Class is
-- available.
hashRaw :: (a -> ByteString) -> a -> Crypto.Hash Crypto.Blake2b_256 ()
hashRaw serialise = Crypto.UnsafeHash . Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) . serialise

genLovelace :: Gen Lovelace
genLovelace =
  Lovelace <$> Gen.integral (Range.linear 1 999999999999)

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)
