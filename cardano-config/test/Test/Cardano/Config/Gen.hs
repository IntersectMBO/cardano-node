{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Config.Gen
  ( genAddress
  , genGenesisDelegationPair
  , genGenesisFundPair
  , genKESKeyPair
  , genKeyRole
  , genNetworkTopology
  , genNodeAddress
  , genNodeHostAddress
  , genNodeSetup
  , genShelleyGenesis
  , genSigningKey
  , genTextView
  , genVRFKeyPair
  ) where

import           Cardano.Prelude

import           Cardano.Config.Shelley.KES (SignKey, VerKey)
import           Cardano.Config.Shelley.Genesis
import           Cardano.Config.TextView
import           Cardano.Config.Topology
import           Cardano.Config.Types
import           Cardano.Crypto.VRF.Simple (SimpleVRF)
import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Crypto.VRF.Class
import           Cardano.Crypto.KES.Class
import           Cardano.Config.Shelley.ColdKeys (KeyRole (..), OperatorKeyRole (..))
import           Crypto.Random (drgNewTest, withDRG)

import qualified Data.Map.Strict as Map
import qualified Data.IP as IP
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Hedgehog (Gen)
import           Hedgehog.Corpus (cooking)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Internal.Gen ()

import           Ouroboros.Consensus.BlockchainTime (SlotLength (..), SystemStart (..),
                    slotLengthFromMillisec)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Protocol (Crypto, TPraosStandardCrypto)
import           Ouroboros.Consensus.Shelley.Node (emptyGenesisStaking)
import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Shelley.Spec.Ledger.Address (toAddr)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys (GenKeyHash, KeyHash, Hash, SKey (..),
                    SignKeyVRF, VerKeyVRF, hashKeyVRF,
                    VKey, VKeyGenesis, pattern KeyPair,
                    pattern VKey, pattern VKeyGenesis, hashKey, sKey, vKey,
                    VKeyES(..), SKeyES(..))
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.TxData (Addr)

import           Test.Cardano.Crypto.Gen (genProtocolMagicId)
import           Test.Cardano.Config.Orphans ()

genShelleyGenesis :: Gen (ShelleyGenesis TPraosStandardCrypto)
genShelleyGenesis =
  ShelleyGenesis
    <$> fmap SystemStart genUTCTime
    <*> genNetworkMagic
    <*> genProtocolMagicId
    <*> fmap (realToFrac . getSlotLength) genSlotLength
    <*> genNeatUnitDouble
    <*> fmap SecurityParam (Gen.word64 $ Range.linear 1 1000000)
    <*> fmap (EpochSize . maxRollbacks) genSecurityParam
    <*> Gen.word64 (Range.linear 1 100000)
    <*> Gen.word64 (Range.linear 1 100000)
    <*> genSlotLength
    <*> Gen.word64 (Range.linear 1 100000)
    <*> fmap fromIntegral (Gen.word $ Range.linear 0 100000)
    <*> Gen.word64 (Range.linear 1 100000)
    <*> fmap fromIntegral (Gen.word $ Range.linear 100 1000000)
    <*> fmap fromIntegral (Gen.word $ Range.linear 100 1000000)
    <*> fmap Map.fromList genGenesisDelegationList
    <*> fmap Map.fromList genFundsList
    <*> pure emptyGenesisStaking


genGenesisFundPair :: Gen (Addr TPraosStandardCrypto, Coin)
genGenesisFundPair =
  (,) <$> genAddress <*> genCoin

genFundsList :: Gen [(Addr TPraosStandardCrypto, Coin)]
genFundsList = Gen.list (Range.linear 1 100) genGenesisFundPair

genGenesisDelegationList :: Gen [(GenKeyHash TPraosStandardCrypto, KeyHash TPraosStandardCrypto)]
genGenesisDelegationList = Gen.list (Range.linear 1 10) genGenesisDelegationPair

genGenesisDelegationPair :: Gen (GenKeyHash TPraosStandardCrypto, KeyHash TPraosStandardCrypto)
genGenesisDelegationPair =
  -- GenKeyHash should refer to the hash of the genesis verification key
  -- KeyHash should refer to the hash of the delegators verification key
  (,) <$> genGenesisKeyHash
      <*> genKeyHash


genGenesisKeyPair :: Crypto crypto => Gen (SKey crypto, VKeyGenesis crypto)
genGenesisKeyPair = mkGenKeyPair <$> genSeed5

genGenesisKeyHash :: Gen (GenKeyHash TPraosStandardCrypto)
genGenesisKeyHash = hashKey . snd <$> genGenesisKeyPair

genKESKeyPair ::  Gen (VerKey, SignKey)
genKESKeyPair = do
  duration <- Gen.integral $ Range.constant 0 1000
  seed <- genSeed5
  pure $ fst $ withDRG (drgNewTest seed) $ do
    sk <- genKeyKES duration
    let vk = deriveVerKeyKES sk
    pure (VKeyES vk, SKeyES sk)


genKeyHash :: Gen (KeyHash TPraosStandardCrypto)
genKeyHash = hashKey . snd <$> genKeyPair

genKeyPair :: Crypto crypto => Gen (SKey crypto, VKey crypto)
genKeyPair = mkKeyPair <$> genSeed5

genKeyRole :: Gen KeyRole
genKeyRole =
  Gen.element
    [ GenesisKey
    , GenesisUTxOKey
    , OperatorKey GenesisDelegateKey
    , OperatorKey StakePoolOperatorKey
    ]

genNetworkTopology :: Gen NetworkTopology
genNetworkTopology =
  Gen.choice
    [ MockNodeTopology <$> Gen.list (Range.linear 0 10) genNodeSetup
    , RealNodeTopology <$> Gen.list (Range.linear 0 10) genRemoteAddress
    ]

genSigningKey :: Crypto crypto => Gen (SKey crypto)
genSigningKey = fst <$> genKeyPair

genTextView :: Gen TextView
genTextView =
  TextView
    <$> fmap TextViewType (Gen.utf8 (Range.linear 1 20) Gen.alpha)
    <*> fmap TextViewTitle (Gen.utf8 (Range.linear 1 80) (Gen.filter (/= '\n') Gen.ascii))
    <*> Gen.bytes (Range.linear 0 500)


_genVRFKeyHash :: Gen (Hash (HASH TPraosStandardCrypto) (VerKeyVRF SimpleVRF))
_genVRFKeyHash = (hashKeyVRF @TPraosStandardCrypto . snd) <$> genVRFKeyPair

genVRFKeyPair :: Gen (SignKeyVRF SimpleVRF, VerKeyVRF SimpleVRF)
genVRFKeyPair = do
  seed <- genSeed5
  pure $ fst $ withDRG (drgNewTest seed) $ do
     sk <- genKeyVRF
     let vk = deriveVerKeyVRF sk
     pure (sk, vk)

-- -------------------------------------------------------------------------------------------------

-- | Generate a deterministic genesis key pair given a seed.
mkGenKeyPair :: Crypto crypto => (Word64, Word64, Word64, Word64, Word64) -> (SKey crypto, VKeyGenesis crypto)
mkGenKeyPair seed =
  fst . withDRG (drgNewTest seed) $ do
    sk <- genKeyDSIGN
    return (SKey sk, VKeyGenesis $ deriveVerKeyDSIGN sk)

-- | Generate a deterministic key pair given a seed.
mkKeyPair :: Crypto crypto => (Word64, Word64, Word64, Word64, Word64) -> (SKey crypto, VKey crypto)
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKey $ deriveVerKeyDSIGN sk)

genAddress :: Gen (Addr TPraosStandardCrypto)
genAddress = do
  (secKey1, verKey1) <- genKeyPair
  (secKey2, verKey2) <- genKeyPair
  let keyPair1 = KeyPair {sKey = secKey1, vKey = verKey1}
      keyPair2 = KeyPair {sKey = secKey2, vKey = verKey2}
  pure $ toAddr (keyPair1, keyPair2)


genCoin :: Gen Coin
genCoin =
  Coin <$> Gen.integral (Range.linear 1 1000000000)

genNeatUnitDouble :: Gen Double
genNeatUnitDouble =
  (\x -> fromIntegral x / 1000) <$> Gen.int (Range.linear 0 1000)

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic =
  NetworkMagic <$> Gen.enumBounded

genNodeAddress :: Gen NodeAddress
genNodeAddress =
  NodeAddress
    <$> Gen.maybe genNodeHostAddress
    <*> fmap fromIntegral (Gen.word16 $ Range.linear 100 20000)

genNodeHostAddress :: Gen NodeHostAddress
genNodeHostAddress =
  NodeHostAddress
    <$> Gen.choice
          [ IP.IPv4 . IP.toIPv4w <$> Gen.enumBounded
          , IP.IPv6 . IP.toIPv6w <$> genFourWord32
          ]
  where
    genFourWord32 :: Gen (Word32, Word32, Word32, Word32)
    genFourWord32 =
       (,,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.enumBounded <*> Gen.enumBounded

genNodeSetup :: Gen NodeSetup
genNodeSetup =
  NodeSetup
    <$> Gen.word64 (Range.linear 0 10000)
    <*> genNodeAddress
    <*> Gen.list (Range.linear 0 6) genRemoteAddress

genRemoteAddress :: Gen RemoteAddress
genRemoteAddress =
  RemoteAddress
    <$> Gen.element cooking
    <*> fmap fromIntegral (Gen.word16 $ Range.linear 100 20000)
    <*> Gen.int (Range.linear 0 100)

genSecurityParam :: Gen SecurityParam
genSecurityParam =
  SecurityParam <$> Gen.word64 (Range.linear 1 20000)

genSeed5 :: Gen (Word64, Word64, Word64, Word64, Word64)
genSeed5 =
  (,,,,)
    <$> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded

genSlotLength :: Gen SlotLength
genSlotLength =
  slotLengthFromMillisec <$> Gen.integral (Range.linear 1 60000)

genUTCTime :: Gen UTCTime
genUTCTime =
  posixSecondsToUTCTime . realToFrac <$> Gen.int (Range.linear 1000000000 5000000000)
