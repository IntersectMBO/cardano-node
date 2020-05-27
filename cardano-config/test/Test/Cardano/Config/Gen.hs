{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

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

import           Cardano.Config.Shelley.Genesis
import           Cardano.Config.TextView
import           Cardano.Config.Topology
import           Cardano.Config.Types

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.VRF.Class
import           Cardano.Config.Shelley.ColdKeys (KeyRole (..), OperatorKeyRole (..))

import qualified Data.Map.Strict as Map
import qualified Data.IP as IP
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Hedgehog (Gen)
import           Hedgehog.Corpus (cooking)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Range (Range)
import           Hedgehog.Internal.Gen ()

import           Ouroboros.Consensus.BlockchainTime (SlotLength (..), SystemStart (..),
                    slotLengthFromMillisec)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Consensus.Shelley.Node (emptyGenesisStaking)
import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Shelley.Spec.Ledger.Address (Addr, toAddr)
import           Shelley.Spec.Ledger.BaseTypes (Nonce (..), Network (..), UnitInterval (..),
                    mkNonce)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys
                   (KeyHash, KeyPair(..), VKey(..), hashKey)
import qualified Shelley.Spec.Ledger.Keys as Ledger (KeyRole(..))
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.PParams (PParams, PParams' (..), ProtVer (..))

import           Test.Cardano.Crypto.Gen (genProtocolMagicId)


genShelleyGenesis :: Gen (ShelleyGenesis TPraosStandardCrypto)
genShelleyGenesis =
  ShelleyGenesis
    <$> fmap SystemStart genUTCTime
    <*> genNetworkMagic
    <*> Gen.element [Mainnet, Testnet]
    <*> genProtocolMagicId
    <*> fmap (realToFrac . getSlotLength) genSlotLength
    <*> fmap SecurityParam (Gen.word64 $ Range.linear 1 1000000)
    <*> fmap (EpochSize . maxRollbacks) genSecurityParam
    <*> Gen.word64 (Range.linear 1 100000)
    <*> Gen.word64 (Range.linear 1 100000)
    <*> genSlotLength
    <*> Gen.word64 (Range.linear 1 100000)
    <*> fmap fromIntegral (Gen.word $ Range.linear 0 100000)
    <*> Gen.word64 (Range.linear 1 100000)
    <*> genPParams
    <*> fmap Map.fromList genGenesisDelegationList
    <*> fmap Map.fromList genFundsList
    <*> pure emptyGenesisStaking


genPParams :: Gen PParams
genPParams =
  PParams
    <$> genNatural (Range.linear 0 1000)
    <*> genNatural (Range.linear 0 3)
    <*> fmap fromIntegral (Gen.word $ Range.linear 100 1000000)
    <*> fmap fromIntegral (Gen.word $ Range.linear 100 1000000)
    <*> fmap fromIntegral (Gen.word $ Range.linear 100 1000000)
    <*> genCoin
    <*> genUnitInterval
    <*> genRational
    <*> genCoin
    <*> genUnitInterval
    <*> genRational
    <*> genEpochNo
    <*> genNatural (Range.linear 0 10)
    <*> genRational
    <*> genUnitInterval
    <*> genUnitInterval
    <*> genUnitInterval
    <*> genNonce
    <*> genProtVer
    <*> genMinUTxOValue

genNatural :: Range Natural -> Gen Natural
genNatural range = Gen.integral range

genRational :: Gen Rational
genRational = Gen.realFrac_ (Range.linearFrac 0 10000)

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> Gen.word64 (Range.linear 0 500)

genMinUTxOValue :: Gen Natural
genMinUTxOValue = Gen.integral (Range.linear 1 1000)

genNonce :: Gen Nonce
genNonce =
  Gen.choice
    [ mkNonce <$> genNatural (Range.linear 1 123)
    , pure NeutralNonce
    ]

genProtVer :: Gen ProtVer
genProtVer =
  ProtVer
    <$> genNatural (Range.linear 0 1000)
    <*> genNatural (Range.linear 0 1000)

genUnitInterval :: Gen UnitInterval
genUnitInterval =
  UnsafeUnitInterval
    <$> Gen.realFrac_ (Range.linearFrac 0 1)

genGenesisFundPair :: Gen (Addr TPraosStandardCrypto, Coin)
genGenesisFundPair =
  (,) <$> genAddress <*> genCoin

genFundsList :: Gen [(Addr TPraosStandardCrypto, Coin)]
genFundsList = Gen.list (Range.linear 1 100) genGenesisFundPair


genGenesisDelegationList :: Gen [(KeyHash Ledger.Genesis         TPraosStandardCrypto,
                                  KeyHash Ledger.GenesisDelegate TPraosStandardCrypto)]
genGenesisDelegationList = Gen.list (Range.linear 1 10) genGenesisDelegationPair

genGenesisDelegationPair :: Gen (KeyHash Ledger.Genesis         TPraosStandardCrypto,
                                 KeyHash Ledger.GenesisDelegate TPraosStandardCrypto)
genGenesisDelegationPair = (,) <$> genKeyHash <*> genKeyHash


genKESKeyPair :: forall k. KESAlgorithm k => Gen (VerKeyKES k, SignKeyKES k)
genKESKeyPair = do
    seed <- genSeed seedSize
    let sk = genKeyKES seed
        vk = deriveVerKeyKES sk
    pure (vk, sk)
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeKES (Proxy :: Proxy k))


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

genSigningKey :: Gen (SignKeyDSIGN (DSIGN TPraosStandardCrypto))
genSigningKey = fst <$> genKeyPair

genTextView :: Gen TextView
genTextView =
  TextView
    <$> fmap TextViewType (Gen.utf8 (Range.linear 1 20) Gen.alpha)
    <*> fmap TextViewTitle (Gen.utf8 (Range.linear 1 80) (Gen.filter (/= '\n') Gen.ascii))
    <*> Gen.bytes (Range.linear 0 500)


genVRFKeyPair :: Gen (SignKeyVRF (VRF TPraosStandardCrypto),
                      VerKeyVRF  (VRF TPraosStandardCrypto))
genVRFKeyPair = do
    seed <- genSeed seedSize
    let sk = genKeyVRF seed
        vk = deriveVerKeyVRF sk
    pure (sk, vk)
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeVRF (Proxy :: Proxy (VRF TPraosStandardCrypto)))

genSeed :: Int -> Gen Seed
genSeed n = mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

-- -------------------------------------------------------------------------------------------------

genAddress :: Gen (Addr TPraosStandardCrypto)
genAddress = do
  (secKey1, verKey1) <- genKeyPair
  (secKey2, verKey2) <- genKeyPair
  nw <- Gen.element [Mainnet, Testnet]
  let keyPair1 = KeyPair {sKey = secKey1, vKey = verKey1}
      keyPair2 = KeyPair {sKey = secKey2, vKey = verKey2}
  pure $ toAddr nw (keyPair1, keyPair2)


genCoin :: Gen Coin
genCoin =
  Coin <$> Gen.integral (Range.linear 1 1000000000)

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic =
  NetworkMagic <$> Gen.enumBounded

genNodeAddress :: Gen NodeAddress
genNodeAddress =
  NodeAddress
    <$> genNodeHostAddress
    <*> fmap fromIntegral (Gen.word16 $ Range.linear 100 20000)

genNodeHostAddress :: Gen NodeHostAddress
genNodeHostAddress =
  NodeHostAddress
    <$> Gen.choice
          [ fmap (IP.IPv4 . IP.toIPv4w) <$> Gen.maybe Gen.enumBounded
          , fmap (IP.IPv6 . IP.toIPv6w) <$> Gen.maybe genFourWord32
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

genSlotLength :: Gen SlotLength
genSlotLength =
  slotLengthFromMillisec <$> Gen.integral (Range.linear 1 60000)

genUTCTime :: Gen UTCTime
genUTCTime =
  posixSecondsToUTCTime . realToFrac <$> Gen.int (Range.linear 1000000000 5000000000)
