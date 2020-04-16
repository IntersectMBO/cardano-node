{-# LANGUAGE PatternSynonyms #-}

module Test.Cardano.Config.Gen
  ( genShelleyGenesis
  ) where

import           Cardano.Prelude

import           Cardano.Config.ShelleyGenesis
import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Crypto.Random (drgNewTest, withDRG)

import qualified Data.Map.Strict as Map

import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Ouroboros.Consensus.BlockchainTime (SlotLength (..), SystemStart (..),
                    slotLengthFromMillisec)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Protocol (Crypto, TPraosStandardCrypto)
import           Ouroboros.Network.Magic (NetworkMagic (..))

-- import           Cardano.Ledger.Shelley.Crypto
import           Shelley.Spec.Ledger.Address (toAddr)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Keys as Shelley
import           Shelley.Spec.Ledger.Keys (hashKey, sKey, vKey)
import           Shelley.Spec.Ledger.TxData (Addr)



import           Test.Cardano.Crypto.Gen (genProtocolMagicId)


--import           Test.Shelley.Spec.Ledger.Utils (mkKeyPair) -- Useful starting point


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
    <*> fmap Map.fromList genGenesisDelegationList -- !(Map (SL.GenKeyHash c) (SL.KeyHash c))
    <*> fmap Map.fromList genFundsList             -- !(Map (SL.Addr c) SL.Coin)


genFundsList :: Crypto crypto => Gen [(Addr crypto, Coin)]
genFundsList = Gen.list (Range.linear 1 100) ((,) <$> genAddress <*> genCoin)

genGenesisDelegationList :: Crypto crypto => Gen [(Shelley.GenKeyHash crypto,Shelley.KeyHash crypto)]
genGenesisDelegationList = Gen.list (Range.linear 1 10) genGenesisDelegationPair

genGenesisDelegationPair :: Crypto crypto => Gen (Shelley.GenKeyHash crypto, Shelley.KeyHash crypto)
genGenesisDelegationPair = do
  -- I think SL.GenKeyHash refers to the hash of the genesis verification key
  (_, verGenKey) <- genGenesisKeyPair
  let genKeyHash = hashKey verGenKey

  -- I think SL.KeyHash refers to the hash of the delegators veritifation key
  (_, delegatorVerKey) <- genKeyPair
  let keyhash = hashKey delegatorVerKey
  pure (genKeyHash, keyhash)


genGenesisKeyPair :: Crypto crypto => Gen (Shelley.SKey crypto, Shelley.VKeyGenesis crypto)
genGenesisKeyPair = do
  [a, b, c, d, e] <- Gen.list (Range.singleton 5) (Gen.word64 Range.constantBounded)
  pure $ mkGenKeyPair (a, b, c, d, e)

genKeyPair :: Crypto crypto => Gen (Shelley.SKey crypto, Shelley.VKey crypto)
genKeyPair = do
  [a, b, c, d, e] <- Gen.list (Range.singleton 5) (Gen.word64 Range.constantBounded)
  pure $ mkKeyPair (a, b, c, d, e)

-- | Generate a deterministic genesis key pair given a seed.
mkGenKeyPair :: Crypto crypto => (Word64, Word64, Word64, Word64, Word64) -> (Shelley.SKey crypto, Shelley.VKeyGenesis crypto)
mkGenKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (Shelley.SKey sk, Shelley.VKeyGenesis $ deriveVerKeyDSIGN sk)

-- | Generate a deterministic key pair given a seed.
mkKeyPair :: Crypto crypto => (Word64, Word64, Word64, Word64, Word64) -> (Shelley.SKey crypto, Shelley.VKey crypto)
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (Shelley.SKey sk, Shelley.VKey $ deriveVerKeyDSIGN sk)

genAddress :: Crypto crypto => Gen (Addr crypto)
genAddress = do
  (secKey1, verKey1) <- genKeyPair
  (secKey2, verKey2) <- genKeyPair
  let keyPair1 = Shelley.KeyPair {sKey = secKey1, vKey = verKey1}
      keyPair2 = Shelley.KeyPair {sKey = secKey2, vKey = verKey2}
  pure . toAddr $ (keyPair1, keyPair2)


genCoin :: Gen Coin
genCoin =
  Coin <$> Gen.integral (Range.linear 1 1000000000)

genNeatUnitDouble :: Gen Double
genNeatUnitDouble =
  (\x -> fromIntegral x / 1000) <$> Gen.int (Range.linear 0 1000)

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic =
  NetworkMagic <$> Gen.enumBounded

genSecurityParam :: Gen SecurityParam
genSecurityParam =
  SecurityParam <$> Gen.word64 (Range.linear 1 20000)

genSlotLength :: Gen SlotLength
genSlotLength =
  slotLengthFromMillisec <$> Gen.integral (Range.linear 1 60000)

genUTCTime :: Gen UTCTime
genUTCTime =
  posixSecondsToUTCTime . realToFrac <$> Gen.int (Range.linear 1000000000 5000000000)
