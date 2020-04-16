module Test.Cardano.Config.Gen
  ( genShelleyGenesis
  ) where

import           Cardano.Prelude

import           Cardano.Config.ShelleyGenesis
import           Cardano.Slotting.Slot (EpochSize (..))

import qualified Data.Map.Strict as Map

import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Ouroboros.Consensus.BlockchainTime (SlotLength (..), SystemStart (..),
                    slotLengthFromMillisec)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.TxData (Addr)



import           Test.Cardano.Crypto.Gen (genProtocolMagicId)



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
    <*> pure mempty                         -- !(Map (SL.GenKeyHash c) (SL.KeyHash c))
    <*> fmap Map.fromList genFundsList      -- !(Map (SL.Addr c) SL.Coin)




genFundsList :: Gen [(Addr a, Coin)]
genFundsList =
    Gen.list (Range.linear 1 100) ((,) <$> genAddress <*> genCoin)


genAddress :: Gen (Addr a)
genAddress =

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
