{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Node.LedgerEvent where

import Prelude

import           Cardano.Node.LedgerEvent

import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytes)
import qualified Codec.CBOR.Schema as CDDL
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import           Data.ByteString.Lazy(fromStrict)
import           Data.ByteString.Short (ShortByteString, toShort)
import           Data.Map (Map)
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Hedgehog (Property, discover, footnote)
import qualified Hedgehog
import qualified Hedgehog.Internal.Property as Hedgehog
import qualified Hedgehog.Extras.Test.Process as Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.FilePath ((</>))
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

specification :: Text
specification =
  unsafePerformIO $ do
    base <- either (fail . show) pure . fst =<< Hedgehog.runTestT Hedgehog.getProjectBase
    TIO.readFile $ base </> "cardano-node/test-data/ledger_events.cddl"
{-# NOINLINE specification #-}

prop_roundtrip_LedgerEvent_CBOR :: Property
prop_roundtrip_LedgerEvent_CBOR =
  Hedgehog.property $ do
    version <- Hedgehog.forAll Gen.enumBounded
    event <- Hedgehog.forAll genAnchoredEvent
    footnote ("serialized event: " <> show (Hex.encode $ serializeAnchoredEvent version event))
    Hedgehog.tripping event
      (serializeAnchoredEvent version)
      (fmap snd . deserializeAnchoredEvent . fromStrict)

prop_LedgerEvent_CDDL_conformance :: Property
prop_LedgerEvent_CDDL_conformance =
  Hedgehog.property $ do
  version <- Hedgehog.forAll Gen.enumBounded
  event <- Hedgehog.forAll genAnchoredEvent
  Hedgehog.label (labelName event)
  -- FIXME: We do want to validate full anchored events here, not just ledger events.
  -- This requires the `cddl-cat` Rust crate to support the '.cbor' control
  -- operator which should make for a straightforward and nice contribution.
  let bytes = serialize' version (ledgerEvent event)
  case CDDL.validate specification bytes of
    Right () ->
      Hedgehog.success
    Left (CDDL.ValidationError { CDDL.cbor = cbor, CDDL.hint = hint }) -> do
      Hedgehog.footnote hint
      Hedgehog.footnote cbor
      Hedgehog.failure

--
-- Generators
--

type StakePoolId = KeyHash 'StakePool StandardCrypto

type StakeCredential = Credential 'Staking StandardCrypto

genAnchoredEvent :: Hedgehog.Gen AnchoredEvent
genAnchoredEvent =
  AnchoredEvent
    <$> (At <$> genBlockHeaderHash)
    <*> genBlockHeaderHash
    <*> genSlotNo
    <*> genBlockNo
    <*> Gen.choice (mconcat
      [ fmap LedgerNewEpochEvent <$> genLedgerNewEpochEvent
      , fmap LedgerRewardUpdateEvent <$> genLedgerRewardUpdateEvent
      ])

genLedgerNewEpochEvent :: [Hedgehog.Gen (LedgerNewEpochEvent StandardCrypto)]
genLedgerNewEpochEvent =
  [ LedgerMirDist
      <$> genStakeDistribution
      <*> genStakeDistribution
      <*> genDeltaCoin
      <*> genDeltaCoin
  , LedgerPoolReaping
      <$> genEpoch
      <*> genStakePoolRefunds
      <*> genStakePoolRefunds
  , LedgerStakeDistEvent
      <$> genExtendedStakeDistribution
  , LedgerRestrainedRewards
      <$> genEpoch
      <*> genRewardDistribution
      <*> Gen.set (Range.linear 0 3) genCredential
  , LedgerTotalRewards
      <$> genEpoch
      <*> genRewardDistribution
  , LedgerTotalAdaPots
      <$> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
  ]

genLedgerRewardUpdateEvent :: [Hedgehog.Gen (LedgerRewardUpdateEvent StandardCrypto)]
genLedgerRewardUpdateEvent =
  [ LedgerIncrementalRewards
      <$> genEpoch
      <*> genRewardDistribution
  ]

genBlockHeaderHash :: Hedgehog.Gen ShortByteString
genBlockHeaderHash =
  toShort <$> Gen.bytes (Range.constant 32 32)

genCoin :: Hedgehog.Gen Coin
genCoin =
  Coin . fromIntegral <$> Gen.word32 Range.constantBounded

genCredential :: Hedgehog.Gen StakeCredential
genCredential = Gen.choice
  [ ScriptHashObj <$> genScriptHash
  , KeyHashObj <$> genKeyHash
  ]

genDeltaCoin :: Hedgehog.Gen DeltaCoin
genDeltaCoin =
  DeltaCoin <$> Gen.integral_ (Range.linear (-100) 100)

genEpoch :: Hedgehog.Gen EpochNo
genEpoch =
  fromIntegral <$> Gen.word16 Range.constantBounded

genExtendedStakeDistribution :: Hedgehog.Gen (Map StakeCredential (Coin, StakePoolId))
genExtendedStakeDistribution =
  genStakeCredentialMap $ (,) <$> genCoin <*> genKeyHash

genKeyHash :: Hedgehog.Gen (KeyHash any StandardCrypto)
genKeyHash =
  KeyHash . unsafeHashFromBytes <$> Gen.bytes (Range.singleton 28)

genReward :: Hedgehog.Gen (Reward StandardCrypto)
genReward = Reward
  <$> Gen.enumBounded
  <*> genKeyHash
  <*> genCoin

genRewardDistribution :: Hedgehog.Gen (Map StakeCredential (Set (Reward StandardCrypto)))
genRewardDistribution =
  genStakeCredentialMap $ Gen.set (Range.linear 1 3) genReward

genScriptHash :: Hedgehog.Gen (ScriptHash StandardCrypto)
genScriptHash =
  ScriptHash . unsafeHashFromBytes <$> Gen.bytes (Range.singleton 28)

genSlotNo :: Hedgehog.Gen SlotNo
genSlotNo =
  fromIntegral <$> Gen.word64 Range.constantBounded

genBlockNo :: Hedgehog.Gen BlockNo
genBlockNo =
  fromIntegral <$> Gen.int Range.constantBounded

genStakeDistribution :: Hedgehog.Gen (Map StakeCredential Coin)
genStakeDistribution =
  genStakeCredentialMap genCoin

genStakePoolRefunds :: Hedgehog.Gen (Map StakeCredential (Map StakePoolId Coin))
genStakePoolRefunds =
  genStakeCredentialMap $ Gen.map (Range.linear 1 3) $ (,) <$> genKeyHash <*> genCoin

--
-- Helpers
--

genStakeCredentialMap :: Hedgehog.Gen a -> Hedgehog.Gen (Map StakeCredential a)
genStakeCredentialMap genValue =
  Gen.map (Range.linear 0 3) ((,) <$> genCredential <*> genValue)

labelName
  :: AnchoredEvent
  -> Hedgehog.LabelName
labelName =
  fromString . T.unpack . ledgerEventName . ledgerEvent

unsafeHashFromBytes
  :: (HashAlgorithm algo)
  => ByteString
  -> Hash algo a
unsafeHashFromBytes =
  fromJust . hashFromBytes

--
-- Auto-discover tests / Template Haskell
--

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
