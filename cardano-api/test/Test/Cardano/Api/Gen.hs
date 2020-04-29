{-# LANGUAGE PatternSynonyms #-}

module Test.Cardano.Api.Gen
  ( genByronAddress
  , genGenesisKeyPairShelley
  , genGenesisShelleyVerificationKey
  , genKeyPairByron
  , genKeyPairShelley
  , genNetwork
  , genPublicKeyByron
  , genPublicKeyShelley
  , genRegularKeyPairShelley
  , genRegularShelleyVerificationKey
  , genShelleyAddress
  , genShelleyKeyDiscriminator
  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import           Cardano.Crypto (hashRaw)
import           Cardano.Crypto.DSIGN.Ed448 ()
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Prelude

import           Crypto.Random (drgNewTest, withDRG)

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Shelley.Spec.Ledger.Keys (DiscVKey (..), SKey (..), pattern VKey, VKey,
                     pattern VKeyGenesis, VKeyGenesis)
import           Shelley.Spec.Ledger.TxData (Addr)

import           Test.Cardano.Chain.UTxO.Gen (genTx)
import           Test.Cardano.Crypto.Gen (genProtocolMagicId, genSigningKey, genVerificationKey)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genByronAddress :: Gen ByronAddress
genByronAddress = byronPubKeyAddress <$> genPublicKeyByron

genShelleyAddress :: Gen (Addr TPraosStandardCrypto)
genShelleyAddress =
  mkShelleyPubKeyAddress <$> genRegularKeyPairShelley <*> genRegularKeyPairShelley

genKeyPairByron :: Gen ByronKeyPair
genKeyPairByron =
  KeyPairByron <$> genVerificationKey <*> genSigningKey

genKeyPairShelley :: Gen ShelleyKeyPair
genKeyPairShelley =
  Gen.choice
    [ genGenesisKeyPairShelley
    , genRegularKeyPairShelley
    ]

genSeed :: Gen (Word64, Word64, Word64, Word64, Word64)
genSeed =
  (,,,,)
    <$> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded

genGenesisKeyPairShelley :: Gen ShelleyKeyPair
genGenesisKeyPairShelley =
  mkGenKeyPair <$> genSeed

genRegularKeyPairShelley :: Gen ShelleyKeyPair
genRegularKeyPairShelley =
  mkKeyPair <$> genSeed

genShelleyKeyDiscriminator :: Gen ShelleyKeyDiscriminator
genShelleyKeyDiscriminator =
  Gen.choice [pure GenesisShelleyKey, pure RegularShelleyKey]

genGenesisShelleyVerificationKey :: Gen (VKeyGenesis TPraosStandardCrypto)
genGenesisShelleyVerificationKey = do
  (GenesisKeyPairShelley vk _) <- mkGenKeyPair <$> genSeed
  pure vk

genRegularShelleyVerificationKey :: Gen (VKey TPraosStandardCrypto)
genRegularShelleyVerificationKey = do
  (KeyPairShelley vk _) <- mkKeyPair <$> genSeed
  pure vk

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genProtocolMagicId
    ]

genPublicKeyByron :: Gen ByronPublicKey
genPublicKeyByron =
  mkByronPublicKey <$> genKeyPairByron <*> genNetwork

genPublicKeyShelley :: Gen ShelleyPublicKey
genPublicKeyShelley =
  mkShelleyPublicKey <$> genKeyPairShelley

genTxSigned :: Gen TxSigned
genTxSigned =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, genTxSignedByron)
    , (1, pure TxSignedShelley)
    ]

genTxSignedByron :: Gen TxSigned
genTxSignedByron =
  signTransaction
    <$> genTxUnsignedByron
    <*> genNetwork
    <*> Gen.list (Range.linear 1 5) genSigningKey

genTxUnsigned :: Gen TxUnsigned
genTxUnsigned =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, genTxUnsignedByron)
    , (1, pure TxUnsignedShelley)
    ]

genTxUnsignedByron :: Gen TxUnsigned
genTxUnsignedByron = do
  tx <- genTx
  let cbor = serialize tx
  pure $ TxUnsignedByron tx (LBS.toStrict cbor) (coerce $ hashRaw cbor)

------------------------------------------------------------------------------
-- Shelley Helpers
------------------------------------------------------------------------------

-- | Generate a deterministic genesis key pair given a seed.
mkGenKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> ShelleyKeyPair
mkGenKeyPair seed =
  fst . withDRG (drgNewTest seed) $ do
    sk <- genKeyDSIGN
    return $ GenesisKeyPairShelley (VKeyGenesis $ deriveVerKeyDSIGN sk) (SKey sk)

-- | Generate a deterministic key pair given a seed.
mkKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> ShelleyKeyPair
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return $ KeyPairShelley (VKey $ deriveVerKeyDSIGN sk) (SKey sk)


