{-# LANGUAGE PatternSynonyms #-}

module Test.Cardano.Api.Gen
  ( genAddress
  , genKeyPair
  , genKeyPairByron
  , genKeyPairShelley
  , genNetwork
  , genPublicKey
  , genPublicKeyByron
  , genPublicKeyShelley
  , genShelleyKeyDiscriminator
  , genShelleyVerificationKey
  , genTextView
  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import           Cardano.Crypto (hashRaw)
import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import           Cardano.Crypto.DSIGN.Ed448 ()
import           Cardano.Prelude

import           Crypto.Random (drgNewTest, withDRG)

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)

import qualified Shelley.Spec.Ledger.Keys as Shelley (KeyDiscriminator (..), pattern SKey,
                     pattern VKey, pattern VKeyGenesis)

import           Test.Cardano.Chain.UTxO.Gen (genTx)
import           Test.Cardano.Crypto.Gen (genProtocolMagicId, genSigningKey, genVerificationKey)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genAddress :: Gen Address
genAddress =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, byronPubKeyAddress <$> genPublicKey)
    , (1, pure AddressShelley)
    ]

genKeyPair :: Gen KeyPair
genKeyPair =
  Gen.choice
    [ genKeyPairByron
    , genKeyPairShelley
    ]

genKeyPairByron :: Gen KeyPair
genKeyPairByron =
  KeyPairByron <$> genVerificationKey <*> genSigningKey

genKeyPairShelley :: Gen KeyPair
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

genGenesisKeyPairShelley :: Gen KeyPair
genGenesisKeyPairShelley =
  mkDeterministicGenesisKeyPairShelley <$> genSeed

genRegularKeyPairShelley :: Gen KeyPair
genRegularKeyPairShelley =
  mkDeterministicRegularKeyPairShelley <$> genSeed

genShelleyKeyDiscriminator :: Gen ShelleyKeyDiscriminator
genShelleyKeyDiscriminator =
  ShelleyKeyDiscriminator
    <$> Gen.choice [pure Shelley.Genesis, pure Shelley.Regular]

genShelleyVerificationKey :: Gen ShelleyVerificationKey
genShelleyVerificationKey =
  Gen.choice
    [ genGenesisShelleyVerificationKey
    , genRegularShelleyVerificationKey
    ]

genGenesisShelleyVerificationKey :: Gen ShelleyVerificationKey
genGenesisShelleyVerificationKey = do
  KeyPairShelley vk _ <- mkDeterministicGenesisKeyPairShelley <$> genSeed
  pure vk

genRegularShelleyVerificationKey :: Gen ShelleyVerificationKey
genRegularShelleyVerificationKey = do
  KeyPairShelley vk _ <- mkDeterministicRegularKeyPairShelley <$> genSeed
  pure vk

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genProtocolMagicId
    ]

genPublicKey :: Gen PublicKey
genPublicKey =
  Gen.choice
    [ genPublicKeyByron
    , genPublicKeyShelley
    ]

genPublicKeyByron :: Gen PublicKey
genPublicKeyByron =
  mkPublicKey <$> genKeyPairByron <*> genNetwork

genPublicKeyShelley :: Gen PublicKey
genPublicKeyShelley =
  mkPublicKey <$> genKeyPairShelley <*> genNetwork

genTextView :: Gen TextView
genTextView =
  TextView
    <$> Gen.utf8 (Range.linear 1 20) Gen.alpha
    <*> Gen.utf8 (Range.linear 1 80) (Gen.filter (/= '\n') Gen.ascii)
    <*> Gen.bytes (Range.linear 0 500)

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

mkDeterministicGenesisKeyPairShelley :: (Word64, Word64, Word64, Word64, Word64) -> KeyPair
mkDeterministicGenesisKeyPairShelley seed =
  fst . withDRG (drgNewTest seed) $ do
    sk <- genKeyDSIGN
    pure $ KeyPairShelley
      (GenesisShelleyVerificationKey (Shelley.VKeyGenesis $ deriveVerKeyDSIGN sk))
      (Shelley.SKey sk)

mkDeterministicRegularKeyPairShelley :: (Word64, Word64, Word64, Word64, Word64) -> KeyPair
mkDeterministicRegularKeyPairShelley seed =
  fst . withDRG (drgNewTest seed) $ do
    sk <- genKeyDSIGN
    pure $ KeyPairShelley
      (RegularShelleyVerificationKey (Shelley.VKey $ deriveVerKeyDSIGN sk))
      (Shelley.SKey sk)
