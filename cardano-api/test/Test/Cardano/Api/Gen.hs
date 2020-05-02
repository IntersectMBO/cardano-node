module Test.Cardano.Api.Gen
  ( genAddress
  , genKeyPair
  , genKeyPairByron
  , genKeyPairShelley
  , genNetwork
  , genVerificationKey
  , genVerificationKeyByron
  , genVerificationKeyShelley
  , genShelleyVerificationKey
  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import           Cardano.Crypto (hashRaw)
import           Cardano.Crypto.DSIGN.Ed448 ()
import           Cardano.Prelude

import           Crypto.Random (drgNewTest, withDRG)

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)

import           Test.Cardano.Chain.UTxO.Gen (genTx)
import qualified Test.Cardano.Crypto.Gen as Byron
                   (genProtocolMagicId, genSigningKey, genVerificationKey)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genAddress :: Gen Address
genAddress =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, byronVerificationKeyAddress <$> genVerificationKey <*> genNetwork)
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
  KeyPairByron <$> Byron.genVerificationKey <*> Byron.genSigningKey

genKeyPairShelley :: Gen KeyPair
genKeyPairShelley =
  mkDeterministicKeyPairShelley <$> genSeed

genSeed :: Gen (Word64, Word64, Word64, Word64, Word64)
genSeed =
  (,,,,)
    <$> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded

genShelleyVerificationKey :: Gen ShelleyVerificationKey
genShelleyVerificationKey = do
  KeyPairShelley vk _ <- mkDeterministicKeyPairShelley <$> genSeed
  pure vk

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> Byron.genProtocolMagicId
    ]

genVerificationKey :: Gen VerificationKey
genVerificationKey =
  Gen.choice
    [ genVerificationKeyByron
    , genVerificationKeyShelley
    ]

genVerificationKeyByron :: Gen VerificationKey
genVerificationKeyByron =
  mkVerificationKey <$> genKeyPairByron

genVerificationKeyShelley :: Gen VerificationKey
genVerificationKeyShelley =
  mkVerificationKey <$> genKeyPairShelley

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
    <*> Gen.list (Range.linear 1 5) Byron.genSigningKey

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

mkDeterministicKeyPairShelley :: (Word64, Word64, Word64, Word64, Word64)
                              -> KeyPair
mkDeterministicKeyPairShelley seed =
  fst $ withDRG (drgNewTest seed) genericShelleyKeyPair
