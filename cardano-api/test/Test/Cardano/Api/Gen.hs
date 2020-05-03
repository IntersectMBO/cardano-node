module Test.Cardano.Api.Gen
  ( genSigningKey
  , genSigningKeyByron
  , genSigningKeyShelley
  , genNetwork
  , genVerificationKey
  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  , genByronVerificationKeyAddress
  , genShelleyVerificationKeyAddress
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import           Cardano.Crypto (hashRaw)
import           Cardano.Crypto.DSIGN
import           Cardano.Prelude

import           Crypto.Random (drgNewTest, withDRG)

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)

import           Shelley.Spec.Ledger.Keys

import           Test.Cardano.Chain.UTxO.Gen (genTx)
import qualified Test.Cardano.Crypto.Gen as Byron
                   (genProtocolMagicId, genSigningKey)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genByronVerificationKeyAddress :: Gen Address
genByronVerificationKeyAddress =
  byronVerificationKeyAddress <$> genVerificationKeyByron <*> genNetwork

genShelleyVerificationKeyAddress :: Gen Address
genShelleyVerificationKeyAddress =
  shelleyVerificationKeyAddress <$> genVerificationKeyShelley <*> genNetwork

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
  seed <- genSeed
  let sk = fst (withDRG (drgNewTest seed) genKeyDSIGN)
  return $ SigningKeyShelley (SKey sk)

genSeed :: Gen (Word64, Word64, Word64, Word64, Word64)
genSeed =
  (,,,,)
    <$> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded

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
  getVerificationKey <$> genSigningKeyByron

genVerificationKeyShelley :: Gen VerificationKey
genVerificationKeyShelley =
  getVerificationKey <$> genSigningKeyShelley

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
  Gen.choice
    [ genTxUnsignedByron
--  , genTxUnsignedShelley  --TODO
    ]

genTxUnsignedByron :: Gen TxUnsigned
genTxUnsignedByron = do
  tx <- genTx
  let cbor = serialize tx
  pure $ TxUnsignedByron tx (LBS.toStrict cbor) (coerce $ hashRaw cbor)

--genTxUnsignedShelley :: Gen TxUnsigned
--genTxUnsignedShelley = fail "TODO: genTxUnsignedShelley"
--TODO: reuse an existing generator


