module Test.Cardano.Api.Gen
  ( genAddress
  , genKeyPair
  , genKeyPairByron
  , genNetwork
  , genPublicKey
  , genPublicKeyByron

  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import           Cardano.Crypto (hashRaw)
import           Cardano.Prelude

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)

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
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, genKeyPairByron)
    , (1, pure KeyPairShelley)
    ]

genKeyPairByron :: Gen KeyPair
genKeyPairByron =
  KeyPairByron <$> genVerificationKey <*> genSigningKey

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genProtocolMagicId
    ]

genPublicKey :: Gen PublicKey
genPublicKey =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, genPublicKeyByron)
    , (1, pure PubKeyShelley)
    ]

genPublicKeyByron :: Gen PublicKey
genPublicKeyByron =
  mkPublicKey <$> genKeyPairByron <*> genNetwork

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
