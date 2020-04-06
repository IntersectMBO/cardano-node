module Test.Cardano.Api.Gen
  ( genAddress
  , genKeyPair
  , genKeyPairByron
  , genNetwork
  , genPublicKey
  , genPublicKeyByron
  ) where

import           Cardano.Api
import           Cardano.Prelude

import           Test.Cardano.Crypto.Gen (genProtocolMagicId, genSigningKey, genVerificationKey)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen


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
