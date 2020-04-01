module Test.Cardano.Api.Gen
  ( genKeyPair
  ) where

import           Cardano.Api
import           Cardano.Prelude

import           Test.Cardano.Crypto.Gen (genSigningKey, genVerificationKey)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen


genKeyPair :: Gen KeyPair
genKeyPair =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, KeyPairByron <$> genVerificationKey <*> genSigningKey)
    , (1, pure KeyPairShelley)
    ]
