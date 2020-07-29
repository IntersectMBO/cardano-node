{-# LANGUAGE OverloadedStrings #-}

module Test.Pioneers.Exercise1
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Hedgehog as H
import qualified Test.OptParse as OP

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. We use the generated verification key to build a shelley payment address.
prop_buildShelleyPaymentAddress :: Property
prop_buildShelleyPaymentAddress = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "payment-verification-key-file"
  signKey <- OP.noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $ OP.execCardanoCLI
    [ "shelley","address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  OP.assertFilesExist [verKey, signKey]

  -- Build shelley payment address
  void $ OP.execCardanoCLI
    [ "shelley", "address", "build"
    , "--payment-verification-key-file", verKey
    , "--mainnet"
    ]

-- | 1. We generate a key payment pair
--   2. We generate a staking key pair
--   2. Check for the existence of the key pairs
--   3. We use the payment verification key & staking verification key
--      to build a shelley stake address.
prop_buildShelleyStakeAddress :: Property
prop_buildShelleyStakeAddress = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  stakeVerKey <- OP.noteTempFile tempDir "stake-verification-key-file"
  stakeSignKey <- OP.noteTempFile tempDir "stake-signing-key-file"
  paymentVerKey <- OP.noteTempFile tempDir "payment-verification-key-file"
  paymentSignKey <- OP.noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $ OP.execCardanoCLI
    [ "shelley","address","key-gen"
    , "--verification-key-file", paymentVerKey
    , "--signing-key-file", paymentSignKey
    ]

  -- Generate stake verification key
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", stakeVerKey
    , "--signing-key-file", stakeSignKey
    ]

  OP.assertFilesExist [stakeVerKey, stakeSignKey, paymentVerKey, paymentSignKey]

  -- Build shelley stake address
  void $ OP.execCardanoCLI
    [ "shelley", "address", "build"
    , "--payment-verification-key-file", paymentVerKey
    , "--stake-verification-key-file", stakeVerKey
    , "--mainnet"
    ]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 1"
        [ ("prop_buildShelleyPaymentAddress", prop_buildShelleyPaymentAddress)
        , ("prop_buildShelleyStakeAddress", prop_buildShelleyStakeAddress)
        ]
