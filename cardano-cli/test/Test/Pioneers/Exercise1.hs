{-# LANGUAGE OverloadedStrings #-}

module Test.Pioneers.Exercise1
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. We use the generated verification key to build a shelley payment address.
prop_buildShelleyPaymentAddress :: Property
prop_buildShelleyPaymentAddress =
  propertyOnce $ do

    -- Key filepaths
    let verKey = "payment-verification-key-file"
        signKey = "payment-signing-key-file"
        allFiles = [verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","address","key-gen"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    assertFilesExist [verKey, signKey]

    -- Build shelley payment address
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley", "address", "build"
                               , "--payment-verification-key-file", verKey
                               , "--mainnet"
                               ]

    liftIO $ fileCleanup [verKey, signKey]
    H.success



-- | 1. We generate a key payment pair
--   2. We generate a staking key pair
--   2. Check for the existence of the key pairs
--   3. We use the payment verification key & staking verification key
--      to build a shelley stake address.
prop_buildShelleyStakeAddress :: Property
prop_buildShelleyStakeAddress =
  propertyOnce $ do

    -- Key filepaths
    let stakeVerKey = "stake-verification-key-file"
        stakeSignKey = "stake-signing-key-file"
        paymentVerKey = "payment-verification-key-file"
        paymentSignKey = "payment-signing-key-file"
        allFiles = [stakeVerKey, stakeSignKey, paymentVerKey, paymentSignKey]

    -- Generate payment verification key
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","address","key-gen"
                               , "--verification-key-file", paymentVerKey
                               , "--signing-key-file", paymentSignKey
                               ]
    -- Generate stake verification key
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","key-gen"
                               , "--verification-key-file", stakeVerKey
                               , "--signing-key-file", stakeSignKey
                               ]

    assertFilesExist allFiles

    -- Build shelley stake address
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley", "address", "build"
                               , "--payment-verification-key-file", paymentVerKey
                               , "--stake-verification-key-file", stakeVerKey
                               , "--mainnet"
                               ]

    liftIO $ fileCleanup allFiles
    H.success

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 1"
        [ ("prop_buildShelleyPaymentAddress", prop_buildShelleyPaymentAddress)
        , ("prop_buildShelleyStakeAddress", prop_buildShelleyStakeAddress)
        ]
