{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.Build
  ( golden_shelleyStakeAddressBuild
  ) where

import           Hedgehog (Property)
import           Test.Cardano.CLI.Util as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressBuild :: Property
golden_shelleyStakeAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  verificationKeyFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/keys/stake_keys/verification_key"
  goldenRewardAddressFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/keys/stake_keys/reward_address"

  rewardAddress <- execCardanoCLI
    [ "stake-address","build"
    , "--mainnet"
    , "--staking-verification-key-file", verificationKeyFile
    ]

  goldenRewardsAddress <- H.readFile goldenRewardAddressFile

  equivalence rewardAddress goldenRewardsAddress
