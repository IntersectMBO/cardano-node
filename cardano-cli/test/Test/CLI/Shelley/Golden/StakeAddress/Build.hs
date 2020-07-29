{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.StakeAddress.Build
  ( golden_shelleyStakeAddressBuild
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressBuild :: Property
golden_shelleyStakeAddressBuild = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \_ -> do
  verificationKeyFile <- OP.noteInputFile "test/Test/golden/shelley/keys/stake_keys/verification_key"
  goldenRewardAddressFile <- OP.noteInputFile "test/Test/golden/shelley/keys/stake_keys/reward_address"

  rewardAddress <- OP.execCardanoCLI
      [ "shelley","stake-address","build"
      , "--mainnet"
      , "--staking-verification-key-file", verificationKeyFile
      ]

  goldenRewardsAddress <- OP.readFile goldenRewardAddressFile

  OP.equivalence rewardAddress goldenRewardsAddress
