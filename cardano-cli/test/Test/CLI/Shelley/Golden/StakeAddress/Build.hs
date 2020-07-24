{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.StakeAddress.Build
  ( golden_shelleyStakeAddressBuild
  ) where

import Cardano.Prelude

import Hedgehog (Property, (===))

import qualified System.IO as IO
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressBuild :: Property
golden_shelleyStakeAddressBuild = OP.propertyOnce $ do
  OP.workspace "tmp/stake-address-build" $ \tempDir -> do
    verificationKeyFile <- OP.noteInputFile "test/Test/golden/shelley/keys/stake_keys/verification_key"
    rewardAddressFile <- OP.noteTempFile tempDir "reward-address.hex"

    rewardAddress <- OP.execCardanoCLI
        [ "shelley","stake-address","build"
        , "--mainnet"
        , "--staking-verification-key-file", verificationKeyFile
        ]

    void . liftIO $ IO.writeFile rewardAddressFile rewardAddress

    rewardAddress === "stake1uxqmgfzls3vn7c7qlu3fdycz2nmh5p5sl2w7t7tfetp8evqacghf3\n"
