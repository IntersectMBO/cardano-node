{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.Build
  ( golden_shelleyAddressBuild
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

import qualified System.IO as IO

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressBuild :: Property
golden_shelleyAddressBuild = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteInputFile "test/data/golden/shelley/keys/payment_keys/verification_key"
  addressSKeyFile <- noteInputFile "test/data/golden/shelley/keys/stake_keys/verification_key"
  goldenStakingAddressHexFile <- noteInputFile "test/data/golden/shelley/addresses/staking-address.hex"
  goldenEnterpriseAddressHexFile <- noteInputFile "test/data/golden/shelley/addresses/enterprise-address.hex"
  stakingAddressHexFile <- noteTempFile tempDir "staking-address.hex"
  enterpriseAddressHexFile <- noteTempFile tempDir "enterprise-address.hex"

  void $ OP.readFile addressVKeyFile

  stakingAddressText <- execCardanoCLI
    [ "shelley","address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  goldenStakingAddressHex <- OP.readFile goldenStakingAddressHexFile

  liftIO $ IO.writeFile stakingAddressHexFile stakingAddressText

  equivalence stakingAddressText goldenStakingAddressHex

  void $ OP.readFile addressSKeyFile

  enterpriseAddressText <- execCardanoCLI
    [ "shelley","address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  goldenEnterpriseAddressHex <- OP.readFile goldenEnterpriseAddressHexFile

  liftIO $ IO.writeFile enterpriseAddressHexFile enterpriseAddressText

  equivalence enterpriseAddressText goldenEnterpriseAddressHex
