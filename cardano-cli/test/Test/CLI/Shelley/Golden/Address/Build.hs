{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Address.Build
  ( golden_shelleyAddressBuild
  ) where

import Cardano.Prelude hiding (to)

import Hedgehog (Property, (===))

import qualified Data.List as L
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressBuild :: Property
golden_shelleyAddressBuild = OP.propertyOnce $ OP.workspace "tmp/address-key-gen" $ \tempDir -> do
  addressVKeyFile <- OP.noteEvalM $ OP.newFileWithContents (tempDir <> "/address.vkey") "{\
    \    \"type\": \"PaymentVerificationKeyShelley\",\
    \    \"description\": \"Payment Verification Key\",\
    \    \"cborHex\": \"58208dc60533b5dfa60a530955a696323a2ef4f14e8bc95a8f84cf6c441fea423427\"\
    \}"
  addressSKeyFile <- OP.noteEvalM $ OP.newFileWithContents (tempDir <> "/address.skey") "\
    \{\
    \    \"type\": \"StakingVerificationKeyShelley\",\
    \    \"description\": \"Stake Verification Key\",\
    \    \"cborHex\": \"5820bd07998bca8de945482d950a3051f0e4d18afedecbc08d4c99c23f1804f3c8e5\"\
    \}"

  stackingAddressText <- OP.noteEvalM . liftIO $ OP.execCardanoCLI
    [ "shelley","address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  length (L.lines stackingAddressText) === 1

  enterpriseAddressText <- OP.noteEvalM . liftIO $ OP.execCardanoCLI
    [ "shelley","address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  length (L.lines enterpriseAddressText) === 1
