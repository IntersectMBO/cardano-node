{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.MultiSig.Address
  ( golden_shelleyAllMultiSigAddressBuild
  , golden_shelleyAnyMultiSigAddressBuild
  , golden_shelleyAtLeastMultiSigAddressBuild
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyAllMultiSigAddressBuild :: Property
golden_shelleyAllMultiSigAddressBuild = propertyOnce . moduleWorkspace "tmp" $ \_ -> do
  allMultiSigFp <- noteInputFile "test/data/golden/shelley/multisig/scripts/all"

  allMultiSigAddress <- execCardanoCLI
    [ "shelley", "address", "build-script"
    , "--script-file", allMultiSigFp
    , "--mainnet"
    ]

  goldenAllMultiSigAddrFp <- noteInputFile "test/data/golden/shelley/multisig/addresses/all"

  goldenAllMs <- OP.readFile goldenAllMultiSigAddrFp

  equivalence allMultiSigAddress goldenAllMs

golden_shelleyAnyMultiSigAddressBuild :: Property
golden_shelleyAnyMultiSigAddressBuild = propertyOnce . moduleWorkspace "tmp" $ \_ -> do
  anyMultiSigFp <- noteInputFile "test/data/golden/shelley/multisig/scripts/any"

  anyMultiSigAddress <- execCardanoCLI
    [ "shelley", "address", "build-script"
    , "--script-file", anyMultiSigFp
    , "--mainnet"
    ]

  goldenAnyMultiSigAddrFp <- noteInputFile "test/data/golden/shelley/multisig/addresses/any"

  goldenAnyMs <- OP.readFile goldenAnyMultiSigAddrFp

  equivalence anyMultiSigAddress goldenAnyMs

golden_shelleyAtLeastMultiSigAddressBuild :: Property
golden_shelleyAtLeastMultiSigAddressBuild = propertyOnce . moduleWorkspace "tmp" $ \_ -> do
  atLeastMultiSigFp <- noteInputFile "test/data/golden/shelley/multisig/scripts/atleast"

  atLeastMultiSigAddress <- execCardanoCLI
    [ "shelley", "address", "build-script"
    , "--script-file", atLeastMultiSigFp
    , "--mainnet"
    ]

  goldenAtLeastMultiSigAddrFp <- noteInputFile "test/data/golden/shelley/multisig/addresses/atleast"

  goldenAtLeastMs <- OP.readFile goldenAtLeastMultiSigAddrFp

  equivalence atLeastMultiSigAddress goldenAtLeastMs
