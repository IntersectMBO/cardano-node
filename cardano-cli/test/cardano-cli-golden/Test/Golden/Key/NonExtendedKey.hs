{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Key.NonExtendedKey
  ( golden_KeyNonExtendedKey_GenesisExtendedVerificationKey
  , golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H
import           System.FilePath ((</>))
import           Test.Cardano.CLI.Util (execCardanoCLI, propertyOnce)

{- HLINT ignore "Use camelCase" -}

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
golden_KeyNonExtendedKey_GenesisExtendedVerificationKey :: Property
golden_KeyNonExtendedKey_GenesisExtendedVerificationKey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    genesisVKeyFp <- H.note "test/cardano-cli-golden/files/golden/key/non-extended-keys/shelley.000.vkey"
    nonExtendedFp <-  H.note "test/cardano-cli-golden/files/golden/key/non-extended-keys/non-extended-shelley.000.vkey"
    outFp <- H.note $ tempDir </> "non-extended-shelley.000.vkey"

    H.assertFilesExist [genesisVKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key", "non-extended-key"
      , "--extended-verification-key-file", genesisVKeyFp
      , "--verification-key-file", outFp
      ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [outFp]

    H.diffFileVsGoldenFile outFp nonExtendedFp

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley :: Property
golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    genesisVKeyFp <- H.note "test/cardano-cli-golden/files/golden/key/non-extended-keys/stake.000.vkey"
    nonExtendedFp <-  H.note "test/cardano-cli-golden/files/golden/key/non-extended-keys/non-extended-stake.000.vkey"
    outFp <- H.note $ tempDir </> "non-extended-stake.000.vkey"

    H.assertFilesExist [genesisVKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key", "non-extended-key"
      , "--extended-verification-key-file", genesisVKeyFp
      , "--verification-key-file", outFp
      ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [outFp]

    H.diffFileVsGoldenFile outFp nonExtendedFp
