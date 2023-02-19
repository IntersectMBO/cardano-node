{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Key.NonExtendedKey
  ( golden_KeyNonExtendedKey
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import           System.FilePath ((</>))
import           Test.OptParse (execCardanoCLI, propertyOnce)
import           Test.Utilities (diffVsGoldenFile)

{- HLINT ignore "Use camelCase" -}

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
golden_KeyNonExtendedKey :: Property
golden_KeyNonExtendedKey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    genesisVKeyFp <- H.note "test/data/golden/key/non-extended-keys/shelley.000.vkey"
    nonExtendedGenesisVKeyFp <- H.note $ tempDir </> "non-extended-shelley.000.vkey"

    H.assertFilesExist [genesisVKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key", "non-extended-key"
      , "--extended-verification-key-file", genesisVKeyFp
      , "--verification-key-file", nonExtendedGenesisVKeyFp
      ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [nonExtendedGenesisVKeyFp]

    contents <- H.readFile nonExtendedGenesisVKeyFp

    diffVsGoldenFile contents nonExtendedGenesisVKeyFp
