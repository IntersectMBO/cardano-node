{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.IssueOpCert
  ( golden_shelleyNodeIssueOpCert
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import           Test.Cardano.CLI.Util

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeIssueOpCert :: Property
golden_shelleyNodeIssueOpCert = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  hotKesVerificationKeyFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/keys/kes_keys/verification_key"
  coldSigningKeyFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/keys/genesis_delegate_keys/signing_key"
  originalOperationalCertificateIssueCounterFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/keys/genesis_delegate_keys/operational_certificate_counter"
  operationalCertificateIssueCounterFile <- noteTempFile tempDir "delegate-op-cert.counter"
  operationalCertFile <- noteTempFile tempDir "operational.cert"

  H.copyFile originalOperationalCertificateIssueCounterFile operationalCertificateIssueCounterFile

  -- We could generate the required keys here, but then if the KES generation fails this
  -- test would also fail which is misleading.
  -- However, the keys can be generated eg:
  --    cabal run cardano-cli:cardano-cli -- shelley node key-gen-KES \
  --        --verification-key-file cardano-cli/test/cli/node-issue-op-cert/data/node-kes.vkey \
  --        --signing-key-file /dev/null
  void $ execCardanoCLI
    [ "node","issue-op-cert"
    , "--hot-kes-verification-key-file", hotKesVerificationKeyFile
    , "--cold-signing-key-file", coldSigningKeyFile
    , "--operational-certificate-issue-counter", operationalCertificateIssueCounterFile
    , "--kes-period", "0"
    , "--out-file", operationalCertFile
    ]

  H.assertFileOccurences 1 "NodeOperationalCertificate" operationalCertFile
  H.assertFileOccurences 1 "Next certificate issue number: 1" operationalCertificateIssueCounterFile

  H.assertEndsWithSingleNewline operationalCertFile
  H.assertEndsWithSingleNewline operationalCertificateIssueCounterFile
