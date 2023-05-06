{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.RegistrationCertificate
  ( golden_shelleyStakeAddressRegistrationCertificate
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import           System.FilePath ((</>))
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressRegistrationCertificate :: Property
golden_shelleyStakeAddressRegistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  base <- H.getProjectBase

  keyGenStakingVerificationKeyFile <- noteInputFile "test/data/golden/shelley/keys/stake_keys/verification_key"
  registrationCertFile <- noteTempFile tempDir "registration.cert"
  scriptRegistrationCertFile <- noteTempFile tempDir "script-registration.cert"
  exampleScript <- noteInputFile $ base </> "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"

  void $ execCardanoCLI
    [ "stake-address","registration-certificate"
    , "--staking-verification-key-file", keyGenStakingVerificationKeyFile
    , "--out-file", registrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Address Registration Certificate" registrationCertFile

  void $ execCardanoCLI
    [ "stake-address","registration-certificate"
    , "--stake-script-file", exampleScript
    , "--out-file", scriptRegistrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Address Registration Certificate" scriptRegistrationCertFile

  H.assertEndsWithSingleNewline registrationCertFile
