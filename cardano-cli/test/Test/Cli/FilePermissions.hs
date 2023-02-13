{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.FilePermissions
  ( tests
  ) where

import           Cardano.Node.Run (checkVRFFilePermissions)

import           Hedgehog (Property, discover, success)
import qualified Hedgehog
import qualified Hedgehog.Extras.Test.Base as H
import           Hedgehog.Internal.Property (failWith)

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (runExceptT)
import           Test.OptParse (execCardanoCLI)

-- | This property ensures that the VRF signing key file is created only with owner permissions
prop_createVRFSigningKeyFilePermissions :: Property
prop_createVRFSigningKeyFilePermissions =
  H.propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Key filepaths
    vrfVerKey <- H.noteTempFile tempDir "VRF-verification-key-file"

    vrfSignKey <- H.noteTempFile tempDir "VRF-signing-key-file"

    -- Create VRF key pair
    void $ execCardanoCLI
      [ "node", "key-gen-VRF"
      , "--verification-key-file", vrfVerKey
      , "--signing-key-file", vrfSignKey
      ]

    result <- liftIO . runExceptT $ checkVRFFilePermissions vrfSignKey
    case result of
      Left err ->
        failWith Nothing
          $ "key-gen-VRF cli command created a VRF signing key \
            \file with the wrong permissions: " <> show err
      Right () -> success

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
