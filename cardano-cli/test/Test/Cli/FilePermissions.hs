{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Test.Cli.FilePermissions
  ( tests
  ) where

import           Prelude

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Text (Text)
import qualified Data.Text as Text

#ifdef UNIX
import           System.Posix.Files
import           System.Posix.Types (FileMode)
#else
import           System.Win32.File
#endif

import           Hedgehog (Property, discover, success)
import qualified Hedgehog
import qualified Hedgehog.Extras.Test.Base as H
import           Hedgehog.Internal.Property (failWith)

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

data VRFPrivateKeyFilePermissionError
  = OtherPermissionsExist !FilePath
  | GroupPermissionsExist !FilePath
  | GenericPermissionsExist !FilePath
  deriving Show

renderVRFPrivateKeyFilePermissionError :: VRFPrivateKeyFilePermissionError -> Text
renderVRFPrivateKeyFilePermissionError err =
  case err of
    OtherPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> " has \"other\" file permissions. Please remove all \"other\" file permissions."

    GroupPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> "has \"group\" file permissions. Please remove all \"group\" file permissions."
    GenericPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> "has \"generic\" file permissions. Please remove all \"generic\" file permissions."


-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVRFFilePermissions :: FilePath -> ExceptT VRFPrivateKeyFilePermissionError IO ()
#ifdef UNIX
checkVRFFilePermissions vrfPrivKey = do
  fs <- liftIO $ getFileStatus vrfPrivKey
  let fm = fileMode fs
  -- Check the the VRF private key file does not give read/write/exec permissions to others.
  when (hasOtherPermissions fm)
       (left $ OtherPermissionsExist vrfPrivKey)
  -- Check the the VRF private key file does not give read/write/exec permissions to any group.
  when (hasGroupPermissions fm)
       (left $ GroupPermissionsExist vrfPrivKey)
 where
  hasPermission :: FileMode -> FileMode -> Bool
  hasPermission fModeA fModeB = fModeA `intersectFileModes` fModeB /= nullFileMode

  hasOtherPermissions :: FileMode -> Bool
  hasOtherPermissions fm' = fm' `hasPermission` otherModes

  hasGroupPermissions :: FileMode -> Bool
  hasGroupPermissions fm' = fm' `hasPermission` groupModes
#else
checkVRFFilePermissions vrfPrivKey = do
  attribs <- liftIO $ getFileAttributes vrfPrivKey
  -- https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea
  -- https://docs.microsoft.com/en-us/windows/win32/fileio/file-access-rights-constants
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/standard-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/generic-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/access-mask
  when (attribs `hasPermission` genericPermissions)
       (left $ GenericPermissionsExist vrfPrivKey)
 where
  genericPermissions = gENERIC_ALL .|. gENERIC_READ .|. gENERIC_WRITE .|. gENERIC_EXECUTE
  hasPermission fModeA fModeB = fModeA .&. fModeB /= gENERIC_NONE
#endif


-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
