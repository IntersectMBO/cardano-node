{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Test.Cardano.Node.FilePermissions
  ( tests
  ) where

import           Control.Monad.Except
import           Data.Foldable
import           System.Directory (removeFile)

import           Cardano.Api
import           Cardano.Node.Run (checkVRFFilePermissions)
import           Control.Exception (bracket)
import           Control.Monad (Monad (..))
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Bool (Bool, not)
import           Data.Either (Either (..))
import           Data.Eq ((==))
import           Data.Foldable (foldl', length)
import           Data.Function (const, ($), (.))
import qualified Data.List as L
import           Data.Maybe (Maybe (..))
import           Data.Semigroup (Semigroup (..))
import           Hedgehog (Property, PropertyT, property, success)
import qualified Hedgehog
import           Hedgehog.Internal.Property (Group (..), failWith)
import           System.IO (FilePath, IO)
import           Text.Show (Show (..))

#ifdef UNIX
import           Cardano.Node.Types (VRFPrivateKeyFilePermissionError (..))

import           System.Posix.Files
import           System.Posix.IO (closeFd, createFile)
import           System.Posix.Types (FileMode)

import           Control.Exception (bracket)
import           Hedgehog (Gen, classify, forAll)
import qualified Hedgehog.Gen as Gen
#endif


{- HLINT ignore "Use fewer imports" -}

prop_createVRFFileWithOwnerPermissions :: Property
prop_createVRFFileWithOwnerPermissions =
  property $ do
    let vrfSign = "vrf-signing-key"
    vrfSkey <- liftIO $ generateSigningKey AsVrfKey
    createFileWithOwnerPermissions vrfSign vrfSkey

    fResult <- liftIO . runExceptT $ checkVRFFilePermissions vrfSign
    case fResult of
      Left err -> failWith Nothing $ show err
      Right () -> liftIO (removeFile (unFile vrfSign)) >> success

createFileWithOwnerPermissions :: HasTextEnvelope a => File () Out -> a -> PropertyT IO ()
createFileWithOwnerPermissions targetfp value = do
  result <- liftIO $ writeLazyByteStringFileWithOwnerPermissions targetfp $ textEnvelopeToJSON Nothing value
  case result of
    Left err -> failWith Nothing $ docToString $ prettyError @(FileError ()) err
    Right () -> return ()

#ifdef UNIX

-- | This property ensures that 'checkVRFFilePermissions' checks the
-- file permissions & ownership correctly.
prop_sanityCheck_checkVRFFilePermissions :: Property
prop_sanityCheck_checkVRFFilePermissions =
  property $ do
    -- Correct case: only owner has read permission
    let correctPermission = ownerReadMode
        vrfPrivateKeyCorrect = File "vrf-private-key-correct"
    correctResult <-
      liftIO $ bracket  (createFile (unFile vrfPrivateKeyCorrect) correctPermission)
                        (\h -> closeFd h >> removeFile (unFile vrfPrivateKeyCorrect))
                        (const . liftIO . runExceptT $ checkVRFFilePermissions vrfPrivateKeyCorrect)
    case correctResult of
      Left err ->
        failWith Nothing $ "checkVRFFilePermissions should not have failed with error: "
                         <> show err
      Right () -> success

    -- Error case: owner has read permissions & various combinations of other permissions
    let vrfPrivateKeyOther = File "vrf-private-key-other"
    oPermissions <- forAll genOtherPermissions
    classify "VRF File has one other permission" $ length oPermissions == 1
    classify "VRF File has two other permissions" $ length oPermissions == 2
    classify "VRF File has three other permissions" $ length oPermissions == 3
    otherResult <-
      -- Creating a file with other permissions appears to not work
      -- it instead creates a file with owner permissions. Therefore we must
      -- create a file with no permissions and then set other permissions
      liftIO $ bracket  (do h <- createFile (unFile vrfPrivateKeyOther) nullFileMode
                            setFileMode (unFile vrfPrivateKeyOther) $ createPermissions oPermissions
                            return h)
                        (\h -> closeFd h >> removeFile (unFile vrfPrivateKeyOther))
                        (const .liftIO . runExceptT $ checkVRFFilePermissions vrfPrivateKeyOther)
    case otherResult of
      Left (OtherPermissionsExist _) -> success
      Left err ->
        failWith Nothing $ "checkVRFFilePermissions should not have failed with error: "
                         <> show err
      Right () ->
        failWith Nothing "This should have failed as Other permissions exist"

    -- Error case: owner has read permissions & various combinations of group permissions
    let vrfPrivateKeyGroup = File "vrf-private-key-group"
    gPermissions <- forAll genGroupPermissions
    classify "VRF File has one group permission" $ length gPermissions == 1
    classify "VRF File has two group permissions" $ length gPermissions == 2
    classify "VRF File has three group permissions" $ length gPermissions == 3
    groupResult <-
      -- Creating a file with group permissions appears to not work
      -- it instead creates a file with owner permissions. Therefore we must
      -- create a file with no permissions and then set group permissions.
      liftIO $ bracket  (do h <- createFile (unFile vrfPrivateKeyGroup) nullFileMode
                            setFileMode (unFile vrfPrivateKeyGroup) $ createPermissions gPermissions
                            return h)
                        (\h -> closeFd h >> removeFile (unFile vrfPrivateKeyGroup))
                        (const . liftIO . runExceptT $ checkVRFFilePermissions vrfPrivateKeyGroup)
    case groupResult of
      Left (GroupPermissionsExist _) -> success
      Left err ->
        failWith Nothing $ "checkVRFFilePermissions should not have failed with error: "
                         <> show err
      Right () ->
        failWith Nothing "This should have failed as Group permissions exist"

createPermissions :: [FileMode] -> FileMode
createPermissions = foldl' unionFileModes (ownerReadMode `unionFileModes` ownerWriteMode)

genGroupPermissions :: Gen [FileMode]
genGroupPermissions =
  let gPermissions = [groupReadMode, groupWriteMode, groupExecuteMode]
  in do subSeq <- Gen.filter (not . L.null) $ Gen.subsequence gPermissions
        Gen.frequency [(3, return gPermissions), (12, return subSeq)]

genOtherPermissions :: Gen [FileMode]
genOtherPermissions =
  let oPermissions = [otherReadMode, otherWriteMode, otherExecuteMode]
  in do subSeq <- Gen.filter (not . L.null) $ Gen.subsequence oPermissions
        Gen.frequency [(3, return oPermissions), (12, return subSeq)]
#endif

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $ Group "Test.Cardano.Node.FilePermissons"
#ifdef UNIX
    [ ("prop_createVRFFileWithOwnerPermissions", prop_createVRFFileWithOwnerPermissions)
    , ("prop_sanityCheck_checkVRFFilePermissions", prop_sanityCheck_checkVRFFilePermissions)
    ]
#else
    [("prop_createVRFFileWithOwnerPermissions", prop_createVRFFileWithOwnerPermissions)]
#endif
