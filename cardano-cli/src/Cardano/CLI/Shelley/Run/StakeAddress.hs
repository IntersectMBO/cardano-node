{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError(ShelleyStakeAddressCmdReadKeyFileError)
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  , runStakeAddressKeyGenToFile
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, onLeft)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as Text

import           Cardano.Api
import           Cardano.Api.Pretty
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (DelegationTarget (..), StakeIdentifier (..),
                   StakeVerifier (..), VerificationKeyOrFile, readVerificationKeyOrFile,
                   readVerificationKeyOrHashOrFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Read
import           Cardano.CLI.Types
import           Control.Monad.Trans (lift)
import           Data.Function ((&))

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyStakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyStakeAddressCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Doc Ann
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeAddressCmdReadKeyFileError fileErr -> displayError fileErr
    ShelleyStakeAddressCmdWriteFileError fileErr -> displayError fileErr
    ShelleyStakeAddressCmdReadScriptFileError fileErr -> displayError fileErr

runStakeAddressCmd :: StakeAddressCmd -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGenToFile vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild stakeVerifier nw mOutputFp) =
  runStakeAddressBuild stakeVerifier nw mOutputFp
runStakeAddressCmd (StakeRegistrationCert stakeIdentifier outputFp) =
  runStakeCredentialRegistrationCert stakeIdentifier outputFp
runStakeAddressCmd (StakeCredentialDelegationCert stakeIdentifier stkPoolVerKeyHashOrFp outputFp) =
  runStakeCredentialDelegationCert stakeIdentifier stkPoolVerKeyHashOrFp outputFp
runStakeAddressCmd (StakeCredentialDeRegistrationCert stakeIdentifier outputFp) =
  runStakeCredentialDeRegistrationCert stakeIdentifier outputFp


--
-- Stake address command implementations
--

runStakeAddressKeyGenToFile
  :: VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyGenToFile vkFp skFp = do
  let skeyDesc = "Stake Signing Key"
  let vkeyDesc = "Stake Verification Key"

  skey <- liftIO $ generateSigningKey AsStakeKey

  let vkey = getVerificationKey skey

  firstExceptT ShelleyStakeAddressCmdWriteFileError $ do
    newExceptT $ writeLazyByteStringFile skFp $ textEnvelopeToJSON (Just skeyDesc) skey
    newExceptT $ writeLazyByteStringFile vkFp $ textEnvelopeToJSON (Just vkeyDesc) vkey

runStakeAddressKeyHash
  :: VerificationKeyOrFile StakeKey
  -> Maybe (File () Out)
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyHash stakeVerKeyOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (File fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuild
  :: StakeVerifier
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild stakeVerifier network mOutputFp = do
  stakeAddr <- getStakeAddressFromVerifier network stakeVerifier
  let stakeAddrText = serialiseAddress stakeAddr
  liftIO $
    case mOutputFp of
      Just (File fpath) -> Text.writeFile fpath stakeAddrText
      Nothing -> Text.putStrLn stakeAddrText


runStakeCredentialRegistrationCert
  :: StakeIdentifier
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialRegistrationCert stakeIdentifier oFp = do
  stakeCred <- getStakeCredentialFromIdentifier stakeIdentifier
  writeRegistrationCert stakeCred

 where
  writeRegistrationCert
    :: StakeCredential
    -> ExceptT ShelleyStakeAddressCmdError IO ()
  writeRegistrationCert sCred = do
    let deRegCert = makeStakeAddressRegistrationCertificate sCred
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ textEnvelopeToJSON (Just regCertDesc) deRegCert

  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"


runStakeCredentialDelegationCert
  :: StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> DelegationTarget
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDelegationCert stakeVerifier delegationTarget outFp =
  case delegationTarget of
    StakePoolDelegationTarget poolVKeyOrHashOrFile -> do
      poolStakeVKeyHash <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)
        & onLeft (left . ShelleyStakeAddressCmdReadKeyFileError)
      stakeCred <- getStakeCredentialFromIdentifier stakeVerifier
      let delegCert = makeStakeAddressPoolDelegationCertificate stakeCred poolStakeVKeyHash
      firstExceptT ShelleyStakeAddressCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Address Delegation Certificate") delegCert

runStakeCredentialDeRegistrationCert
  :: StakeIdentifier
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDeRegistrationCert stakeVerifier oFp = do
  stakeCred <- getStakeCredentialFromIdentifier stakeVerifier
  writeDeregistrationCert stakeCred

  where
    writeDeregistrationCert
      :: StakeCredential
      -> ExceptT ShelleyStakeAddressCmdError IO ()
    writeDeregistrationCert sCred = do
      let deRegCert = makeStakeAddressDeregistrationCertificate sCred
      firstExceptT ShelleyStakeAddressCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile oFp
        $ textEnvelopeToJSON (Just deregCertDesc) deRegCert

    deregCertDesc :: TextEnvelopeDescr
    deregCertDesc = "Stake Address Deregistration Certificate"


getStakeCredentialFromVerifier
  :: StakeVerifier
  -> ExceptT ShelleyStakeAddressCmdError IO StakeCredential
getStakeCredentialFromVerifier = \case
  StakeVerifierScriptFile (ScriptFile sFile) -> do
    ScriptInAnyLang _ script <-
      firstExceptT ShelleyStakeAddressCmdReadScriptFileError $
        readFileScriptInAnyLang sFile
    pure $ StakeCredentialByScript $ hashScript script

  StakeVerifierKey stakeVerKeyOrFile -> do
    stakeVerKey <-
      firstExceptT ShelleyStakeAddressCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
    pure $ StakeCredentialByKey $ verificationKeyHash stakeVerKey

getStakeCredentialFromIdentifier
  :: StakeIdentifier
  -> ExceptT ShelleyStakeAddressCmdError IO StakeCredential
getStakeCredentialFromIdentifier = \case
  StakeIdentifierAddress stakeAddr -> pure $ stakeAddressCredential stakeAddr
  StakeIdentifierVerifier stakeVerifier -> getStakeCredentialFromVerifier stakeVerifier

getStakeAddressFromVerifier
  :: NetworkId
  -> StakeVerifier
  -> ExceptT ShelleyStakeAddressCmdError IO StakeAddress
getStakeAddressFromVerifier networkId stakeVerifier =
  makeStakeAddress networkId <$> getStakeCredentialFromVerifier stakeVerifier
