{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError(ShelleyStakeAddressCmdReadKeyFileError)
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  , runStakeAddressKeyGenToFile
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (StakeIdentifier (..), StakeVerifier (..),
                   VerificationKeyOrFile, VerificationKeyOrHashOrFile, readVerificationKeyOrFile,
                   readVerificationKeyOrHashOrFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Read
import           Cardano.CLI.Types

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyStakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyStakeAddressCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Text
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeAddressCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressCmdReadScriptFileError fileErr -> Text.pack (displayError fileErr)

runStakeAddressCmd :: StakeAddressCmd -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen fmt vk sk) = runStakeAddressKeyGenToFile fmt vk sk
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
  :: KeyOutputFormat
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyGenToFile fmt (VerificationKeyFile vkFp) (SigningKeyFile skFp) = do
  let skeyDesc = "Stake Signing Key"
  let vkeyDesc = "Stake Verification Key"

  skey <- liftIO $ generateSigningKey AsStakeKey

  let vkey = getVerificationKey skey

  firstExceptT ShelleyStakeAddressCmdWriteFileError $ do
    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT $ writeLazyByteStringFile skFp $ textEnvelopeToJSON (Just skeyDesc) skey
      KeyOutputFormatBech32 ->
        newExceptT $ writeTextFile skFp $ serialiseToBech32 skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT $ writeLazyByteStringFile vkFp $ textEnvelopeToJSON (Just vkeyDesc) vkey
      KeyOutputFormatBech32 ->
        newExceptT $ writeTextFile vkFp $ serialiseToBech32 vkey

runStakeAddressKeyHash
  :: VerificationKeyOrFile StakeKey
  -> Maybe OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyHash stakeVerKeyOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuild
  :: StakeVerifier
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild stakeVerifier network mOutputFp = do
  stakeAddr <- getStakeAddressFromVerifier network stakeVerifier
  let stakeAddrText = serialiseAddress stakeAddr
  liftIO $
    case mOutputFp of
      Just (OutputFile fpath) -> Text.writeFile fpath stakeAddrText
      Nothing -> Text.putStrLn stakeAddrText


runStakeCredentialRegistrationCert
  :: StakeIdentifier
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialRegistrationCert stakeIdentifier (OutputFile oFp) = do
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
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDelegationCert stakeVerifier poolVKeyOrHashOrFile (OutputFile outFp) = do
  poolStakeVKeyHash <-
    firstExceptT
      ShelleyStakeAddressCmdReadKeyFileError
      (newExceptT $ readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)
  stakeCred <- getStakeCredentialFromIdentifier stakeVerifier
  writeDelegationCert stakeCred poolStakeVKeyHash

  where
    writeDelegationCert
      :: StakeCredential
      -> Hash StakePoolKey
      -> ExceptT ShelleyStakeAddressCmdError IO ()
    writeDelegationCert sCred poolStakeVKeyHash = do
      let delegCert = makeStakeAddressDelegationCertificate sCred poolStakeVKeyHash
      firstExceptT ShelleyStakeAddressCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ textEnvelopeToJSON (Just delegCertDesc) delegCert

    delegCertDesc :: TextEnvelopeDescr
    delegCertDesc = "Stake Address Delegation Certificate"


runStakeCredentialDeRegistrationCert
  :: StakeIdentifier
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDeRegistrationCert stakeVerifier (OutputFile oFp) = do
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
