module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError(ShelleyStakeAddressCmdReadKeyFileError)
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  , runStakeAddressKeyGen
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (InputDecodeError, StakeVerifier (..),
                   VerificationKeyOrFile, VerificationKeyOrHashOrFile, readVerificationKeyOrFile,
                   readVerificationKeyOrHashOrFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Script (ScriptDecodeError, readFileScriptInAnyLang)
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
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGen vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild vk nw mOutputFp) = runStakeAddressBuild vk nw mOutputFp
runStakeAddressCmd (StakeRegistrationCert stakeVerifier outputFp) =
  runStakeCredentialRegistrationCert stakeVerifier outputFp
runStakeAddressCmd (StakeKeyDelegationCert stkKeyVerKeyOrFp stkPoolVerKeyHashOrFp outputFp) =
  runStakeKeyDelegationCert stkKeyVerKeyOrFp stkPoolVerKeyHashOrFp outputFp
runStakeAddressCmd (StakeCredentialDeRegistrationCert stakeVerifier outputFp) =
  runStakeCredentialDeRegistrationCert stakeVerifier outputFp


--
-- Stake address command implementations
--

runStakeAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyGen (VerificationKeyFile vkFp) (SigningKeyFile skFp) = do
    skey <- liftIO $ generateSigningKey AsStakeKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skFp (Just skeyDesc) skey
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkFp (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Signing Key"
    vkeyDesc = "Stake Verification Key"

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

runStakeAddressBuild :: VerificationKeyOrFile StakeKey -> NetworkId -> Maybe OutputFile
                     -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild stakeVerKeyOrFile network mOutputFp = do
    stakeVerKey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        stakeAddr = makeStakeAddress network stakeCred
        stakeAddrText = serialiseAddress stakeAddr

    case mOutputFp of
      Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath stakeAddrText
      Nothing -> liftIO $ Text.putStrLn stakeAddrText


runStakeCredentialRegistrationCert
  :: StakeVerifier
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialRegistrationCert stakeVerifier (OutputFile oFp) =
  case stakeVerifier of
    StakeVerifierScriptFile (ScriptFile sFile) -> do
      ScriptInAnyLang _ script <- firstExceptT ShelleyStakeAddressCmdReadScriptFileError
                                    $ readFileScriptInAnyLang sFile
      let stakeCred = StakeCredentialByScript $ hashScript script
      writeRegistrationCert stakeCred
    StakeVerifierKey stakeVerKeyOrFile -> do
      stakeVerKey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
      writeRegistrationCert stakeCred
 where
  writeRegistrationCert
    :: StakeCredential
    -> ExceptT ShelleyStakeAddressCmdError IO ()
  writeRegistrationCert sCred = do
    let deRegCert = makeStakeAddressRegistrationCertificate sCred
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just regCertDesc) deRegCert

  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"


runStakeKeyDelegationCert
  :: VerificationKeyOrFile StakeKey
  -- ^ Delegator stake verification key or verification key file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDelegationCert stakeVerKeyOrFile poolVKeyOrHashOrFile (OutputFile outFp) = do
    stakeVkey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

    poolStakeVKeyHash <-
      firstExceptT
        ShelleyStakeAddressCmdReadKeyFileError
        (newExceptT $ readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
        delegCert = makeStakeAddressDelegationCertificate
                      stakeCred
                      poolStakeVKeyHash
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outFp (Just delegCertDesc) delegCert
  where
    delegCertDesc :: TextEnvelopeDescr
    delegCertDesc = "Stake Address Delegation Certificate"


runStakeCredentialDeRegistrationCert
  :: StakeVerifier
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDeRegistrationCert stakeVerifier (OutputFile oFp) =
  case stakeVerifier of
    StakeVerifierScriptFile (ScriptFile sFile) -> do
      ScriptInAnyLang _ script <- firstExceptT ShelleyStakeAddressCmdReadScriptFileError
                                    $ readFileScriptInAnyLang sFile
      let stakeCred = StakeCredentialByScript $ hashScript script
      writeDeregistrationCert stakeCred
    StakeVerifierKey stakeVerKeyOrFile -> do
      stakeVkey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
      writeDeregistrationCert stakeCred
  where
    writeDeregistrationCert
      :: StakeCredential
      -> ExceptT ShelleyStakeAddressCmdError IO ()
    writeDeregistrationCert sCred = do
      let deRegCert = makeStakeAddressDeregistrationCertificate sCred
      firstExceptT ShelleyStakeAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope oFp (Just deregCertDesc) deRegCert

    deregCertDesc :: TextEnvelopeDescr
    deregCertDesc = "Stake Address Deregistration Certificate"
