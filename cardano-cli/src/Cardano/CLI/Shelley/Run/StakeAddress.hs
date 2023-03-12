{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError(ShelleyStakeAddressCmdReadKeyFileError)
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  , runStakeAddressKeyGenToFile
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, onLeft)
import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (StakeAddressSource (..), StakeVerifier (..),
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
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGenToFile vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild stakeAddressSource nw mOutputFp) =
  runStakeAddressBuild stakeAddressSource nw mOutputFp
runStakeAddressCmd (StakeRegistrationCert stakeAddressSource outputFp) =
  runStakeCredentialRegistrationCert stakeAddressSource outputFp
runStakeAddressCmd (StakeCredentialDelegationCert stakeAddressSource stkPoolVerKeyHashOrFp outputFp) =
  runStakeCredentialDelegationCert stakeAddressSource stkPoolVerKeyHashOrFp outputFp
runStakeAddressCmd (StakeCredentialDeRegistrationCert stakeAddressSource outputFp) =
  runStakeCredentialDeRegistrationCert stakeAddressSource outputFp


--
-- Stake address command implementations
--

runStakeAddressKeyGenToFile
  :: VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyGenToFile (VerificationKeyFile vkFp) (SigningKeyFile skFp) = do
  let skeyDesc = "Stake Signing Key"
  let vkeyDesc = "Stake Verification Key"

  skey <- liftIO $ generateSigningKey AsStakeKey

  let vkey = getVerificationKey skey

  firstExceptT ShelleyStakeAddressCmdWriteFileError $ do
    newExceptT $ writeFileTextEnvelope skFp (Just skeyDesc) skey
    newExceptT $ writeFileTextEnvelope vkFp (Just vkeyDesc) vkey

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
  :: StakeAddressSource
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild stakeAddressSource network mOutputFp = do
  case stakeAddressSource of
    StakeAddressSourceOfStakeVerifier stakeVerifier -> do
      stakeAddr <- getStakeAddressFromVerifier network stakeVerifier
      let stakeAddrText = serialiseAddress stakeAddr

      case mOutputFp of
        Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath stakeAddrText
        Nothing -> liftIO $ Text.putStrLn stakeAddrText

    StakeAddressSourceOfStakeKeyHash stakeKeyHash -> do
          let stakeCred = StakeCredentialByKey stakeKeyHash
              stakeAddr = makeStakeAddress network stakeCred
              stakeAddrText = serialiseAddress stakeAddr
          case mOutputFp of
            Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath stakeAddrText
            Nothing -> liftIO $ Text.putStrLn stakeAddrText


runStakeCredentialRegistrationCert
  :: StakeAddressSource
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialRegistrationCert stakeAddressSource (OutputFile oFp) = do
  case stakeAddressSource of
    StakeAddressSourceOfStakeVerifier stakeVerifier -> do
      stakeCred <- getStakeCredentialFromVerifier stakeVerifier
      writeRegistrationCert stakeCred
    StakeAddressSourceOfStakeKeyHash stakeKeyHash ->
      writeRegistrationCert (StakeCredentialByKey stakeKeyHash)

  where
    writeRegistrationCert
      :: StakeCredential
      -> ExceptT ShelleyStakeAddressCmdError IO ()
    writeRegistrationCert sCred = do
      let deRegCert = makeStakeAddressRegistrationCertificate sCred
      lift (writeFileTextEnvelope oFp (Just regCertDesc) deRegCert)
        & onLeft (left . ShelleyStakeAddressCmdWriteFileError)

    regCertDesc :: TextEnvelopeDescr
    regCertDesc = "Stake Address Registration Certificate"


runStakeCredentialDelegationCert
  :: StakeAddressSource
  -- ^ Delegator stake verification key, verification key file or script file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDelegationCert stakeAddressSource poolVKeyOrHashOrFile (OutputFile outFp) = do
  poolStakeVKeyHash <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile

  case stakeAddressSource of
    StakeAddressSourceOfStakeVerifier stakeVerifier -> do
        stakeCred <- getStakeCredentialFromVerifier stakeVerifier
        writeDelegationCert stakeCred poolStakeVKeyHash

    StakeAddressSourceOfStakeKeyHash stakeKeyHash ->
      writeDelegationCert (StakeCredentialByKey stakeKeyHash) poolStakeVKeyHash

  where
    writeDelegationCert
      :: StakeCredential
      -> Hash StakePoolKey
      -> ExceptT ShelleyStakeAddressCmdError IO ()
    writeDelegationCert sCred poolStakeVKeyHash = do
      let delegCert = makeStakeAddressDelegationCertificate sCred poolStakeVKeyHash

      lift (writeFileTextEnvelope outFp (Just delegCertDesc) delegCert)
        & onLeft (left . ShelleyStakeAddressCmdWriteFileError)

    delegCertDesc :: TextEnvelopeDescr
    delegCertDesc = "Stake Address Delegation Certificate"


runStakeCredentialDeRegistrationCert
  :: StakeAddressSource
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDeRegistrationCert stakeAddressSource (OutputFile oFp) = do
  case stakeAddressSource of
    StakeAddressSourceOfStakeVerifier stakeVerifier -> do
      stakeCred <- getStakeCredentialFromVerifier stakeVerifier
      writeDeregistrationCert stakeCred

    StakeAddressSourceOfStakeKeyHash stakeKeyHash ->
      writeDeregistrationCert (StakeCredentialByKey stakeKeyHash)
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


getStakeCredentialFromVerifier
  :: StakeVerifier -> ExceptT ShelleyStakeAddressCmdError IO StakeCredential
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

  StakeVerifierAddress stakeAddr -> pure $ stakeAddressCredential stakeAddr

getStakeAddressFromVerifier
  :: NetworkId
  -> StakeVerifier
  -> ExceptT ShelleyStakeAddressCmdError IO StakeAddress
getStakeAddressFromVerifier networkId = \case
  StakeVerifierAddress stakeAddr -> pure stakeAddr
  stakeVerifier ->
    makeStakeAddress networkId <$> getStakeCredentialFromVerifier stakeVerifier
