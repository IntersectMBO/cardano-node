{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Monad law, left identity" -}

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
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Cardano.Api
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

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Text
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeAddressCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressCmdReadScriptFileError fileErr -> Text.pack (displayError fileErr)

runStakeAddressCmd :: StakeAddressCmd -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGenToFile vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild stakeVerifier nw mOutputFp) =
  runStakeAddressBuild stakeVerifier nw mOutputFp
runStakeAddressCmd (StakeRegistrationCert anyEra stakeIdentifier outputFp) =
  runStakeCredentialRegistrationCert anyEra stakeIdentifier outputFp
runStakeAddressCmd (StakeCredentialDelegationCert anyEra stakeIdentifier stkPoolVerKeyHashOrFp outputFp) =
  runStakeCredentialDelegationCert anyEra stakeIdentifier stkPoolVerKeyHashOrFp outputFp
runStakeAddressCmd (StakeCredentialDeRegistrationCert anyEra stakeIdentifier outputFp) =
  runStakeCredentialDeRegistrationCert anyEra stakeIdentifier outputFp


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
  :: AnyCardanoEra
  -> StakeIdentifier
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialRegistrationCert anyEra stakeIdentifier oFp = do
  AnyCardanoEra era <- pure anyEra
  stakeCred <- getStakeCredentialFromIdentifier stakeIdentifier
  writeRegistrationCert era stakeCred

 where
  writeRegistrationCert
    :: CardanoEra era
    -> StakeCredential
    -> ExceptT ShelleyStakeAddressCmdError IO ()
  writeRegistrationCert era sCred = do
    let deRegCert = makeStakeAddressRegistrationCertificate era sCred
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ cardanoEraConstraints era
      $ textEnvelopeToJSON (Just regCertDesc) deRegCert

  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"


runStakeCredentialDelegationCert
  :: AnyCardanoEra
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> DelegationTarget
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDelegationCert anyEra stakeVerifier delegationTarget outFp = do
  AnyCardanoEra era <- pure anyEra
  case delegationTarget of
    StakePoolDelegationTarget poolVKeyOrHashOrFile -> do
      poolStakeVKeyHash <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)
        & onLeft (left . ShelleyStakeAddressCmdReadKeyFileError)
      stakeCred <- getStakeCredentialFromIdentifier stakeVerifier
      let delegCert = makeStakeAddressPoolDelegationCertificate era stakeCred poolStakeVKeyHash
      firstExceptT ShelleyStakeAddressCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ cardanoEraConstraints era
        $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Address Delegation Certificate") delegCert

runStakeCredentialDeRegistrationCert
  :: AnyCardanoEra
  -> StakeIdentifier
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeCredentialDeRegistrationCert anyEra stakeVerifier oFp = do
  AnyCardanoEra era <- pure anyEra
  stakeCred <- getStakeCredentialFromIdentifier stakeVerifier
  writeDeregistrationCert era stakeCred

  where
    writeDeregistrationCert
      :: CardanoEra era
      -> StakeCredential
      -> ExceptT ShelleyStakeAddressCmdError IO ()
    writeDeregistrationCert era sCred = do
      let deRegCert = makeStakeAddressDeregistrationCertificate era sCred
      firstExceptT ShelleyStakeAddressCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile oFp
        $ cardanoEraConstraints era
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
