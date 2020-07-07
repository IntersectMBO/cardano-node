module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)

import           Cardano.Api.TextView (TextViewDescription (..))
import           Cardano.Api.Typed

import           Cardano.CLI.Helpers
import           Cardano.CLI.Shelley.Parsers

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressConvError !ConversionError
  | ShelleyStakeAddressKeyPairError
      !Text
      -- ^ bech32 private key
      !Text
      -- ^ bech32 public key
  | ShelleyStakeAddressReadFileError !(FileError TextEnvelopeError)
  | ShelleyStakeAddressWriteFileError !(FileError ())
  deriving Show

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Text
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeAddressConvError convErr -> renderConversionError convErr
    ShelleyStakeAddressKeyPairError bech32PrivKey bech32PubKey ->
      "Error while deriving the shelley verification key from bech32 private Key: " <> bech32PrivKey <>
      " Corresponding bech32 public key: " <> bech32PubKey
    ShelleyStakeAddressReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressWriteFileError fileErr -> Text.pack (displayError fileErr)


runStakeAddressCmd :: StakeAddressCmd -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGen vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild vk nw mOutputFp) = runStakeAddressBuild vk nw mOutputFp
runStakeAddressCmd (StakeKeyRegistrationCert stkKeyVerKeyFp outputFp) =
  runStakeKeyRegistrationCert stkKeyVerKeyFp outputFp
runStakeAddressCmd (StakeKeyDelegationCert stkKeyVerKeyFp stkPoolVerKeyFp outputFp) =
  runStakeKeyDelegationCert stkKeyVerKeyFp stkPoolVerKeyFp outputFp
runStakeAddressCmd (StakeKeyDeRegistrationCert stkKeyVerKeyFp outputFp) =
  runStakeKeyDeRegistrationCert stkKeyVerKeyFp outputFp
runStakeAddressCmd (StakeKeyITNConversion itnKeyFile mOutFile) = runSingleITNKeyConversion itnKeyFile mOutFile
runStakeAddressCmd cmd = liftIO $ putStrLn $ "runStakeAddressCmd: " ++ show cmd


--
-- Stake address command implementations
--

runStakeAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyGen (VerificationKeyFile vkFp) (SigningKeyFile skFp) = do
    skey <- liftIO $ generateSigningKey AsStakeKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyStakeAddressWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skFp (Just skeyDesc) skey
    firstExceptT ShelleyStakeAddressWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkFp (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewDescription
    skeyDesc = TextViewDescription "Stake Signing Key"
    vkeyDesc = TextViewDescription "Stake Verification Key"

runStakeAddressKeyHash :: VerificationKeyFile -> Maybe OutputFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyHash (VerificationKeyFile vkeyPath) mOutputFp = do
  vkey <- firstExceptT ShelleyStakeAddressReadFileError
    . newExceptT
    $ readFileTextEnvelope (AsVerificationKey AsStakeKey) vkeyPath

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuild :: VerificationKeyFile -> NetworkId -> Maybe OutputFile
                     -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild (VerificationKeyFile stkVkeyFp) network mOutputFp = do
    stakeVerKey <- firstExceptT ShelleyStakeAddressReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stkVkeyFp

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        stakeAddr = makeStakeAddress network stakeCred
        stakeAddrText = serialiseAddress stakeAddr

    case mOutputFp of
      Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath stakeAddrText
      Nothing -> liftIO $ Text.putStrLn stakeAddrText


runStakeKeyRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
    stakeVerKey <- firstExceptT ShelleyStakeAddressReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) vkFp
    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        regCert = makeStakeAddressRegistrationCertificate stakeCred
    firstExceptT ShelleyStakeAddressWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just regCertDesc) regCert
  where
    regCertDesc :: TextViewDescription
    regCertDesc = TextViewDescription "Stake Address Registration Certificate"


runStakeKeyDelegationCert
  :: VerificationKeyFile
  -- ^ Delegator stake verification key file.
  -> VerificationKeyFile
  -- ^ Delegatee stake pool verification key file.
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDelegationCert (VerificationKeyFile stkKey) (VerificationKeyFile poolVKey) (OutputFile outFp) = do
    stakeVkey <- firstExceptT ShelleyStakeAddressReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stkKey

    poolStakeVkey <- firstExceptT ShelleyStakeAddressReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) poolVKey

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
        delegCert = makeStakeAddressDelegationCertificate
                      stakeCred
                      (verificationKeyHash poolStakeVkey)
    firstExceptT ShelleyStakeAddressWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outFp (Just delegCertDesc) delegCert
  where
    delegCertDesc :: TextViewDescription
    delegCertDesc = TextViewDescription "Stake Address Delegation Certificate"


runStakeKeyDeRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDeRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
    stakeVkey <- firstExceptT ShelleyStakeAddressReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) vkFp
    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
        deRegCert = makeStakeAddressDeregistrationCertificate stakeCred
    firstExceptT ShelleyStakeAddressWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just deregCertDesc) deRegCert
  where
    deregCertDesc :: TextViewDescription
    deregCertDesc = TextViewDescription "Stake Address Deregistration Certificate"


runSingleITNKeyConversion
  :: ITNKeyFile
  -> Maybe OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runSingleITNKeyConversion (ITNVerificationKeyFile (VerificationKeyFile vk)) mOutFile = do
  bech32publicKey <- firstExceptT ShelleyStakeAddressConvError . newExceptT $ readBech32 vk
  vkey <- hoistEither
    . first ShelleyStakeAddressConvError
    $ convertITNVerificationKey bech32publicKey
  case mOutFile of
    Just (OutputFile fp) ->
      firstExceptT ShelleyStakeAddressWriteFileError
        . newExceptT
        $ writeFileTextEnvelope fp Nothing vkey
    Nothing -> print vkey

runSingleITNKeyConversion (ITNSigningKeyFile (SigningKeyFile sk)) mOutFile = do
  bech32privateKey <- firstExceptT ShelleyStakeAddressConvError . newExceptT $ readBech32 sk
  skey <- hoistEither
    . first ShelleyStakeAddressConvError
    $ convertITNSigningKey bech32privateKey
  case mOutFile of
    Just (OutputFile fp) ->
      firstExceptT ShelleyStakeAddressWriteFileError
        . newExceptT
        $ writeFileTextEnvelope fp Nothing skey
    Nothing -> print skey
