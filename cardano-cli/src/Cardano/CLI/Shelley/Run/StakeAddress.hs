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
import           Control.Monad.Trans.Except.Extra (bimapExceptT, firstExceptT, newExceptT)

import           Cardano.Api.TextView (TextViewDescription (..))
import           Cardano.Api.Typed

import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressCmdStakeAddressKeyPairError
      !Text
      -- ^ bech32 private key
      !Text
      -- ^ bech32 public key
  | ShelleyStakeAddressCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyStakeAddressCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Text
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeAddressCmdStakeAddressKeyPairError bech32PrivKey bech32PubKey ->
      "Error while deriving the shelley verification key from bech32 private Key: " <> bech32PrivKey <>
      " Corresponding bech32 public key: " <> bech32PubKey
    ShelleyStakeAddressCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)


runStakeAddressCmd :: StakeAddressCmd -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGen vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild vk nw mOutputFp) = runStakeAddressBuild vk nw mOutputFp
runStakeAddressCmd (StakeKeyRegistrationCert stkKeyVerKeyFp outputFp) =
  runStakeKeyRegistrationCert stkKeyVerKeyFp outputFp
runStakeAddressCmd (StakeKeyDelegationCert stkKeyVerKeyFp stkPoolVerKeyHashOrFp outputFp) =
  runStakeKeyDelegationCert stkKeyVerKeyFp stkPoolVerKeyHashOrFp outputFp
runStakeAddressCmd (StakeKeyDeRegistrationCert stkKeyVerKeyFp outputFp) =
  runStakeKeyDeRegistrationCert stkKeyVerKeyFp outputFp


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
    skeyDesc, vkeyDesc :: TextViewDescription
    skeyDesc = TextViewDescription "Stake Signing Key"
    vkeyDesc = TextViewDescription "Stake Verification Key"

runStakeAddressKeyHash :: VerificationKeyFile -> Maybe OutputFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyHash (VerificationKeyFile vkeyPath) mOutputFp = do
  vkey <- firstExceptT ShelleyStakeAddressCmdReadFileError
    . newExceptT
    $ readFileTextEnvelope (AsVerificationKey AsStakeKey) vkeyPath

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuild :: VerificationKeyFile -> NetworkId -> Maybe OutputFile
                     -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild (VerificationKeyFile stkVkeyFp) network mOutputFp = do
    stakeVerKey <- firstExceptT ShelleyStakeAddressCmdReadFileError
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
    stakeVerKey <- firstExceptT ShelleyStakeAddressCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) vkFp
    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        regCert = makeStakeAddressRegistrationCertificate stakeCred
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just regCertDesc) regCert
  where
    regCertDesc :: TextViewDescription
    regCertDesc = TextViewDescription "Stake Address Registration Certificate"


runStakeKeyDelegationCert
  :: VerificationKeyFile
  -- ^ Delegator stake verification key file.
  -> StakePoolVerificationKeyHashOrFile
  -- ^ Delegatee stake pool verification key hash or file.
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDelegationCert (VerificationKeyFile stkKey) poolVKeyHashOrFile (OutputFile outFp) = do
    stakeVkey <- firstExceptT ShelleyStakeAddressCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stkKey

    poolStakeVKeyHash <-
      case poolVKeyHashOrFile of
        StakePoolVerificationKeyHash hash -> pure hash
        StakePoolVerificationKeyFile (VerificationKeyFile fp) ->
          bimapExceptT
            ShelleyStakeAddressCmdReadFileError
            verificationKeyHash
            (newExceptT $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) fp)

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
        delegCert = makeStakeAddressDelegationCertificate
                      stakeCred
                      poolStakeVKeyHash
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outFp (Just delegCertDesc) delegCert
  where
    delegCertDesc :: TextViewDescription
    delegCertDesc = TextViewDescription "Stake Address Delegation Certificate"


runStakeKeyDeRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDeRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
    stakeVkey <- firstExceptT ShelleyStakeAddressCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) vkFp
    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
        deRegCert = makeStakeAddressDeregistrationCertificate stakeCred
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just deregCertDesc) deRegCert
  where
    deregCertDesc :: TextViewDescription
    deregCertDesc = TextViewDescription "Stake Address Deregistration Certificate"
