module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError
  , checkKeyPair
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT)

import           Cardano.Api (ApiError, Network (..), SigningKey (..),
                   StakingVerificationKey (..), renderApiError,
                   writeSigningKey, writeStakingVerificationKey)
import           Cardano.Api.TextView (TextViewTitle (..), textShow)
import qualified Cardano.Api.Typed as Api (NetworkId (..))
import           Cardano.Api.Typed (AsType (..), Error (..), FileError,
                   Key (..), StakeCredential (..), TextEnvelopeError,
                   generateSigningKey, getVerificationKey, makeStakeAddress,
                   makeStakeAddressDelegationCertificate,
                   makeStakeAddressDeregistrationCertificate,
                   makeStakeAddressRegistrationCertificate,
                   readFileTextEnvelope, serialiseToRawBytesHex,
                   writeFileTextEnvelope)

import qualified Cardano.Crypto.DSIGN as DSIGN

import           Shelley.Spec.Ledger.Keys (VKey(..))

import           Cardano.CLI.Helpers
import           Cardano.CLI.Shelley.Parsers

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressConvError !ConversionError
  | ShelleyStakeAddressKeyPairError
      !Text
      -- ^ bech32 private key
      !Text
      -- ^ bech32 public key
  | ShelleyStakeAddressWriteSignKeyError !FilePath !ApiError
  | ShelleyStakeAddressWriteVerKeyError !FilePath !ApiError
  | ShelleyStakeAddressReadFileError !(FileError TextEnvelopeError)
  | ShelleyStakeAddressWriteFileError !(FileError ())
  deriving Show

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Text
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeAddressConvError convErr -> renderConversionError convErr
    ShelleyStakeAddressWriteSignKeyError fp apiErr ->
      "Error while writing signing stake key at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressWriteVerKeyError fp apiErr ->
      "Error while writing verification stake key at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressKeyPairError bech32PrivKey bech32PubKey ->
      "Error while deriving the shelley verification key from bech32 private Key: " <> bech32PrivKey <>
      " Corresponding bech32 public key: " <> bech32PubKey
    ShelleyStakeAddressReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressWriteFileError fileErr -> Text.pack (displayError fileErr)


runStakeAddressCmd :: StakeAddressCmd -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGen vk sk
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
    skeyDesc, vkeyDesc :: TextViewTitle
    skeyDesc = TextViewTitle "Stake Signing Key"
    vkeyDesc = TextViewTitle "Stake Verification Key"

runStakeAddressBuild :: VerificationKeyFile -> Network -> Maybe OutputFile
                     -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild (VerificationKeyFile stkVkeyFp) network mOutputFp = do
    stakeVerKey <- firstExceptT ShelleyStakeAddressReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stkVkeyFp

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        rwdAddr = makeStakeAddress nwId stakeCred
        hexAddr = LBS.fromStrict (serialiseToRawBytesHex rwdAddr)

    case mOutputFp of
      Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath hexAddr
      Nothing -> liftIO $ LBS.putStrLn hexAddr
  where
    -- TODO: Remove this once we remove usage of 'Cardano.Api.Types.Network'
    --       from this module.
    nwId :: Api.NetworkId
    nwId =
      case network of
        Mainnet -> Api.Mainnet
        Testnet nm -> Api.Testnet nm


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
    regCertDesc :: TextViewTitle
    regCertDesc = TextViewTitle "Stake Address Registration Certificate"


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
        stakePoolId = verificationKeyHash poolStakeVkey
        delegCert = makeStakeAddressDelegationCertificate stakeCred stakePoolId
    firstExceptT ShelleyStakeAddressWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outFp (Just delegCertDesc) delegCert
  where
    delegCertDesc :: TextViewTitle
    delegCertDesc = TextViewTitle "Stake Address Delegation Certificate"


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
    deregCertDesc :: TextViewTitle
    deregCertDesc = TextViewTitle "Stake Address Deregistration Certificate"


runSingleITNKeyConversion
  :: ITNKeyFile
  -> Maybe OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runSingleITNKeyConversion (ITNVerificationKeyFile (VerificationKeyFile vk)) mOutFile = do
  bech32publicKey <- firstExceptT ShelleyStakeAddressConvError . newExceptT $ readBech32 vk
  v@(StakingVerificationKeyShelley (VKey _vkey)) <- hoistEither . first ShelleyStakeAddressConvError $ convertITNverificationKey bech32publicKey
  case mOutFile of
    Just (OutputFile fp) -> firstExceptT (ShelleyStakeAddressWriteVerKeyError fp) . newExceptT $ writeStakingVerificationKey fp v
    Nothing -> print v

runSingleITNKeyConversion (ITNSigningKeyFile (SigningKeyFile sk)) mOutFile = do
  bech32privateKey <- firstExceptT ShelleyStakeAddressConvError . newExceptT $ readBech32 sk
  s@(SigningKeyShelley _sKey) <- hoistEither . first ShelleyStakeAddressConvError $ convertITNsigningKey bech32privateKey
  case mOutFile of
    Just (OutputFile fp) -> firstExceptT (ShelleyStakeAddressWriteSignKeyError fp) . newExceptT $ writeSigningKey fp s
    Nothing -> print s

-- | Checks that the verification key corresponds to the given signing key
-- This does not need to be in 'IO' however the 'MonadFail' constraint
-- imposed by the ITN conversion functions forces us to use 'IO'
-- in order to report useful errors with 'Either'.
checkKeyPair
  :: Text
  -- ^ Bech32 public key
  -> Text
  -- ^ Bech32 private key
  -> ExceptT ShelleyStakeAddressCmdError IO (SigningKey, StakingVerificationKey)
checkKeyPair bech32publicKey bech32privateKey = do
  v@(StakingVerificationKeyShelley (VKey vkey)) <- hoistEither . first ShelleyStakeAddressConvError $ convertITNverificationKey bech32publicKey
  s@(SigningKeyShelley sKey) <- hoistEither . first ShelleyStakeAddressConvError $ convertITNsigningKey bech32privateKey

  if DSIGN.deriveVerKeyDSIGN sKey == vkey
  then return (s, v)
  else left $ ShelleyStakeAddressKeyPairError bech32privateKey bech32publicKey
