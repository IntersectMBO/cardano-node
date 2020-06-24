module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError
  , checkKeyPair
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT)

import           Cardano.Api

import           Cardano.Api (StakingVerificationKey (..),
                   readStakingVerificationKey,
                   shelleyDeregisterStakingAddress, shelleyDelegateStake,
                   shelleyRegisterStakingAddress, writeCertificate)
import           Shelley.Spec.Ledger.Keys (VKey(..), hashKey)
import           Cardano.Api.Shelley.ColdKeys hiding (writeSigningKey)
import qualified Cardano.Crypto.DSIGN as DSIGN


import           Cardano.CLI.Helpers
import           Cardano.CLI.Shelley.Parsers

data ShelleyStakeAddressCmdError
  = ShelleyStakeReadPoolOperatorKeyError !FilePath !KeyError
  | ShelleyStakeAddressConvError !ConversionError
  | ShelleyStakeAddressKeyPairError
      !Text
      -- ^ bech32 private key
      !Text
      -- ^ bech32 public key
  | ShelleyStakeAddressReadFileError !FilePath !Text
  | ShelleyStakeAddressReadVerKeyError !FilePath !ApiError
  | ShelleyStakeAddressWriteCertError !FilePath !ApiError
  | ShelleyStakeAddressWriteSignKeyError !FilePath !ApiError
  | ShelleyStakeAddressWriteVerKeyError !FilePath !ApiError
  deriving Show

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Text
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeReadPoolOperatorKeyError fp keyErr ->
      "Error reading pool operator key at: " <> textShow fp <> " Error: " <> renderKeyError keyErr
    ShelleyStakeAddressConvError convErr -> renderConversionError convErr
    ShelleyStakeAddressReadFileError fp readErr ->
      "Error reading file at: " <> textShow fp <> " Error: " <> readErr
    ShelleyStakeAddressReadVerKeyError fp apiErr ->
      "Error while reading verification stake key at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressWriteCertError fp apiErr ->
      "Error while writing delegation certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressWriteSignKeyError fp apiErr ->
      "Error while writing signing stake key at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressWriteVerKeyError fp apiErr ->
      "Error while writing verification stake key at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressKeyPairError bech32PrivKey bech32PubKey ->
      "Error while deriving the shelley verification key from bech32 private Key: " <> bech32PrivKey <>
      " Corresponding bech32 public key: " <> bech32PubKey


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
  (vkey, skey) <- liftIO genKeyPair
  firstExceptT (ShelleyStakeAddressWriteVerKeyError vkFp)
    . newExceptT
    $ writeStakingVerificationKey vkFp (StakingVerificationKeyShelley vkey)
  --TODO: writeSigningKey should really come from Cardano.Api.Shelley.ColdKeys
  firstExceptT (ShelleyStakeAddressWriteSignKeyError skFp) . newExceptT $ writeSigningKey skFp (SigningKeyShelley skey)


runStakeAddressBuild :: VerificationKeyFile -> Network -> Maybe OutputFile
                     -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild (VerificationKeyFile stkVkeyFp) network mOutputFp =
  firstExceptT (ShelleyStakeAddressReadVerKeyError stkVkeyFp) $ do
    stkVKey <- ExceptT $ readStakingVerificationKey stkVkeyFp
    let rwdAddr = AddressShelleyReward (shelleyVerificationKeyRewardAddress network stkVKey)
        hexAddr = addressToHex rwdAddr
    case mOutputFp of
      Just (OutputFile fpath) -> liftIO . LBS.writeFile fpath $ textToLByteString hexAddr
      Nothing -> liftIO $ Text.putStrLn hexAddr


runStakeKeyRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
  StakingVerificationKeyShelley stakeVkey <-
    firstExceptT (ShelleyStakeAddressReadVerKeyError vkFp) . newExceptT $ readStakingVerificationKey vkFp
  let regCert = shelleyRegisterStakingAddress (hashKey stakeVkey)
  firstExceptT (ShelleyStakeAddressWriteCertError oFp) . newExceptT $ writeCertificate oFp regCert


runStakeKeyDelegationCert
  :: VerificationKeyFile
  -- ^ Delegator stake verification key file.
  -> VerificationKeyFile
  -- ^ Delegatee stake pool verification key file.
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDelegationCert (VerificationKeyFile stkKey) (VerificationKeyFile poolVKey) (OutputFile outFp) = do
  StakingVerificationKeyShelley stakeVkey <-
    firstExceptT (ShelleyStakeAddressReadVerKeyError stkKey) . newExceptT $ readStakingVerificationKey stkKey
  poolStakeVkey <- firstExceptT (ShelleyStakeReadPoolOperatorKeyError poolVKey) $
    readVerKey (OperatorKey StakePoolOperatorKey) poolVKey
  let delegCert = shelleyDelegateStake (hashKey stakeVkey) (hashKey poolStakeVkey)
  firstExceptT (ShelleyStakeAddressWriteCertError outFp) . newExceptT $ writeCertificate outFp delegCert


runStakeKeyDeRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDeRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
  StakingVerificationKeyShelley stakeVkey <-
    firstExceptT (ShelleyStakeAddressReadVerKeyError vkFp)  . newExceptT $ readStakingVerificationKey vkFp
  let deRegCert = shelleyDeregisterStakingAddress (hashKey stakeVkey)
  firstExceptT (ShelleyStakeAddressWriteCertError oFp) . newExceptT $ writeCertificate oFp deRegCert



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
