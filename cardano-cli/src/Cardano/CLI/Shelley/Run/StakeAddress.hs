module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api

import           Cardano.Api (StakingVerificationKey (..),
                   readStakingVerificationKey,
                   shelleyDeregisterStakingAddress, shelleyDelegateStake,
                   shelleyRegisterStakingAddress, writeCertificate)
import           Shelley.Spec.Ledger.Keys (hashKey)
import           Cardano.Config.Shelley.ColdKeys hiding (writeSigningKey)

import           Cardano.CLI.Helpers (textToLByteString)
import           Cardano.CLI.Shelley.Parsers

data ShelleyStakeAddressCmdError
  = ShelleyStakeReadPoolOperatorKeyError !FilePath !KeyError
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
    ShelleyStakeAddressReadVerKeyError fp apiErr ->
      "Error while reading verification stake key at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressWriteCertError fp apiErr ->
      "Error while writing delegation certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressWriteSignKeyError fp apiErr ->
      "Error while writing signing stake key at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyStakeAddressWriteVerKeyError fp apiErr ->
      "Error while writing verification stake key at: " <> textShow fp <> " Error: " <> renderApiError apiErr

runStakeAddressCmd :: StakeAddressCmd -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGen vk sk
runStakeAddressCmd (StakeAddressBuild vk mOutputFp) = runStakeAddressBuild vk mOutputFp
runStakeAddressCmd (StakeKeyRegistrationCert stkKeyVerKeyFp outputFp) =
  runStakeKeyRegistrationCert stkKeyVerKeyFp outputFp
runStakeAddressCmd (StakeKeyDelegationCert stkKeyVerKeyFp stkPoolVerKeyFp outputFp) =
  runStakeKeyDelegationCert stkKeyVerKeyFp stkPoolVerKeyFp outputFp
runStakeAddressCmd (StakeKeyDeRegistrationCert stkKeyVerKeyFp outputFp) =
  runStakeKeyDeRegistrationCert stkKeyVerKeyFp outputFp
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
  --TODO: writeSigningKey should really come from Cardano.Config.Shelley.ColdKeys
  firstExceptT (ShelleyStakeAddressWriteSignKeyError skFp) . newExceptT $ writeSigningKey skFp (SigningKeyShelley skey)


runStakeAddressBuild :: VerificationKeyFile -> Maybe OutputFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild (VerificationKeyFile stkVkeyFp) mOutputFp =
  firstExceptT (ShelleyStakeAddressReadVerKeyError stkVkeyFp) $ do
    stkVKey <- ExceptT $ readStakingVerificationKey stkVkeyFp
    let rwdAddr = AddressShelleyReward $ shelleyVerificationKeyRewardAddress stkVKey
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
