module Cardano.CLI.Shelley.Run.StakeAddress
  ( runStakeAddressCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api

import           Cardano.Api (StakingVerificationKey (..),
                   readStakingVerificationKey, readVerificationKeyStakePool,
                   shelleyDeregisterStakingAddress, shelleyDelegateStake,
                   shelleyRegisterStakingAddress, writeCertificate)
import           Shelley.Spec.Ledger.Keys (hashKey)
import           Cardano.Config.Shelley.ColdKeys (genKeyPair)

import           Cardano.CLI.Errors (CliError(..))
import           Cardano.CLI.Shelley.Parsers


runStakeAddressCmd :: StakeAddressCmd -> ExceptT CliError IO ()

runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGen vk sk
runStakeAddressCmd (StakeAddressBuild vk) = runStakeAddressBuild vk
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

runStakeAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT CliError IO ()
runStakeAddressKeyGen (VerificationKeyFile vkFp) (SigningKeyFile skFp) = do
  (vkey, skey) <- liftIO genKeyPair
  firstExceptT CardanoApiError
    . newExceptT
    $ writeStakingVerificationKey vkFp (StakingVerificationKeyShelley vkey)
  --TODO: writeSigningKey should really come from Cardano.Config.Shelley.ColdKeys
  firstExceptT CardanoApiError . newExceptT $ writeSigningKey skFp (SigningKeyShelley skey)


runStakeAddressBuild :: VerificationKeyFile -> ExceptT CliError IO ()
runStakeAddressBuild (VerificationKeyFile stkVkeyFp) =
  firstExceptT CardanoApiError $ do
    stkVKey <- ExceptT $ readStakingVerificationKey stkVkeyFp
    let rwdAddr = AddressShelleyReward $ shelleyVerificationKeyRewardAddress stkVKey
    liftIO . Text.putStrLn $ addressToHex rwdAddr


runStakeKeyRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT CliError IO ()
runStakeKeyRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
  StakingVerificationKeyShelley stakeVkey <-
    firstExceptT CardanoApiError . newExceptT $ readStakingVerificationKey vkFp
  let regCert = shelleyRegisterStakingAddress (hashKey stakeVkey)
  firstExceptT CardanoApiError . newExceptT $ writeCertificate oFp regCert


runStakeKeyDelegationCert
  :: VerificationKeyFile
  -- ^ Delegator staking verification key file.
  -> VerificationKeyFile
  -- ^ Delegatee stake pool verification key file.
  -> OutputFile
  -> ExceptT CliError IO ()
runStakeKeyDelegationCert (VerificationKeyFile stkKey) (VerificationKeyFile poolVKey) (OutputFile outFp) = do
  StakingVerificationKeyShelley stakeVkey <-
    firstExceptT CardanoApiError . newExceptT $ readStakingVerificationKey stkKey
  poolStakeVkey <- firstExceptT CardanoApiError . newExceptT $ readVerificationKeyStakePool poolVKey
  let delegCert = shelleyDelegateStake (hashKey stakeVkey) (hashKey poolStakeVkey)
  firstExceptT CardanoApiError . newExceptT $ writeCertificate outFp delegCert


runStakeKeyDeRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT CliError IO ()
runStakeKeyDeRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
  StakingVerificationKeyShelley stakeVkey <-
    firstExceptT CardanoApiError . newExceptT $ readStakingVerificationKey vkFp
  let deRegCert = shelleyDeregisterStakingAddress (hashKey stakeVkey)
  firstExceptT CardanoApiError . newExceptT $ writeCertificate oFp deRegCert

