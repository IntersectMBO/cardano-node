{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Shelley.Run
  ( runShelleyClientCommand
  ) where

import           Cardano.Prelude hiding (option, trace)

import qualified Data.Set as Set

import           Cardano.Api (ShelleyCoin, ShelleyStakePoolMargin, ShelleyStakePoolRelay,
                   mkShelleyStakingCredential, readVerificationKeyStaking, readVRFVerificationKey,
                   readVerificationKeyStakePool, shelleyDeregisterStakingAddress,
                   shelleyDelegateStake, shelleyRegisterStakePool,
                   shelleyRegisterStakingAddress, shelleyRetireStakePool, writeCertificate)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.CLI.Key (VerificationKeyFile(..))
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers

import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Shelley.Run.Genesis
import           Cardano.CLI.Shelley.Run.KeyGen
import           Cardano.CLI.Shelley.Run.Query (runQueryCmd)
import           Cardano.CLI.Shelley.Run.TextView (runTextViewCmd)
import           Cardano.CLI.Shelley.Run.Transaction (runTransactionCmd)

import           Cardano.Config.Shelley.ColdKeys
import           Cardano.Config.Shelley.KES
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF
import           Cardano.Config.Types (SigningKeyFile(..))

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.Keys (hash, hashKey)
import qualified Shelley.Spec.Ledger.Slot as Shelley


--
-- CLI shelley command dispatch
--

runShelleyClientCommand :: ShelleyCommand -> ExceptT CliError IO ()
runShelleyClientCommand (AddressCmd      cmd) = runAddressCmd      cmd
runShelleyClientCommand (StakeAddressCmd cmd) = runStakeAddressCmd cmd
runShelleyClientCommand (TransactionCmd  cmd) = runTransactionCmd  cmd
runShelleyClientCommand (NodeCmd         cmd) = runNodeCmd         cmd
runShelleyClientCommand (PoolCmd         cmd) = runPoolCmd         cmd
runShelleyClientCommand (QueryCmd        cmd) = runQueryCmd        cmd
runShelleyClientCommand (BlockCmd        cmd) = runBlockCmd        cmd
runShelleyClientCommand (SystemCmd       cmd) = runSystemCmd       cmd
runShelleyClientCommand (DevOpsCmd       cmd) = runDevOpsCmd       cmd
runShelleyClientCommand (GenesisCmd      cmd) = runGenesisCmd      cmd
runShelleyClientCommand (TextViewCmd     cmd) = runTextViewCmd     cmd

--
-- CLI shelley subcommand dispatch
--

runStakeAddressCmd :: StakeAddressCmd -> ExceptT CliError IO ()

runStakeAddressCmd (StakeKeyRegistrationCert stkKeyVerKeyFp outputFp) =
  runStakeKeyRegistrationCert stkKeyVerKeyFp outputFp
runStakeAddressCmd (StakeKeyDelegationCert stkKeyVerKeyFp stkPoolVerKeyFp outputFp) =
  runStakeKeyDelegationCert stkKeyVerKeyFp stkPoolVerKeyFp outputFp
runStakeAddressCmd (StakeKeyDeRegistrationCert stkKeyVerKeyFp outputFp) =
  runStakeKeyDeRegistrationCert stkKeyVerKeyFp outputFp
runStakeAddressCmd cmd = liftIO $ putStrLn $ "runStakeAddressCmd: " ++ show cmd


runNodeCmd :: NodeCmd -> ExceptT CliError IO ()
runNodeCmd (NodeKeyGenCold vk sk ctr) = runNodeKeyGenCold vk sk ctr
runNodeCmd (NodeKeyGenKES  vk sk)     = runNodeKeyGenKES  vk sk
runNodeCmd (NodeKeyGenVRF  vk sk)     = runNodeKeyGenVRF  vk sk
runNodeCmd (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out


runPoolCmd :: PoolCmd -> ExceptT CliError IO ()
runPoolCmd (PoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays outfp) =
  runStakePoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays outfp
runPoolCmd (PoolRetirmentCert sPvkeyFp retireEpoch outfp) =
  runStakePoolRetirementCert sPvkeyFp retireEpoch outfp
runPoolCmd cmd = liftIO $ putStrLn $ "runPoolCmd: " ++ show cmd


runBlockCmd :: BlockCmd -> ExceptT CliError IO ()
runBlockCmd cmd = liftIO $ putStrLn $ "runBlockCmd: " ++ show cmd


runSystemCmd:: SystemCmd -> ExceptT CliError IO ()
runSystemCmd cmd = liftIO $ putStrLn $ "runSystemCmd: " ++ show cmd


runDevOpsCmd :: DevOpsCmd -> ExceptT CliError IO ()
runDevOpsCmd cmd = liftIO $ putStrLn $ "runDevOpsCmd: " ++ show cmd


runGenesisCmd :: GenesisCmd -> ExceptT CliError IO ()
runGenesisCmd (GenesisKeyGenGenesis vk sk) = runGenesisKeyGenGenesis vk sk
runGenesisCmd (GenesisKeyGenDelegate vk sk ctr) = runGenesisKeyGenDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenUTxO vk sk) = runGenesisKeyGenUTxO vk sk
runGenesisCmd (GenesisKeyHash vk) = runGenesisKeyHash vk
runGenesisCmd (GenesisVerKey vk sk) = runGenesisVerKey vk sk
runGenesisCmd (GenesisTxIn vk) = runGenesisTxIn vk
runGenesisCmd (GenesisAddr vk) = runGenesisAddr vk
runGenesisCmd (GenesisCreate gd gn un ms am) = runGenesisCreate gd gn un ms am

--
-- Node command implementations
--

runNodeKeyGenCold :: VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT CliError IO ()
runNodeKeyGenCold vkeyPath skeyPath (OpCertCounterFile ocertCtrPath) = do
    runColdKeyGen (OperatorKey StakePoolOperatorKey) vkeyPath skeyPath
    firstExceptT OperationalCertError $
      writeOperationalCertIssueCounter ocertCtrPath initialCounter
  where
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT CliError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT KESCliError $ do
      (vkey, skey) <- liftIO $ genKESKeyPair
      writeKESVerKey     vkeyPath vkey
      writeKESSigningKey skeyPath skey


runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT CliError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT VRFCliError $ do
      --FIXME: genVRFKeyPair genKESKeyPair results are in an inconsistent order
      (skey, vkey) <- liftIO genVRFKeyPair
      writeVRFVerKey     vkeyPath vkey
      writeVRFSigningKey skeyPath skey


runNodeIssueOpCert :: VerificationKeyFile
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> KESPeriod
                   -- ^ Start of the validity period for this certificate.
                   -> OutputFile
                   -> ExceptT CliError IO ()
runNodeIssueOpCert (VerificationKeyFile vkeyKESPath)
                   (SigningKeyFile skeyPath)
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do
    issueNumber <- firstExceptT OperationalCertError $
      readOperationalCertIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT KESCliError $
      readKESVerKey vkeyKESPath

    signKey <- firstExceptT KeyCliError $
      readSigningKey (OperatorKey StakePoolOperatorKey) skeyPath

    let cert = signOperationalCertificate
                 verKeyKes signKey
                 issueNumber kesPeriod
        vkey = deriveVerKey signKey

    firstExceptT OperationalCertError $ do
      -- Write the counter first, to reduce the chance of ending up with
      -- a new cert but without updating the counter.
      writeOperationalCertIssueCounter ocertCtrPath (succ issueNumber)
      writeOperationalCert certFile cert vkey

--
-- Stake address command implementations
--

runStakeKeyRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT CliError IO ()
runStakeKeyRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
  stakeVkey <- firstExceptT CardanoApiError . newExceptT $ readVerificationKeyStaking vkFp
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
  stakeVkey <- firstExceptT CardanoApiError . newExceptT $ readVerificationKeyStaking stkKey
  poolStakeVkey <- firstExceptT CardanoApiError . newExceptT $ readVerificationKeyStakePool poolVKey
  let delegCert = shelleyDelegateStake (hashKey stakeVkey) (hashKey poolStakeVkey)
  firstExceptT CardanoApiError . newExceptT $ writeCertificate outFp delegCert

runStakeKeyDeRegistrationCert :: VerificationKeyFile -> OutputFile -> ExceptT CliError IO ()
runStakeKeyDeRegistrationCert (VerificationKeyFile vkFp) (OutputFile oFp) = do
  stakeVkey <- firstExceptT CardanoApiError . newExceptT $ readVerificationKeyStaking vkFp
  let deRegCert = shelleyDeregisterStakingAddress (hashKey stakeVkey)
  firstExceptT CardanoApiError . newExceptT $ writeCertificate oFp deRegCert

--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCert
  :: VerificationKeyFile
  -- ^ Stake pool verification key.
  -> VerificationKeyFile
  -- ^ VRF Verification key.
  -> ShelleyCoin
  -- ^ Pool pledge.
  -> ShelleyCoin
  -- ^ Pool cost.
  -> ShelleyStakePoolMargin
  -- ^ Pool margin.
  -> VerificationKeyFile
  -- ^ Reward account verification staking key.
  -> [VerificationKeyFile]
  -- ^ Pool owner verification staking key(s).
  -> [ShelleyStakePoolRelay]
  -- ^ Stake pool relays.
  -> OutputFile
  -> ExceptT CliError IO ()
runStakePoolRegistrationCert
  (VerificationKeyFile sPvkeyFp)
  (VerificationKeyFile vrfVkeyFp)
  pldg
  pCost
  pMrgn
  (VerificationKeyFile rwdVerFp)
  ownerVerFps
  relays
  (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT CardanoApiError . newExceptT $ readVerificationKeyStakePool sPvkeyFp

    -- VRF verification key
    vrfVerKey <- firstExceptT CardanoApiError . newExceptT $ readVRFVerificationKey vrfVkeyFp

    -- Pool reward account
    rewardAcctVerKey <- firstExceptT CardanoApiError . newExceptT $ readVerificationKeyStaking rwdVerFp
    let rewardAccount = Shelley.mkRwdAcnt . mkShelleyStakingCredential $ hashKey rewardAcctVerKey

    -- Pool owner(s)
    sPoolOwnerVkeys <-  mapM (\(VerificationKeyFile fp) -> firstExceptT CardanoApiError $ newExceptT $ readVerificationKeyStaking fp) ownerVerFps
    let stakePoolOwners = Set.fromList $ map hashKey sPoolOwnerVkeys

    let registrationCert = shelleyRegisterStakePool
                             (hashKey stakePoolVerKey)
                             (hash vrfVerKey)
                             pldg
                             pCost
                             pMrgn
                             rewardAccount
                             stakePoolOwners
                             relays
                             Nothing

    firstExceptT CardanoApiError . newExceptT $ writeCertificate outfp registrationCert

runStakePoolRetirementCert
  :: VerificationKeyFile
  -> Shelley.EpochNo
  -> OutputFile
  -> ExceptT CliError IO ()
runStakePoolRetirementCert (VerificationKeyFile sPvkeyFp) retireEpoch (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT CardanoApiError . newExceptT $ readVerificationKeyStakePool sPvkeyFp

    let retireCert = shelleyRetireStakePool stakePoolVerKey retireEpoch

    firstExceptT CardanoApiError . newExceptT $ writeCertificate outfp retireCert
