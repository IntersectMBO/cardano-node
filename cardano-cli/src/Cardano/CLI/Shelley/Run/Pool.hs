module Cardano.CLI.Shelley.Run.Pool
  ( ShelleyPoolCmdError
  , renderShelleyPoolCmdError
  , runPoolCmd
  ) where

import           Cardano.Prelude

import qualified Data.Set as Set

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, newExceptT)

import qualified Data.ByteString.Char8 as BS

import           Cardano.Api (ApiError(..), ShelleyCoin, ShelleyStakePoolMargin,
                   ShelleyStakePoolRelay, StakingVerificationKey (..),
                   ShelleyVerificationKeyHashStakePool,
                   Network, toShelleyNetwork,
                   mkShelleyStakingCredential, readStakingVerificationKey,
                   renderApiError, shelleyRegisterStakePool,
                   shelleyRetireStakePool, textShow, writeCertificate)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.Keys (KeyHash (..), hashKey, hashVerKeyVRF)
import qualified Shelley.Spec.Ledger.Slot as Shelley

import           Cardano.Config.Shelley.ColdKeys
import           Cardano.Config.Shelley.VRF
import           Cardano.Config.Types (PoolMetaDataFile (..))

import           Cardano.CLI.Shelley.Commands

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

data ShelleyPoolCmdError
  = ShelleyPoolReadStakeVerKeyError !FilePath !ApiError
  | ShelleyPoolReadStakePoolVerKeyError !FilePath !KeyError
  | ShelleyPoolReadVRFVerKeyError !FilePath !VRFError
  | ShelleyPoolReadMetaDataError !FilePath !IOException
  | ShelleyPoolWriteRegistrationCertError !FilePath !ApiError
  | ShelleyPoolWriteRetirementCertError !FilePath !ApiError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ShelleyPoolReadVRFVerKeyError fp vrfErr ->
      "Error reading VRF verification key at: " <> textShow fp <> " Error: " <> renderVRFError vrfErr
    ShelleyPoolReadStakePoolVerKeyError fp keyErr ->
      "Error reading stake pool verification key at: " <> textShow fp <> " Error: " <> renderKeyError keyErr
    ShelleyPoolReadStakeVerKeyError fp apiErr ->
      "Error reading stake verification key at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyPoolReadMetaDataError fp ioException ->
      "Error while reading pool metadata at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyPoolWriteRegistrationCertError fp apiErr ->
      "Error writing stake pool registration certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyPoolWriteRetirementCertError fp apiErr ->
      "Error writing stake pool retirement certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr



runPoolCmd :: PoolCmd -> ExceptT ShelleyPoolCmdError IO ()
runPoolCmd (PoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays network outfp) =
  runStakePoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays network outfp
runPoolCmd (PoolRetirementCert sPvkeyFp retireEpoch outfp) =
  runStakePoolRetirementCert sPvkeyFp retireEpoch outfp
runPoolCmd (PoolGetId sPvkey) = runPoolId sPvkey
runPoolCmd (PoolMetaDataHash poolMdFile) = runPoolMetaDataHash poolMdFile
runPoolCmd cmd = liftIO $ putStrLn $ "runPoolCmd: " ++ show cmd


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
  -- ^ Stake verification key for reward account.
  -> [VerificationKeyFile]
  -- ^ Pool owner stake verification key(s).
  -> [ShelleyStakePoolRelay]
  -- ^ Stake pool relays.
  -> Network
  -> OutputFile
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRegistrationCert
  (VerificationKeyFile sPvkeyFp)
  (VerificationKeyFile vrfVkeyFp)
  pldg
  pCost
  pMrgn
  (VerificationKeyFile rwdVerFp)
  ownerVerFps
  relays
  network
  (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT (ShelleyPoolReadStakePoolVerKeyError sPvkeyFp) $
      readVerKey (OperatorKey StakePoolOperatorKey) sPvkeyFp

    -- VRF verification key
    -- TODO: VRF key reading and writing has two versions and needs to be sorted out.
    vrfVerKey <- firstExceptT (ShelleyPoolReadVRFVerKeyError vrfVkeyFp) $ readVRFVerKey vrfVkeyFp

    -- Pool reward account
    StakingVerificationKeyShelley rewardAcctVerKey <-
      firstExceptT (ShelleyPoolReadStakeVerKeyError rwdVerFp)  . newExceptT $ readStakingVerificationKey rwdVerFp
    let rewardAccount = Shelley.mkRwdAcnt (toShelleyNetwork network)
                      . mkShelleyStakingCredential
                      . hashKey
                      $ rewardAcctVerKey

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (\(VerificationKeyFile fp) -> do
          StakingVerificationKeyShelley svk <-
            firstExceptT (ShelleyPoolReadStakeVerKeyError fp) $ newExceptT $ readStakingVerificationKey fp
          pure svk
        )
        ownerVerFps
    let stakePoolOwners = Set.fromList $ map hashKey sPoolOwnerVkeys

    let registrationCert = shelleyRegisterStakePool
                             (hashKey stakePoolVerKey)
                             (hashVerKeyVRF vrfVerKey)
                             pldg
                             pCost
                             pMrgn
                             rewardAccount
                             stakePoolOwners
                             relays
                             Nothing

    firstExceptT (ShelleyPoolWriteRegistrationCertError outfp) . newExceptT $ writeCertificate outfp registrationCert

runStakePoolRetirementCert
  :: VerificationKeyFile
  -> Shelley.EpochNo
  -> OutputFile
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRetirementCert (VerificationKeyFile sPvkeyFp) retireEpoch (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT (ShelleyPoolReadStakePoolVerKeyError sPvkeyFp) $
      readVerKey (OperatorKey StakePoolOperatorKey) sPvkeyFp

    let retireCert = shelleyRetireStakePool stakePoolVerKey retireEpoch

    firstExceptT (ShelleyPoolWriteRetirementCertError outfp) . newExceptT $ writeCertificate outfp retireCert

runPoolId :: VerificationKeyFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolId (VerificationKeyFile vkeyPath) = do
    stakePoolVerKey <- firstExceptT (ShelleyPoolReadStakePoolVerKeyError vkeyPath) $
                        readVerKey (OperatorKey StakePoolOperatorKey) vkeyPath
    let KeyHash hash = hashKey stakePoolVerKey :: ShelleyVerificationKeyHashStakePool
    liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex hash

runPoolMetaDataHash :: PoolMetaDataFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolMetaDataHash (PoolMetaDataFile poolMDPath) = do
  metaDataBytes <- handleIOExceptT (ShelleyPoolReadMetaDataError poolMDPath) $
    BS.readFile poolMDPath
  let metaDataHash :: Crypto.Hash Crypto.Blake2b_256 ByteString
      metaDataHash = Crypto.hash metaDataBytes
  liftIO $ BS.putStrLn (Crypto.getHashBytesAsHex metaDataHash)
