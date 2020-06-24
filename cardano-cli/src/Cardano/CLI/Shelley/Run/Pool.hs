module Cardano.CLI.Shelley.Run.Pool
  ( ShelleyPoolCmdError
  , renderShelleyPoolCmdError
  , runPoolCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text
import qualified Data.Set as Set

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   newExceptT)

import qualified Data.ByteString.Char8 as BS

import           Cardano.Api (ApiError(..), ShelleyCoin, ShelleyStakePoolMargin,
                   ShelleyStakePoolMetaData, ShelleyStakePoolRelay, StakingVerificationKey (..),
                   ShelleyVerificationKeyHashStakePool, StakePoolMetadataValidationError,
                   Network, decodeAndValidateStakePoolMetadata, toShelleyNetwork,
                   mkShelleyStakingCredential, readStakingVerificationKey, renderApiError,
                   renderStakePoolMetadataValidationError, shelleyRegisterStakePool,
                   shelleyRetireStakePool, textShow, writeCertificate)
import           Cardano.Api.Typed (AsType (..), Error (..), FileError, Hash (..), Key (..),
                   TextEnvelopeError, readFileTextEnvelope)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.Keys (KeyHash (..), hashKey)
import qualified Shelley.Spec.Ledger.Slot as Shelley

import           Cardano.Api.Shelley.ColdKeys
import           Cardano.Config.Types (PoolMetaDataFile (..))

import           Cardano.CLI.Shelley.Commands

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

data ShelleyPoolCmdError
  = ShelleyPoolReadStakeVerKeyError !FilePath !ApiError
  | ShelleyPoolReadStakePoolVerKeyError !FilePath !KeyError
  | ShelleyPoolReadVRFVerKeyError !(FileError TextEnvelopeError)
  | ShelleyPoolReadMetaDataError !FilePath !IOException
  | ShelleyPoolWriteRegistrationCertError !FilePath !ApiError
  | ShelleyPoolWriteRetirementCertError !FilePath !ApiError
  | ShelleyPoolMetaDataValidationError !StakePoolMetadataValidationError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ShelleyPoolReadVRFVerKeyError fileErr ->
      "Error reading VRF verification key: " <> Text.pack (displayError fileErr)
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
    ShelleyPoolMetaDataValidationError validationErr ->
      "Error validating stake pool metadata: " <> renderStakePoolMetadataValidationError validationErr



runPoolCmd :: PoolCmd -> ExceptT ShelleyPoolCmdError IO ()
runPoolCmd (PoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp) =
  runStakePoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
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
  -> (Maybe ShelleyStakePoolMetaData)
  -- ^ Stake pool metadata.
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
  mbMetadata
  network
  (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT (ShelleyPoolReadStakePoolVerKeyError sPvkeyFp) $
      readVerKey (OperatorKey StakePoolOperatorKey) sPvkeyFp

    -- VRF verification key
    vrfVerKey <- firstExceptT ShelleyPoolReadVRFVerKeyError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsVrfKey) vrfVkeyFp
    let VrfKeyHash shelleyVrfKeyHash = verificationKeyHash vrfVerKey

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
                             shelleyVrfKeyHash
                             pldg
                             pCost
                             pMrgn
                             rewardAccount
                             stakePoolOwners
                             relays
                             mbMetadata

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
  _ <- firstExceptT ShelleyPoolMetaDataValidationError
    . hoistEither
    $ decodeAndValidateStakePoolMetadata metaDataBytes
  let metaDataHash :: Crypto.Hash Crypto.Blake2b_256 ByteString
      metaDataHash = Crypto.hash metaDataBytes
  liftIO $ BS.putStrLn (Crypto.getHashBytesAsHex metaDataHash)
