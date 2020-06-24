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
                   ShelleyStakePoolMetaData, ShelleyStakePoolRelay,
                   ShelleyVerificationKeyHashStaking, Network, toShelleyNetwork,
                   mkShelleyStakingCredential, renderApiError, shelleyRegisterStakePool,
                   shelleyRetireStakePool, textShow, writeCertificate)
import           Cardano.Api.Typed (AsType (..), Error (..), FileError(..), Hash (..), Key (..),
                   StakeKey, VerificationKey(..), TextEnvelopeError, readFileTextEnvelope,
                   serialiseToRawBytesHex)
import qualified Cardano.Api.Typed as Typed

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Slot as Shelley

import           Cardano.Config.Types (PoolMetaDataFile (..))

import           Cardano.CLI.Shelley.Commands

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

data ShelleyPoolCmdError
  = ShelleyPoolWriteRegistrationCertError !FilePath !ApiError
  | ShelleyPoolWriteRetirementCertError !FilePath !ApiError
  | ShelleyPoolReadFileError !(FileError TextEnvelopeError)
  | ShelleyPoolMetaDataValidationError !Typed.StakePoolMetadataValidationError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ShelleyPoolWriteRegistrationCertError fp apiErr ->
      "Error writing stake pool registration certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyPoolWriteRetirementCertError fp apiErr ->
      "Error writing stake pool retirement certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyPoolReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolMetaDataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Typed.renderStakePoolMetadataValidationError validationErr



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
    stakePoolVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) sPvkeyFp
    let StakePoolKeyHash shelleyStakePoolKeyHash = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsVrfKey) vrfVkeyFp
    let VrfKeyHash shelleyVrfKeyHash = verificationKeyHash vrfVerKey

    -- Pool reward account
    stakeVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) rwdVerFp
    let StakeKeyHash shelleyStakeKeyHash = verificationKeyHash stakeVerKey
        rewardAccount = Shelley.mkRwdAcnt (toShelleyNetwork network)
                      . mkShelleyStakingCredential
                      $ shelleyStakeKeyHash

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (\(VerificationKeyFile fp) ->
          firstExceptT ShelleyPoolReadFileError
            . newExceptT
            $ readFileTextEnvelope (AsVerificationKey AsStakeKey) fp
        )
        ownerVerFps
    let stakePoolOwners = Set.fromList $
                            map (toShelleyStakeKeyHash . verificationKeyHash) sPoolOwnerVkeys

    let registrationCert = shelleyRegisterStakePool
                             shelleyStakePoolKeyHash
                             shelleyVrfKeyHash
                             pldg
                             pCost
                             pMrgn
                             rewardAccount
                             stakePoolOwners
                             relays
                             mbMetadata

    firstExceptT (ShelleyPoolWriteRegistrationCertError outfp) . newExceptT $ writeCertificate outfp registrationCert
    where
      toShelleyStakeKeyHash :: Hash StakeKey -> ShelleyVerificationKeyHashStaking
      toShelleyStakeKeyHash (StakeKeyHash skh) = skh

runStakePoolRetirementCert
  :: VerificationKeyFile
  -> Shelley.EpochNo
  -> OutputFile
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRetirementCert (VerificationKeyFile sPvkeyFp) retireEpoch (OutputFile outfp) = do
    -- Pool verification key
    StakePoolVerificationKey stakePoolVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) sPvkeyFp

    let retireCert = shelleyRetireStakePool stakePoolVerKey retireEpoch

    firstExceptT (ShelleyPoolWriteRetirementCertError outfp) . newExceptT $ writeCertificate outfp retireCert

runPoolId :: VerificationKeyFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolId (VerificationKeyFile vkeyPath) = do
    stakePoolVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) vkeyPath
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)

runPoolMetaDataHash :: PoolMetaDataFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolMetaDataHash (PoolMetaDataFile poolMDPath) = do
  metaDataBytes <- handleIOExceptT (ShelleyPoolReadFileError . FileIOError poolMDPath) $
    BS.readFile poolMDPath
  _ <- firstExceptT ShelleyPoolMetaDataValidationError
    . hoistEither
    $ Typed.decodeAndValidateStakePoolMetadata metaDataBytes
  let metaDataHash :: Crypto.Hash Crypto.Blake2b_256 ByteString
      metaDataHash = Crypto.hashRaw (\x -> x) metaDataBytes
  liftIO $ BS.putStrLn (Crypto.getHashBytesAsHex metaDataHash)
