{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Shelley.Run.Pool
  ( ShelleyPoolCmdError(ShelleyPoolCmdReadFileError)
  , renderShelleyPoolCmdError
  , runPoolCmd
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                   newExceptT, onLeft)
import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Cardano.Api
import           Cardano.Api.Shelley
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyOrFile, readVerificationKeyOrFile)
import           Cardano.CLI.Types (OutputFormat (..))

import qualified Cardano.Ledger.Slot as Shelley

data ShelleyPoolCmdError
  = ShelleyPoolCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyPoolCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyPoolCmdWriteFileError !(FileError ())
  | ShelleyPoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ShelleyPoolCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdMetadataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)



runPoolCmd :: PoolCmd -> ExceptT ShelleyPoolCmdError IO ()
runPoolCmd (PoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp) =
  runStakePoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
runPoolCmd (PoolRetirementCert sPvkeyFp retireEpoch outfp) =
  runStakePoolRetirementCert sPvkeyFp retireEpoch outfp
runPoolCmd (PoolGetId sPvkey outputFormat) = runPoolId sPvkey outputFormat
runPoolCmd (PoolMetadataHash poolMdFile mOutFile) = runPoolMetadataHash poolMdFile mOutFile


--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCert
  :: VerificationKeyOrFile StakePoolKey
  -- ^ Stake pool verification key.
  -> VerificationKeyOrFile VrfKey
  -- ^ VRF Verification key.
  -> Lovelace
  -- ^ Pool pledge.
  -> Lovelace
  -- ^ Pool cost.
  -> Rational
  -- ^ Pool margin.
  -> VerificationKeyOrFile StakeKey
  -- ^ Stake verification key for reward account.
  -> [VerificationKeyOrFile StakeKey]
  -- ^ Pool owner stake verification key(s).
  -> [StakePoolRelay]
  -- ^ Stake pool relays.
  -> Maybe StakePoolMetadataReference
  -- ^ Stake pool metadata.
  -> NetworkId
  -> File () Out
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRegistrationCert
  stakePoolVerKeyOrFile
  vrfVerKeyOrFile
  pldg
  pCost
  pMrgn
  rwdStakeVerKeyOrFile
  ownerStakeVerKeyOrFiles
  relays
  mbMetadata
  network
  outfp = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile
    let stakePoolId' = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsVrfKey vrfVerKeyOrFile
    let vrfKeyHash' = verificationKeyHash vrfVerKey

    -- Pool reward account
    rwdStakeVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey rwdStakeVerKeyOrFile
    let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
        rewardAccountAddr = makeStakeAddress network stakeCred

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (firstExceptT ShelleyPoolCmdReadKeyFileError
          . newExceptT
          . readVerificationKeyOrFile AsStakeKey
        )
        ownerStakeVerKeyOrFiles
    let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

    let stakePoolParams =
          StakePoolParameters
            { stakePoolId = stakePoolId'
            , stakePoolVRF = vrfKeyHash'
            , stakePoolCost = pCost
            , stakePoolMargin = pMrgn
            , stakePoolRewardAccount = rewardAccountAddr
            , stakePoolPledge = pldg
            , stakePoolOwners = stakePoolOwners'
            , stakePoolRelays = relays
            , stakePoolMetadata = mbMetadata
            }

    let registrationCert = makeStakePoolRegistrationCertificate stakePoolParams

    firstExceptT ShelleyPoolCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outfp
      $ textEnvelopeToJSON (Just registrationCertDesc) registrationCert
  where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

runStakePoolRetirementCert
  :: VerificationKeyOrFile StakePoolKey
  -> Shelley.EpochNo
  -> File Certificate Out
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRetirementCert stakePoolVerKeyOrFile retireEpoch outfp = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile

    let stakePoolId' = verificationKeyHash stakePoolVerKey
        retireCert = makeStakePoolRetirementCertificate stakePoolId' retireEpoch

    firstExceptT ShelleyPoolCmdWriteFileError
      . newExceptT
      $ intoFile outfp retireCert writeLazyByteStringFile
      $ textEnvelopeToJSON (Just retireCertDesc)
  where
    retireCertDesc :: TextEnvelopeDescr
    retireCertDesc = "Stake Pool Retirement Certificate"

runPoolId
  :: VerificationKeyOrFile StakePoolKey
  -> OutputFormat
  -> ExceptT ShelleyPoolCmdError IO ()
runPoolId verKeyOrFile outputFormat = do
    stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey verKeyOrFile
    liftIO $
      case outputFormat of
        OutputFormatHex ->
          BS.putStrLn $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)
        OutputFormatBech32 ->
          Text.putStrLn $ serialiseToBech32 (verificationKeyHash stakePoolVerKey)

runPoolMetadataHash :: File StakePoolMetadata In -> Maybe (File () Out) -> ExceptT ShelleyPoolCmdError IO ()
runPoolMetadataHash poolMDPath mOutFile = do
  metadataBytes <- lift (readByteStringFile poolMDPath)
    & onLeft (left . ShelleyPoolCmdReadFileError)

  (_metadata, metadataHash) <-
      firstExceptT ShelleyPoolCmdMetadataValidationError
    . hoistEither
    $ validateAndHashStakePoolMetadata metadataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
    Just (File fpath) ->
      handleIOExceptT (ShelleyPoolCmdWriteFileError . FileIOError fpath)
        $ BS.writeFile fpath (serialiseToRawBytesHex metadataHash)
