{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Shelley.Run.Pool
  ( ShelleyPoolCmdError
  , renderShelleyPoolCmdError
  , runPoolCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   newExceptT)

import qualified Data.ByteString.Char8 as BS

import           Cardano.Api (StakePoolMetadataValidationError, Network(..),
                   decodeAndValidateStakePoolMetadata, renderStakePoolMetadataValidationError)
import           Cardano.Api.TextView (TextViewTitle (..), textShow)
import           Cardano.Api.Typed (AsType (..), Error (..), FileError (..), Key (..),
                   Lovelace, StakeCredential (..), StakePoolMetadataReference (..),
                   StakePoolParameters (..), StakePoolRelay (..), TextEnvelopeError,
                   makeStakeAddress, makeStakePoolRegistrationCertificate,
                   makeStakePoolRetirementCertificate, readFileTextEnvelope, serialiseToRawBytesHex,
                   writeFileTextEnvelope)
import qualified Cardano.Api.Typed as Api (NetworkId (..))

import qualified Shelley.Spec.Ledger.Slot as Shelley

import           Cardano.Config.Types (PoolMetaDataFile (..))

import           Cardano.CLI.Shelley.Commands

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

data ShelleyPoolCmdError
  = ShelleyPoolReadFileError !(FileError TextEnvelopeError)
  | ShelleyPoolWriteFileError !(FileError ())
  | ShelleyPoolWriteMetaDataHashError !FilePath !IOException
  | ShelleyPoolMetaDataValidationError !StakePoolMetadataValidationError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ShelleyPoolReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolWriteMetaDataHashError fp ioException ->
      "Error writing stake pool metadata hash at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyPoolMetaDataValidationError validationErr ->
      "Error validating stake pool metadata: " <> renderStakePoolMetadataValidationError validationErr



runPoolCmd :: PoolCmd -> ExceptT ShelleyPoolCmdError IO ()
runPoolCmd (PoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp) =
  runStakePoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
runPoolCmd (PoolRetirementCert sPvkeyFp retireEpoch outfp) =
  runStakePoolRetirementCert sPvkeyFp retireEpoch outfp
runPoolCmd (PoolGetId sPvkey) = runPoolId sPvkey
runPoolCmd (PoolMetaDataHash poolMdFile mOutFile) = runPoolMetaDataHash poolMdFile mOutFile
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
  -> Lovelace
  -- ^ Pool pledge.
  -> Lovelace
  -- ^ Pool cost.
  -> Rational
  -- ^ Pool margin.
  -> VerificationKeyFile
  -- ^ Stake verification key for reward account.
  -> [VerificationKeyFile]
  -- ^ Pool owner stake verification key(s).
  -> [StakePoolRelay]
  -- ^ Stake pool relays.
  -> (Maybe StakePoolMetadataReference)
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
    let stakePoolId = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsVrfKey) vrfVkeyFp
    let vrfKeyHash = verificationKeyHash vrfVerKey

    -- Pool reward account
    stakeVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) rwdVerFp
    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        rewardAccountAddr = makeStakeAddress nwId stakeCred

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (\(VerificationKeyFile fp) ->
          firstExceptT ShelleyPoolReadFileError
            . newExceptT
            $ readFileTextEnvelope (AsVerificationKey AsStakeKey) fp
        )
        ownerVerFps
    let stakePoolOwners = map verificationKeyHash sPoolOwnerVkeys

    let stakePoolParams =
          StakePoolParameters
            { stakePoolId = stakePoolId
            , stakePoolVRF = vrfKeyHash
            , stakePoolCost = pCost
            , stakePoolMargin = pMrgn
            , stakePoolRewardAccount = rewardAccountAddr
            , stakePoolPledge = pldg
            , stakePoolOwners = stakePoolOwners
            , stakePoolRelays = relays
            , stakePoolMetadata = mbMetadata
            }

    let registrationCert = makeStakePoolRegistrationCertificate stakePoolParams

    firstExceptT ShelleyPoolWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outfp (Just registrationCertDesc) registrationCert
  where
    registrationCertDesc :: TextViewTitle
    registrationCertDesc = TextViewTitle "Stake Pool Registration Certificate"

    -- TODO: Remove this once we remove usage of 'Cardano.Api.Types.Network'
    --       from this module.
    nwId :: Api.NetworkId
    nwId =
      case network of
        Mainnet -> Api.Mainnet
        Testnet nm -> Api.Testnet nm

runStakePoolRetirementCert
  :: VerificationKeyFile
  -> Shelley.EpochNo
  -> OutputFile
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRetirementCert (VerificationKeyFile sPvkeyFp) retireEpoch (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) sPvkeyFp

    let stakePoolId = verificationKeyHash stakePoolVerKey
        retireCert = makeStakePoolRetirementCertificate stakePoolId retireEpoch

    firstExceptT ShelleyPoolWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outfp (Just retireCertDesc) retireCert
  where
    retireCertDesc :: TextViewTitle
    retireCertDesc = TextViewTitle "Stake Pool Retirement Certificate"

runPoolId :: VerificationKeyFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolId (VerificationKeyFile vkeyPath) = do
    stakePoolVerKey <- firstExceptT ShelleyPoolReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) vkeyPath
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)

runPoolMetaDataHash :: PoolMetaDataFile -> Maybe OutputFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolMetaDataHash (PoolMetaDataFile poolMDPath) mOutFile = do
  metaDataBytes <- handleIOExceptT (ShelleyPoolReadFileError . FileIOError poolMDPath) $
    BS.readFile poolMDPath
  _ <- firstExceptT ShelleyPoolMetaDataValidationError
    . hoistEither
    $ decodeAndValidateStakePoolMetadata metaDataBytes
  let metaDataHash :: Crypto.Hash Crypto.Blake2b_256 ByteString
      metaDataHash = Crypto.hashRaw (\x -> x) metaDataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (Crypto.getHashBytesAsHex metaDataHash)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyPoolWriteMetaDataHashError fpath)
        $ BS.writeFile fpath (Crypto.getHashBytesAsHex metaDataHash)
