{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Shelley.Run.DRep
  ( ShelleyDRepCmdError(ShelleyDRepCmdReadFileError)
  , renderShelleyDRepCmdError
  , runDRepCmd
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
import           Cardano.Api.Shelley hiding (DRepMetadataHash)
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyOrFile, readVerificationKeyOrFile)
import           Cardano.CLI.Types (OutputFormat (..))

import qualified Cardano.Ledger.Slot as Shelley

data ShelleyDRepCmdError
  = ShelleyDRepCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyDRepCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyDRepCmdWriteFileError !(FileError ())
  | ShelleyDRepCmdMetadataValidationError !DRepMetadataValidationError
  deriving Show

renderShelleyDRepCmdError :: ShelleyDRepCmdError -> Text
renderShelleyDRepCmdError err =
  case err of
    ShelleyDRepCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyDRepCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyDRepCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyDRepCmdMetadataValidationError validationErr ->
      "Error validating drep metadata: " <> Text.pack (displayError validationErr)



runDRepCmd :: DRepCmd -> ExceptT ShelleyDRepCmdError IO ()
runDRepCmd (DRepRegistrationCert network sPvkey vrfVkey mbMetadata outfp) =
  runDRepRegistrationCert network sPvkey vrfVkey mbMetadata outfp
runDRepCmd (DRepRetirementCert sPvkeyFp retireEpoch outfp) =
  runDRepRetirementCert sPvkeyFp retireEpoch outfp
runDRepCmd (DRepGetId sPvkey outputFormat) = runDRepId sPvkey outputFormat
runDRepCmd (DRepMetadataHash drepMetadataFile mOutFile) = runDRepMetadataHash drepMetadataFile mOutFile

--
-- DRep command implementations
--

-- | Create a drep registration cert.
-- TODO: Metadata and more drep relay support to be
-- added in the future.
runDRepRegistrationCert
  :: NetworkId
  -- ^ Network ID.
  -> VerificationKeyOrFile DRepKey
  -- ^ DRep verification key.
  -> VerificationKeyOrFile VrfKey
  -- ^ VRF Verification key.
  -> Maybe DRepMetadataReference
  -- ^ DRep metadata.
  -> File () Out
  -> ExceptT ShelleyDRepCmdError IO ()
runDRepRegistrationCert
  _drepVerKeyOrFile
  _vrfVerKeyOrFile
  _mbMetadata
  _network
  _outfp = error "Not implemented"

runDRepRetirementCert
  :: VerificationKeyOrFile DRepKey
  -> Shelley.EpochNo
  -> File Certificate Out
  -> ExceptT ShelleyDRepCmdError IO ()
runDRepRetirementCert _drepVerKeyOrFile _retireEpoch _outfp = error "Not implemented"

runDRepId
  :: VerificationKeyOrFile DRepKey
  -> OutputFormat
  -> ExceptT ShelleyDRepCmdError IO ()
runDRepId verKeyOrFile outputFormat = do
    drepVerKey <- firstExceptT ShelleyDRepCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsDRepKey verKeyOrFile
    liftIO $
      case outputFormat of
        OutputFormatHex ->
          BS.putStrLn $ serialiseToRawBytesHex (verificationKeyHash drepVerKey)
        OutputFormatBech32 ->
          Text.putStrLn $ serialiseToBech32 (verificationKeyHash drepVerKey)

runDRepMetadataHash :: File DRepMetadata In -> Maybe (File () Out) -> ExceptT ShelleyDRepCmdError IO ()
runDRepMetadataHash drepMDPath mOutFile = do
  metadataBytes <- lift (readByteStringFile drepMDPath)
    & onLeft (left . ShelleyDRepCmdReadFileError)

  (_metadata, metadataHash) <-
      firstExceptT ShelleyDRepCmdMetadataValidationError
    . hoistEither
    $ validateAndHashDRepMetadata metadataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
    Just (File fpath) ->
      handleIOExceptT (ShelleyDRepCmdWriteFileError . FileIOError fpath)
        $ BS.writeFile fpath (serialiseToRawBytesHex metadataHash)
