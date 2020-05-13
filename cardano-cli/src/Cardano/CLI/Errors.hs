module Cardano.CLI.Errors
  ( CliError(..)
  , RealPBFTError(..)
  ) where

import           Prelude (show, unlines)
import           Cardano.Prelude hiding (atomically, catch, option, show, unlines)

import qualified Data.Text as T
import           Codec.CBOR.Read (DeserialiseFailure)

import           Cardano.Api
                   (ApiError, LocalStateQueryError, renderLocalStateQueryError)
import           Cardano.Binary (DecoderError)
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Config.Shelley.Genesis (ShelleyGenesisError, renderShelleyGenesisError)
import           Cardano.Config.TextView (TextViewFileError, renderTextViewFileError)

import           Cardano.Config.Protocol
                   (Protocol(..), ProtocolInstantiationError,
                    renderProtocolInstantiationError)
import           Cardano.Config.Shelley.Address (AddressError, renderAddressError)
import           Cardano.Config.Shelley.ColdKeys (KeyError, renderKeyError)
import           Cardano.Config.Shelley.KES (KESError, renderKESError)
import           Cardano.Config.Shelley.VRF (VRFError, renderVRFError)
import           Cardano.Config.Shelley.OCert (OperationalCertError)
import           Cardano.CLI.Era


-- | Exception type for all errors thrown by the CLI.
--   Well, almost all, since we don't rethrow the errors from readFile & such.
data CliError
  = AddressCliError AddressError
  | ByronVoteDecodingError !DecoderError
  | ByronVoteSubmissionError !RealPBFTError
  | ByronReadUpdateProposalFileFailure !FilePath !Text
  | ByronReadVoteFileFailure !FilePath !Text
  | CardanoEraNotSupported !CardanoEra
  | CBORDecodingError !DeserialiseFailure
  | CBORPrettyPrintError !DeserialiseFailure
  | CertificateValidationErrors !FilePath ![Text]
  | DelegationError !Genesis.GenesisDelegationError
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | GenerateTxsError !RealPBFTError
  | GenesisGenerationError !Genesis.GenesisDataGenerationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenesisSpecError !Text
  | IssueUtxoError !RealPBFTError
  | KESCliError KESError
  | KeyCliError KeyError
  | NoBlocksFound !FilePath
  | NodeSubmitTxError !RealPBFTError
  | NotEnoughTxInputs
  | NotEnoughTxOutputs
  | NoGenesisDelegationForKey !Text
  | OutputMustNotAlreadyExist !FilePath
  | OperationalCertError OperationalCertError
  | ProtocolError !ProtocolInstantiationError
  | ProtocolParametersParseFailed !FilePath !Text
  | ReadCBORFileFailure !FilePath !Text
  | ReadSigningKeyFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  | ShelleyCertReadError !ApiError
  | TxDeserialisationFailed !FilePath !DecoderError
  -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  | SigningKeyDeserialisationFailed !FilePath !DeserialiseFailure
  | SpendGenesisUTxOError !RealPBFTError
  | UpdateProposalDecodingError !DecoderError
  | UpdateProposalSubmissionError !RealPBFTError
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | VRFCliError VRFError
  | FileNotFoundError !FilePath
  | CardanoApiError !ApiError
  | IOError !FilePath !IOException
  | AesonDecode !FilePath !Text
  | ShelleyGenesisError !ShelleyGenesisError
  | IncorrectProtocolSpecifiedError !Protocol
  | NodeLocalStateQueryError !LocalStateQueryError
  | AddressDescribeError !Text
  | CliTextViewFileError !TextViewFileError
  | CliEnvVarLookup !Text

instance Show CliError where
  show (AddressCliError e)
    = T.unpack $ renderAddressError e
  show (ByronVoteDecodingError err)
    =  "Error decoding Byron vote: " <> show err
  show (ByronVoteSubmissionError pbftErr)
    = "Error submitting Byron vote: " <> (show $ renderRealPBFTError pbftErr)
  show (ByronReadUpdateProposalFileFailure fp err)
    =  "Error reading Byron update proposal at: " <> fp <> "Error: " <> show err
  show (ByronReadVoteFileFailure fp err)
    =  "Error reading Byron vote at: " <> fp <> "Error: " <> show err
  show (CBORDecodingError e)
    = "Error with CBOR decoding: " <> show e
  show (CBORPrettyPrintError e)
    = "Error with CBOR decoding: " <> show e
  show (CertificateValidationErrors fp errs)
    = unlines $
      "Errors while validating certificate '" <> fp <> "':":
      (("  " <>) . T.unpack <$> errs)
  show (GenesisReadError fp err)
    = "Genesis file '" <> fp <> "' read failure: "<> show err
  show (DelegationError err)
    = "Error while issuing delegation: " <> show err
  show (DlgCertificateDeserialisationFailed fp err)
    = "Delegation certificate '" <> fp <> "' read failure: "<> T.unpack err
  show (FileNotFoundError fp)
    = "File '" <> fp <> "' not found!"
  show (GenerateTxsError err)
    = "Error in GenerateTxs command: " <> (T.unpack $ renderRealPBFTError err)
  show (GenesisGenerationError err)
    = "Genesis generation failed in mkGenesis: " <> show err
  show (GenesisSpecError err)
    = "Error in genesis specification: " <> T.unpack err
  show (IssueUtxoError err)
    = "Error SpendUTxO command: " <> (T.unpack $ renderRealPBFTError err)
  show (KESCliError err)
    = show $ renderKESError err
  show (KeyCliError err)
    = T.unpack $ renderKeyError err
  show (NoBlocksFound fp)
    = "Error while creating update proposal, no blocks found in: " <> fp
  show (NodeSubmitTxError err)
    = "Error in SubmitTx command: " <> (T.unpack $ renderRealPBFTError err)
  show (NoGenesisDelegationForKey key)
    = "Newly-generated genesis doesn't delegate to operational key: " <> T.unpack key
  show NotEnoughTxInputs
    = "Transactions must have at least one input."
  show NotEnoughTxOutputs
    = "Transactions must have at least one output."
  show (OutputMustNotAlreadyExist fp)
    = "Output file/directory must not already exist: " <> fp
  show (OperationalCertError err)
    = show err --TODO: renderOperationalCertError
  show (ProtocolError err)
    = "Protocol Instantiation Error " <> (T.unpack $ renderProtocolInstantiationError err)
  show (CardanoApiError apiError)
    = show apiError
  show (CardanoEraNotSupported era)
    = "Unsupported Cardano era " <> show era
  show (ProtocolParametersParseFailed fp err)
    = "Protocol parameters file '" <> fp <> "' read failure: "<> T.unpack err
  show (ReadSigningKeyFailure fp expt)
    = "Exception encountered while trying to read the signing key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadCBORFileFailure fp expt)
    = "Exception encountered while trying to read the CBOR file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadVerificationKeyFailure fp expt)
    = "Exception encountered while trying to read the verification key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ShelleyCertReadError err)
    = "Shelley certificate read error: " <> show err
  show (SigningKeyDeserialisationFailed fp err)
    = "Signing key '" <> fp <> "' read failure: "<> show err
  show (SpendGenesisUTxOError err)
    = "Error in SpendGenesisUTxO command: " <> (T.unpack $ renderRealPBFTError err)
  show (TxDeserialisationFailed fp err)
    = "Transaction file '" <> fp <> "' read failure: "<> show err
  show (UpdateProposalDecodingError err)
    = "Error decoding update proposal: " <> show err
  show (UpdateProposalSubmissionError err)
    = "Error submitting update proposal: " <> (T.unpack $ renderRealPBFTError err)
  show (VerificationKeyDeserialisationFailed fp err)
    = "Verification key '" <> fp <> "' read failure: "<> T.unpack err
  show (VRFCliError err) = T.unpack $ renderVRFError err
  show (IOError fp ioe)
    = "File '" <> fp <> "': " ++ show ioe
  show (AesonDecode fp txt)
    = "File '" <> fp <> "': " ++ show txt
  show (ShelleyGenesisError sge)
    = T.unpack $ renderShelleyGenesisError sge
  show (IncorrectProtocolSpecifiedError ptcl)
    = "Incorrect protocol specified: " <> (toS $ show ptcl)
  show (NodeLocalStateQueryError err)
    = T.unpack $ renderLocalStateQueryError err
  show (AddressDescribeError txt)
    = T.unpack txt
  show (CliTextViewFileError err)
    = T.unpack $ renderTextViewFileError err
  show (CliEnvVarLookup name)
    = "Lookup of environment variable " <> show name <> " failed."


data RealPBFTError
  = IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ProtocolInstantiationError
  | InvariantViolation !Text
  | TransactionTypeNotHandledYet !Text

renderRealPBFTError :: RealPBFTError -> Text
renderRealPBFTError err =
  case err of
    IncorrectProtocolSpecified ptcl -> "Incorrect protocol specified: " <> (toS $ show ptcl)
    FromProtocolError ptclInstErr -> renderProtocolInstantiationError ptclInstErr
    InvariantViolation invErr -> "Invariant violation: " <> invErr
    TransactionTypeNotHandledYet err' -> "Transaction type not handled yet: " <> err'

