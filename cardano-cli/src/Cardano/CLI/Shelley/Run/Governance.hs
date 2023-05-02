{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Governance
  ( ShelleyGovernanceCmdError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Control.Monad (unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, left, newExceptT,
                   onLeft)
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LB
import           Data.Function ((&))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Formatting (build, sformat)
import qualified Prettyprinter as PP
import qualified System.IO as IO
import           System.IO (stderr, stdin, stdout)

import           Cardano.Api
import           Cardano.Api.Pretty
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (VerificationKeyOrHashOrFile,
                   readVerificationKeyOrHashOrFile, readVerificationKeyOrHashOrTextEnvFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Read (CddlError, fileOrPipe, readFileTx)
import           Cardano.CLI.Types

import           Cardano.Binary (DecoderError)
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

data ShelleyGovernanceCmdError
  = ShelleyGovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | ShelleyGovernanceCmdCddlError !CddlError
  | ShelleyGovernanceCmdKeyReadError !(FileError InputDecodeError)
  | ShelleyGovernanceCmdCostModelReadError !(FileError ())
  | ShelleyGovernanceCmdTextEnvWriteError !(FileError ())
  | ShelleyGovernanceCmdEmptyUpdateProposalError
  | ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  | ShelleyGovernanceCmdCostModelsJsonDecodeErr !FilePath !Text
  | ShelleyGovernanceCmdEmptyCostModel !FilePath
  | ShelleyGovernanceCmdUnexpectedKeyType
      ![TextEnvelopeType]
      -- ^ Expected key types
  | ShelleyGovernanceCmdPollOutOfBoundAnswer
      !Int
      -- ^ Maximum answer index
  | ShelleyGovernanceCmdPollInvalidChoice
  | ShelleyGovernanceCmdDecoderError !DecoderError
  | ShelleyGovernanceCmdVerifyPollError !GovernancePollError
  | ShelleyGovernanceCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceCmdError -> Doc Ann
renderShelleyGovernanceError err =
  case err of
    ShelleyGovernanceCmdTextEnvReadError fileErr -> displayError fileErr
    ShelleyGovernanceCmdCddlError cddlErr -> displayError cddlErr
    ShelleyGovernanceCmdKeyReadError fileErr -> displayError fileErr
    ShelleyGovernanceCmdTextEnvWriteError fileErr -> displayError fileErr
    -- TODO: The equality check is still not working for empty update proposals.
    ShelleyGovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"
    ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
       "Error creating the MIR certificate at: " <> pretty fp
       <> " The number of staking keys: " <> pretty numVKeys
       <> " and the number of reward amounts: " <> pretty numRwdAmts
       <> " are not equivalent."
    ShelleyGovernanceCmdCostModelsJsonDecodeErr fp err' ->
      "Error decoding cost model: " <> pretty err' <> " at: " <> pretty fp
    ShelleyGovernanceCmdEmptyCostModel fp ->
      "The decoded cost model was empty at: " <> pretty fp
    ShelleyGovernanceCmdCostModelReadError err' ->
      "Error reading the cost model: " <> displayError err'
    ShelleyGovernanceCmdUnexpectedKeyType expected ->
      "Unexpected poll key type; expected one of: " <> PP.prettyList (show <$> expected)
    ShelleyGovernanceCmdPollOutOfBoundAnswer nMax ->
      "Poll answer out of bounds. Choices are between 0 and " <> pretty nMax
    ShelleyGovernanceCmdPollInvalidChoice ->
      "Invalid choice. Please choose from the available answers."
    ShelleyGovernanceCmdDecoderError decoderError ->
      "Unable to decode metadata: " <> pretty (sformat build decoderError)
    ShelleyGovernanceCmdVerifyPollError pollError ->
      renderGovernancePollError pollError
    ShelleyGovernanceCmdWriteFileError fileErr -> displayError fileErr

runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceCmd (GovernanceMIRPayStakeAddressesCertificate mirpot vKeys rewards out) =
  runGovernanceMIRCertificatePayStakeAddrs mirpot vKeys rewards out
runGovernanceCmd (GovernanceMIRTransfer amt out direction) =
  runGovernanceMIRCertificateTransfer amt out direction
runGovernanceCmd (GovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out) =
  runGovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp) =
  runGovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp
runGovernanceCmd (GovernanceCreatePoll prompt choices nonce out) =
  runGovernanceCreatePoll prompt choices nonce out
runGovernanceCmd (GovernanceAnswerPoll poll ix mOutFile) =
  runGovernanceAnswerPoll poll ix mOutFile
runGovernanceCmd (GovernanceVerifyPoll poll metadata mOutFile) =
  runGovernanceVerifyPoll poll metadata mOutFile

runGovernanceMIRCertificatePayStakeAddrs
  :: Shelley.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [Lovelace]     -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificatePayStakeAddrs mirPot sAddrs rwdAmts oFp = do

    unless (length sAddrs == length rwdAmts) $
      left $ ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
               (unFile oFp) (length sAddrs) (length rwdAmts)

    let sCreds  = map stakeAddressCredential sAddrs
        mirCert = makeMIRCertificate mirPot (StakeAddressesMIR $ zip sCreds rwdAmts)

    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeLazyByteStringFile oFp $ textEnvelopeToJSON (Just mirCertDesc) mirCert
  where
    mirCertDesc :: TextEnvelopeDescr
    mirCertDesc = "Move Instantaneous Rewards Certificate"

runGovernanceMIRCertificateTransfer
  :: Lovelace
  -> File () Out
  -> TransferDirection
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificateTransfer ll oFp direction = do
  mirCert <- case direction of
                 TransferToReserves ->
                   return . makeMIRCertificate Shelley.TreasuryMIR $ SendToReservesMIR ll
                 TransferToTreasury ->
                   return . makeMIRCertificate Shelley.ReservesMIR $ SendToTreasuryMIR ll

  firstExceptT ShelleyGovernanceCmdTextEnvWriteError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just $ mirCertDesc direction) mirCert
 where
  mirCertDesc :: TransferDirection -> TextEnvelopeDescr
  mirCertDesc TransferToTreasury = "MIR Certificate Send To Treasury"
  mirCertDesc TransferToReserves = "MIR Certificate Send To Reserves"


runGovernanceGenesisKeyDelegationCertificate
  :: VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             oFp = do
    genesisVkHash <- firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
    genesisDelVkHash <-firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
    vrfVkHash <- firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp
    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ textEnvelopeToJSON (Just genKeyDelegCertDesc)
      $ makeGenesisKeyDelegationCertificate genesisVkHash genesisDelVkHash vrfVkHash
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"

runGovernanceUpdateProposal
  :: File () Out
  -> EpochNo
  -> [VerificationKeyFile In]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> Maybe FilePath -- ^ Cost models file path
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceUpdateProposal upFile eNo genVerKeyFiles upPprams mCostModelFp = do
  finalUpPprams <- case mCostModelFp of
    Nothing -> return upPprams
    Just fp -> do
      costModelsBs <- handleIOExceptT (ShelleyGovernanceCmdCostModelReadError . FileIOError fp) $ LB.readFile fp

      cModels <- pure (eitherDecode costModelsBs)
        & onLeft (left . ShelleyGovernanceCmdCostModelsJsonDecodeErr fp . Text.pack)

      let costModels = fromAlonzoCostModels cModels

      when (null costModels) $ left (ShelleyGovernanceCmdEmptyCostModel fp)

      return $ upPprams {protocolUpdateCostModels = costModels}

  when (finalUpPprams == mempty) $ left ShelleyGovernanceCmdEmptyUpdateProposalError

  genVKeys <- sequence
    [ firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $ readFileTextEnvelope (AsVerificationKey AsGenesisKey) vkeyFile
    | vkeyFile <- genVerKeyFiles
    ]
  let genKeyHashes = fmap verificationKeyHash genVKeys
      upProp = makeShelleyUpdateProposal finalUpPprams genKeyHashes eNo

  firstExceptT ShelleyGovernanceCmdTextEnvWriteError . newExceptT
    $ writeLazyByteStringFile upFile $ textEnvelopeToJSON Nothing upProp

runGovernanceCreatePoll
  :: Text
  -> [Text]
  -> Maybe Word
  -> File GovernancePoll Out
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceCreatePoll govPollQuestion govPollAnswers govPollNonce out = do
  let poll = GovernancePoll{ govPollQuestion, govPollAnswers, govPollNonce }

  let description = fromString $ "An on-chain poll for SPOs: " <> Text.unpack govPollQuestion
  firstExceptT ShelleyGovernanceCmdTextEnvWriteError . newExceptT $
    writeFileTextEnvelope out (Just description) poll

  let metadata = asTxMetadata poll
        & metadataToJson TxMetadataJsonDetailedSchema

  let outPath = unFile out & Text.encodeUtf8 . Text.pack

  liftIO $ do
    BSC.hPutStrLn stderr $ mconcat
      [ "Poll created successfully.\n"
      , "Please submit a transaction using the resulting metadata.\n"
      ]
    BSC.hPutStrLn stdout (prettyPrintJSON metadata)
    BSC.hPutStrLn stderr $ mconcat
      [ "\n"
      , "Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' "
      , "from the build or build-raw commands.\n"
      , "Hint (2): You can redirect the standard output of this command to a JSON "
      , "file to capture metadata.\n\n"
      , "Note: A serialized version of the poll suitable for sharing with "
      , "participants has been generated at '" <> outPath <> "'."
      ]

runGovernanceAnswerPoll
  :: File GovernancePoll In
  -> Maybe Word -- ^ Answer index
  -> Maybe (File () Out) -- ^ Output file
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceAnswerPoll pollFile maybeChoice mOutFile = do
  poll <- firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $
    readFileTextEnvelope AsGovernancePoll pollFile

  choice <- case maybeChoice of
    Nothing -> do
      askInteractively poll
    Just ix -> do
      validateChoice poll ix
      liftIO $ BSC.hPutStrLn stderr $ Text.encodeUtf8 $ Text.intercalate "\n"
        [ govPollQuestion poll
        , "→ " <> (govPollAnswers poll !! fromIntegral ix)
        , ""
        ]
      pure ix

  let pollAnswer = GovernancePollAnswer
        { govAnsPoll = hashGovernancePoll poll
        , govAnsChoice = choice
        }
  let metadata =
        metadataToJson TxMetadataJsonDetailedSchema (asTxMetadata pollAnswer)

  liftIO $ BSC.hPutStrLn stderr $ mconcat
      [ "Poll answer created successfully.\n"
      , "Please submit a transaction using the resulting metadata.\n"
      , "To be valid, the transaction must also be signed using a valid key\n"
      , "identifying your stake pool (e.g. your cold key).\n"
      ]

  lift (writeByteStringOutput mOutFile (prettyPrintJSON metadata))
    & onLeft (left . ShelleyGovernanceCmdWriteFileError)

  liftIO $ BSC.hPutStrLn stderr $ mconcat
      [ "\n"
      , "Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' "
      , "from the build or build-raw commands.\n"
      , "Hint (2): You can redirect the standard output of this command to a JSON "
      , "file to capture metadata."
      ]
 where
  validateChoice :: GovernancePoll -> Word -> ExceptT ShelleyGovernanceCmdError IO ()
  validateChoice GovernancePoll{govPollAnswers} ix = do
    let maxAnswerIndex = length govPollAnswers - 1
    when (fromIntegral ix > maxAnswerIndex) $ left $
      ShelleyGovernanceCmdPollOutOfBoundAnswer maxAnswerIndex

  askInteractively :: GovernancePoll -> ExceptT ShelleyGovernanceCmdError IO Word
  askInteractively poll@GovernancePoll{govPollQuestion, govPollAnswers} = do
    liftIO $ BSC.hPutStrLn stderr $ Text.encodeUtf8 $ Text.intercalate "\n"
      ( govPollQuestion
      : [ "[" <> textShow ix <> "] " <> answer
        | (ix :: Int, answer) <- zip [0..] govPollAnswers
        ]
      )
    liftIO $ BSC.hPutStrLn stderr ""
    liftIO $ BSC.hPutStr stderr "Please indicate an answer (by index): "
    txt <- liftIO $ Text.hGetLine stdin
    liftIO $ BSC.hPutStrLn stderr ""
    case Text.decimal txt of
      Right (choice, rest) | Text.null rest ->
        choice <$ validateChoice poll choice
      _ ->
        left ShelleyGovernanceCmdPollInvalidChoice

runGovernanceVerifyPoll
  :: File GovernancePoll In
  -> File (Tx ()) In
  -> Maybe (File () Out) -- ^ Output file
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceVerifyPoll pollFile txFile mOutFile = do
  poll <- firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $
    readFileTextEnvelope AsGovernancePoll pollFile

  txFileOrPipe <- liftIO $ fileOrPipe (unFile txFile)
  tx <- firstExceptT ShelleyGovernanceCmdCddlError . newExceptT $
    readFileTx txFileOrPipe

  signatories <- firstExceptT ShelleyGovernanceCmdVerifyPollError . newExceptT $ pure $
    verifyPollAnswer poll tx

  liftIO $ IO.hPutStrLn stderr $ "Found valid poll answer with " <> show (length signatories) <> " signatories"

  lift (writeByteStringOutput mOutFile (prettyPrintJSON signatories))
    & onLeft (left . ShelleyGovernanceCmdWriteFileError)
