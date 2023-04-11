module Cardano.CLI.Shelley.Run.Governance
  ( ShelleyGovernanceCmdError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Control.Monad (unless, when)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, left, newExceptT,
                   onLeft)
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (VerificationKeyOrHashOrFile,
                   readVerificationKeyOrHashOrFile, readVerificationKeyOrHashOrTextEnvFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

import qualified Cardano.Ledger.Shelley.TxBody as Shelley


data ShelleyGovernanceCmdError
  = ShelleyGovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
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
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceCmdError -> Text
renderShelleyGovernanceError err =
  case err of
    ShelleyGovernanceCmdTextEnvReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdKeyReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdTextEnvWriteError fileErr -> Text.pack (displayError fileErr)
    -- TODO: The equality check is still not working for empty update proposals.
    ShelleyGovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"
    ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
       "Error creating the MIR certificate at: " <> textShow fp
       <> " The number of staking keys: " <> textShow numVKeys
       <> " and the number of reward amounts: " <> textShow numRwdAmts
       <> " are not equivalent."
    ShelleyGovernanceCmdCostModelsJsonDecodeErr fp err' ->
      "Error decoding cost model: " <> err' <> " at: " <> Text.pack fp
    ShelleyGovernanceCmdEmptyCostModel fp ->
      "The decoded cost model was empty at: " <> Text.pack fp
    ShelleyGovernanceCmdCostModelReadError err' ->
      "Error reading the cost model: " <> Text.pack (displayError err')


runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceCmd (GovernanceMIRPayStakeAddressesCertificate mirpot vKeys rewards out) =
  runGovernanceMIRCertificatePayStakeAddrs mirpot vKeys rewards out
runGovernanceCmd (GovernanceMIRTransfer amt out direction) =
  runGovernanceMIRCertificateTransfer amt out direction
runGovernanceCmd (GovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out) =
  runGovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp) =
  runGovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp

runGovernanceMIRCertificatePayStakeAddrs
  :: Shelley.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [Lovelace]     -- ^ Corresponding reward amounts (same length)
  -> OutputFile
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificatePayStakeAddrs mirPot sAddrs rwdAmts (OutputFile oFp) = do

    unless (length sAddrs == length rwdAmts) $
      left $ ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
               oFp (length sAddrs) (length rwdAmts)

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
  -> OutputFile
  -> TransferDirection
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificateTransfer ll (OutputFile oFp) direction = do
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
  -> OutputFile
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             (OutputFile oFp) = do
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
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> Maybe FilePath -- ^ Cost models file path
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams mCostModelFp = do
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
    | VerificationKeyFile vkeyFile <- genVerKeyFiles
    ]
  let genKeyHashes = fmap verificationKeyHash genVKeys
      upProp = makeShelleyUpdateProposal finalUpPprams genKeyHashes eNo

  firstExceptT ShelleyGovernanceCmdTextEnvWriteError . newExceptT
    $ writeLazyByteStringFile upFile $ textEnvelopeToJSON Nothing upProp

