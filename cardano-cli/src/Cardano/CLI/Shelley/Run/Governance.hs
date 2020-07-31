module Cardano.CLI.Shelley.Run.Governance
  ( ShelleyGovernanceCmdError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, right)

import           Cardano.Api.TextView (TextViewDescription (..), textShow)
import           Cardano.Api.Typed

import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

import qualified Shelley.Spec.Ledger.TxData as Shelley


data ShelleyGovernanceCmdError
  = ShelleyGovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | ShelleyGovernanceCmdTextEnvWriteError !(FileError ())
  | ShelleyGovernanceCmdEmptyUpdateProposalError
  | ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceCmdError -> Text
renderShelleyGovernanceError err =
  case err of
    ShelleyGovernanceCmdTextEnvReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdTextEnvWriteError fileErr -> Text.pack (displayError fileErr)
    -- TODO: The equality check is still not working for empty update proposals.
    ShelleyGovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"
    ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
       "Error creating the MIR certificate at: " <> textShow fp
       <> " The number of staking keys: " <> textShow numVKeys
       <> " and the number of reward amounts: " <> textShow numRwdAmts
       <> " are not equivalent."



runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceCmd (GovernanceMIRCertificate mirpot vKeys rewards out) = runGovernanceMIRCertificate mirpot vKeys rewards out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp) = runGovernanceUpdateProposal out eNo genVKeys ppUp

runGovernanceMIRCertificate
  :: Shelley.MIRPot
  -> [VerificationKeyFile]
  -- ^ Stake verification keys
  -> [Lovelace]
  -- ^ Reward amounts
  -> OutputFile
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificate mirPot vKeys rwdAmts (OutputFile oFp) = do
    sCreds <- mapM readStakeKeyToCred vKeys

    checkEqualKeyRewards vKeys rwdAmts

    let mirCert = makeMIRCertificate mirPot (zip sCreds rwdAmts)

    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just mirCertDesc) mirCert
  where
    mirCertDesc :: TextViewDescription
    mirCertDesc = TextViewDescription "Move Instantaneous Rewards Certificate"

    checkEqualKeyRewards :: [VerificationKeyFile] -> [Lovelace] -> ExceptT ShelleyGovernanceCmdError IO ()
    checkEqualKeyRewards keys rwds = do
       let numVKeys = length keys
           numRwdAmts = length rwds
       if numVKeys == numRwdAmts
       then return () else left $ ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach oFp numVKeys numRwdAmts

    readStakeKeyToCred :: VerificationKeyFile -> ExceptT ShelleyGovernanceCmdError IO StakeCredential
    readStakeKeyToCred (VerificationKeyFile stVKey) = do
      stakeVkey <- firstExceptT ShelleyGovernanceCmdTextEnvReadError
        . newExceptT
        $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stVKey
      right . StakeCredentialByKey $ verificationKeyHash stakeVkey

runGovernanceUpdateProposal
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams = do
    when (upPprams == mempty) $ left ShelleyGovernanceCmdEmptyUpdateProposalError
    genVKeys <- sequence
                  [ firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $
                      readFileTextEnvelope
                        (AsVerificationKey AsGenesisKey)
                        vkeyFile
                  | VerificationKeyFile vkeyFile <- genVerKeyFiles ]
    let genKeyHashes = map verificationKeyHash genVKeys
        upProp = makeShelleyUpdateProposal upPprams genKeyHashes eNo
    firstExceptT ShelleyGovernanceCmdTextEnvWriteError . newExceptT $
      writeFileTextEnvelope upFile Nothing upProp
