module Cardano.CLI.Shelley.Run.Governance
  ( ShelleyGovernanceError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, right,
                   newExceptT)

import           Cardano.Api (ApiError, EpochNo, ShelleyCoin,
                   ShelleyCredentialStaking, StakingVerificationKey (..),
                   hashKey, mkShelleyStakingCredential,
                   readStakingVerificationKey, renderApiError, shelleyMIRCertificate,
                   textShow, writeCertificate)
import qualified Cardano.Api.Typed as Api

import           Cardano.CLI.Shelley.Parsers

import qualified Shelley.Spec.Ledger.TxData as Shelley


data ShelleyGovernanceError
  = GovernanceWriteMIRCertError !FilePath !ApiError
  | GovernanceWriteUpdateError !(Api.FileError ())
  | GovernanceEmptyUpdateProposalError
  | GovernanceReadGenVerKeyError !(Api.FileError Api.TextEnvelopeError)
  | GovernanceReadStakeVerKeyError !FilePath !ApiError
  | GovernanceMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceError -> Text
renderShelleyGovernanceError err =
  case err of
    GovernanceReadGenVerKeyError apiErr ->
      "Error reading genesis verification key: " <> Text.pack (Api.displayError apiErr)
    GovernanceReadStakeVerKeyError stKeyFp apiErr ->
      "Error reading stake key at: " <> textShow stKeyFp <> " Error: " <> renderApiError apiErr
    GovernanceWriteMIRCertError certFp apiErr ->
      "Error writing MIR certificate at: " <> textShow certFp <> " Error: " <> renderApiError apiErr
    GovernanceWriteUpdateError apiErr ->
      "Error writing shelley update proposal: " <> Text.pack (Api.displayError apiErr)
    -- TODO: The equality check is still not working for empty update proposals.
    GovernanceEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"
    GovernanceMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
       "Error creating the MIR certificate at: " <> textShow fp
       <> " The number of staking keys: " <> textShow numVKeys
       <> " and the number of reward amounts: " <> textShow numRwdAmts
       <> " are not equivalent."



runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyGovernanceError IO ()
runGovernanceCmd (GovernanceMIRCertificate mirpot vKeys rewards out) = runGovernanceMIRCertificate mirpot vKeys rewards out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp) = runGovernanceUpdateProposal out eNo genVKeys ppUp
runGovernanceCmd cmd = liftIO $ putStrLn $ "TODO: runGovernanceCmd: " ++ show cmd

runGovernanceMIRCertificate
  :: Shelley.MIRPot
  -> [VerificationKeyFile]
  -- ^ Stake verification keys
  -> [ShelleyCoin]
  -- ^ Reward amounts
  -> OutputFile
  -> ExceptT ShelleyGovernanceError IO ()
runGovernanceMIRCertificate mirPot vKeys rwdAmts (OutputFile oFp) = do
    sCreds <- mapM readStakeKeyToCred vKeys

    checkEqualKeyRewards vKeys rwdAmts

    let mirCert = shelleyMIRCertificate mirPot (mirMap sCreds rwdAmts)

    firstExceptT (GovernanceWriteMIRCertError oFp) . newExceptT $ writeCertificate oFp mirCert
  where
    checkEqualKeyRewards :: [VerificationKeyFile] -> [ShelleyCoin] -> ExceptT ShelleyGovernanceError IO ()
    checkEqualKeyRewards keys rwds = do
       let numVKeys = length keys
           numRwdAmts = length rwds
       if numVKeys == numRwdAmts
       then return () else left $ GovernanceMIRCertificateKeyRewardMistmach oFp numVKeys numRwdAmts

    readStakeKeyToCred :: VerificationKeyFile -> ExceptT ShelleyGovernanceError IO ShelleyCredentialStaking
    readStakeKeyToCred (VerificationKeyFile stVKey) = do
      StakingVerificationKeyShelley stakeVkey <-
        firstExceptT (GovernanceReadStakeVerKeyError stVKey) . newExceptT $ readStakingVerificationKey stVKey
      right . mkShelleyStakingCredential $ hashKey stakeVkey

    mirMap :: [ShelleyCredentialStaking] -> [ShelleyCoin] -> Map.Map ShelleyCredentialStaking ShelleyCoin
    mirMap stakeCreds rwds = Map.fromList $ zip stakeCreds rwds

runGovernanceUpdateProposal
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> Api.ProtocolParametersUpdate
  -> ExceptT ShelleyGovernanceError IO ()
runGovernanceUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams = do
    when (upPprams == mempty) $ left GovernanceEmptyUpdateProposalError
    genVKeys <- sequence
                  [ firstExceptT GovernanceReadGenVerKeyError . newExceptT $
                      Api.readFileTextEnvelope
                        (Api.AsVerificationKey Api.AsGenesisKey)
                        vkeyFile
                  | VerificationKeyFile vkeyFile <- genVerKeyFiles ]
    let genKeyHashes = map Api.verificationKeyHash genVKeys
        upProp = Api.makeShelleyUpdateProposal upPprams genKeyHashes eNo
    firstExceptT GovernanceWriteUpdateError . newExceptT $
      Api.writeFileTextEnvelope upFile Nothing upProp

