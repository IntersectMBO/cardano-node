module Cardano.CLI.Shelley.Run.Governance
  ( ShelleyGovernanceError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, right,
                   newExceptT)

import           Cardano.Api (ApiError, EpochNo, PParams'(..), ShelleyCoin,
                   ShelleyCredentialStaking, ShelleyPParamsUpdate,
                   StakingVerificationKey (..), StrictMaybe (..), Update (..),
                   createShelleyUpdateProposal, hashKey, mkShelleyStakingCredential,
                   readStakingVerificationKey, renderApiError, shelleyMIRCertificate,
                   textShow, writeCertificate, writeUpdate)


import           Cardano.Config.Shelley.ColdKeys (KeyError, KeyRole(..), readVerKey,
                   renderKeyError)
import           Cardano.CLI.Shelley.Parsers

data ShelleyGovernanceError
  = GovernanceWriteMIRCertError !FilePath !ApiError
  | GovernanceWriteUpdateError !FilePath !ApiError
  | GovernanceEmptyUpdateProposalError
  | GovernanceReadGenVerKeyError !FilePath !KeyError
  | GovernanceReadStakeVerKeyError !FilePath !ApiError
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceError -> Text
renderShelleyGovernanceError err =
  case err of
    GovernanceReadGenVerKeyError fp keyErr ->
      "Error reading genesis verification key at: " <> textShow fp <> " Error: " <> renderKeyError keyErr
    GovernanceReadStakeVerKeyError stKeyFp apiErr ->
      "Error reading stake key at: " <> textShow stKeyFp <> " Error: " <> renderApiError apiErr
    GovernanceWriteMIRCertError certFp apiErr ->
      "Error writing MIR certificate at: " <> textShow certFp <> " Error: " <> renderApiError apiErr
    GovernanceWriteUpdateError fp apiError ->
      "Error writing shelley update proposal at: " <> textShow fp <> " Error: " <> renderApiError apiError
    -- TODO: The equality check is still not working for empty update proposals.
    GovernanceEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"

runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyGovernanceError IO ()
runGovernanceCmd (GovernanceMIRCertificate vKeys rewards out) = runGovernanceMIRCertificate vKeys rewards out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp) = runGovernanceUpdateProposal out eNo genVKeys ppUp
runGovernanceCmd cmd = liftIO $ putStrLn $ "TODO: runGovernanceCmd: " ++ show cmd

runGovernanceMIRCertificate
  :: [VerificationKeyFile]
  -- ^ Stake verification keys
  -> [ShelleyCoin]
  -- ^ Reward amounts
  -> OutputFile
  -> ExceptT ShelleyGovernanceError IO ()
runGovernanceMIRCertificate vKeys rwdAmts (OutputFile oFp) = do
    sCreds <- mapM readStakeKeyToCred vKeys

    let mirCert = shelleyMIRCertificate $ mirMap sCreds rwdAmts

    firstExceptT (GovernanceWriteMIRCertError oFp) . newExceptT $ writeCertificate oFp mirCert
  where
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
  -> ShelleyPParamsUpdate
  -> ExceptT ShelleyGovernanceError IO ()
runGovernanceUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams' = do
    upPprams <- checkForEmptyProposal $! upPprams'
    genVKeys <- mapM
                  (\(VerificationKeyFile fp) -> do
                    gvk <- firstExceptT (GovernanceReadGenVerKeyError fp) $ readVerKey GenesisKey fp
                    pure gvk
                  )
                  genVerKeyFiles
    let genKeyHashes = map hashKey genVKeys
        upProp = ShelleyUpdate $ createShelleyUpdateProposal eNo genKeyHashes upPprams
    firstExceptT (GovernanceWriteUpdateError upFile)  . newExceptT $ writeUpdate upFile upProp
  where
    checkForEmptyProposal :: ShelleyPParamsUpdate -> ExceptT ShelleyGovernanceError IO ShelleyPParamsUpdate
    checkForEmptyProposal sPParams
      | sPParams == emptyPParamsUpdate = left GovernanceEmptyUpdateProposalError
      | otherwise = right sPParams

-- TODO: Import from shelley ledger specs
emptyPParamsUpdate :: ShelleyPParamsUpdate
emptyPParamsUpdate =
  PParams
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _keyMinRefund = SNothing,
      _keyDecayRate = SNothing,
      _poolDeposit = SNothing,
      _poolMinRefund = SNothing,
      _poolDecayRate = SNothing,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SNothing,
      _protocolVersion = SNothing,
      _minUTxOValue = SNothing
    }
