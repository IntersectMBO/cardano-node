{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Gov.DRepActivity
  ( hprop_check_drep_activity
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyLedgerEra)
import           Cardano.Api.Ledger (EpochInterval (EpochInterval, unEpochInterval), drepExpiry)

import           Cardano.Ledger.Conway.Core (EraGov, curPParamsGovStateL)
import           Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppDRepActivityL)
import           Cardano.Ledger.Shelley.LedgerState (epochStateGovStateL, nesEpochStateL)
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Data.Data (Typeable)
import qualified Data.Map as Map
import           Data.Word (Word16)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Defaults (defaultDRepKeyPair, defaultDelegatorStakeKeyPair)
import           Testnet.Process.Cli.DRep
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Types

import           Hedgehog (MonadTest, Property, annotateShow)
import qualified Hedgehog.Extras as H


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/DRep Activity/"'@
hprop_check_drep_activity :: Property
hprop_check_drep_activity = integrationWorkspace "test-activity" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog $ \watchdog -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Create default testnet with 3 DReps and 3 stake holders delegated, one to each DRep.
  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 200
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 1
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:wallet2:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- This proposal should pass
  let minEpochsToWaitIfChanging = EpochInterval 0 -- We don't need a min wait since we are changing
                                                  -- the parameter, to a new value, if the parameter
                                                  -- becomes the new value we will know the proposal
                                                  -- passed.
      minEpochsToWaitIfNotChanging = EpochInterval 2 -- We are not making a change to a parameter
                                                     -- so we are testing the absence of a change and
                                                     -- that means we have to wait some time to
                                                     -- make sure it doesn't change.
      maxEpochsToWaitAfterProposal = EpochInterval 2 -- If it takes more than 2 epochs we give up in any case.
      firstTargetDRepActivity = EpochInterval 3
  void $ activityChangeProposalTest execConfig epochStateView ceo gov
                                    "firstProposal" wallet0 [(1, "yes")] firstTargetDRepActivity
                                    minEpochsToWaitIfChanging (Just firstTargetDRepActivity)
                                    maxEpochsToWaitAfterProposal

  -- Now we register two new DReps
  drep2 <- registerDRep execConfig epochStateView ceo work "drep2" wallet1
  delegateToDRep execConfig epochStateView sbe work "drep2-delegation"
                 wallet2 (defaultDelegatorStakeKeyPair 2) drep2

  drep3 <- registerDRep execConfig epochStateView ceo work "drep3" wallet0
  delegateToDRep execConfig epochStateView sbe work "drep3-delegation"
                 wallet1 (defaultDelegatorStakeKeyPair 3) drep3

  expirationDates <- checkDRepState epochStateView sbe $ \m ->
    if length m == 3
       then Just $ Map.map drepExpiry m
       else Nothing
  H.note_ $ "Expiration dates for the registered DReps: " ++ show expirationDates

  -- This proposal should fail because there is 2 DReps that don't vote (out of 3)
  -- and we have the stake distributed evenly
  let secondTargetDRepActivity = EpochInterval (unEpochInterval firstTargetDRepActivity + 1)
  void $ activityChangeProposalTest execConfig epochStateView ceo gov
                                    "failingProposal" wallet2 [(1, "yes")] secondTargetDRepActivity
                                    minEpochsToWaitIfNotChanging (Just firstTargetDRepActivity)
                                    maxEpochsToWaitAfterProposal

  H.kickWatchdog watchdog

  -- We now send a bunch of proposals to make sure that the 2 new DReps expire.
  -- because DReps won't expire if there is not enough activity (opportunites to participate).
  -- This is accounted for by the dormant epoch count
  let numOfFillerProposals = 4 :: Int
  sequence_
    [activityChangeProposalTest execConfig epochStateView ceo gov
                                ("fillerProposalNum" ++ show proposalNum) wallet [(1, "yes")]
                                (EpochInterval (unEpochInterval secondTargetDRepActivity + fromIntegral proposalNum))
                                minEpochsToWaitIfNotChanging Nothing
                                maxEpochsToWaitAfterProposal
     | (proposalNum, wallet) <- zip [1..numOfFillerProposals] (cycle [wallet0, wallet1, wallet2])]

  (EpochNo epochAfterTimeout) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after which we are going to test timeout: " <> show epochAfterTimeout

  -- Last proposal (set activity to something else again and it should pass, because of inactivity)
  -- Because 2 out of 3 DReps were inactive, prop should pass
  let lastTargetDRepActivity = EpochInterval (unEpochInterval secondTargetDRepActivity + fromIntegral numOfFillerProposals + 1)
  void $ activityChangeProposalTest execConfig epochStateView ceo gov
                                    "lastProposal" wallet0 [(1, "yes")] lastTargetDRepActivity
                                    minEpochsToWaitIfChanging (Just lastTargetDRepActivity)
                                    maxEpochsToWaitAfterProposal

-- | This function creates a proposal to change the DRep activity interval
-- and issues the specified votes using default DReps. Optionally, it also
-- waits checks the expected effect of the proposal.
activityChangeProposalTest
  :: forall m t era . (HasCallStack, MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t, Typeable era,
      EraGov (ShelleyLedgerEra era), ConwayEraPParams (ShelleyLedgerEra era))
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ConwayEraOnwards era -- ^ The ConwayEraOnwards witness for current era.
  -> FilePath -- ^ Base directory path where generated files will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder.
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transactions.
  -> t (Int, String) -- ^ Votes to be casted for the proposal. Each tuple contains the number
                     -- of votes of each type and the type of vote (i.e: "yes", "no", "abstain").
  -> EpochInterval -- ^ The target DRep activity interval to be set by the proposal.
  -> EpochInterval -- ^ The minimum number of epochs to wait before checking the proposal result.
  -> Maybe EpochInterval -- ^ The expected DRep activity interval after the proposal is applied,
                         -- or 'Nothing' if there are no expectations about whether the result of
                         -- the proposal.
  -> EpochInterval -- ^ The maximum number of epochs to wait for the DRep activity interval to
                   -- become expected value.
  -> m (String, Word16) -- ^ The transaction id and the index of the governance action.
activityChangeProposalTest execConfig epochStateView ceo work prefix
                           wallet votes change minWait mExpected maxWait = do
  let sbe = conwayEraOnwardsToShelleyBasedEra ceo

  mPreviousProposalInfo <- getLastPParamUpdateActionId execConfig

  baseDir <- H.createDirectoryIfMissing $ work </> prefix
  let propVotes :: [(String, Int)]
      propVotes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  (EpochNo epochBeforeProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch before \"" <> prefix <> "\" prop: " <> show epochBeforeProp

  thisProposal@(governanceActionTxId, governanceActionIndex) <-
    makeActivityChangeProposal execConfig epochStateView ceo baseDir "proposal"
                               mPreviousProposalInfo change wallet maxWait

  voteChangeProposal execConfig epochStateView sbe baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes wallet

  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp

  -- Wait for the number of epochs for an enactment of the change
  void $ waitForEpochs epochStateView minWait

  forM_ mExpected $
    -- enactment check
    assertNewEpochState epochStateView sbe maxWait
      (nesEpochStateL . epochStateGovStateL . curPParamsGovStateL . ppDRepActivityL)

  pure thisProposal

-- | Cast votes for a governance action.
voteChangeProposal
  :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m, H.MonadAssertion m, Typeable era)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.v
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ShelleyBasedEra era -- ^ The 'ShelleyBasedEra' witness for current era.
  -> FilePath -- ^ Base directory path where generated files will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder.
  -> String -- ^ The transaction id of the governance action to vote.
  -> Word16 -- ^ The index of the governance action to vote.
  -> [([Char], Int)] -- ^ Votes to be casted for the proposal. Each tuple contains the index
                     -- of the default DRep that will make the vote and the type of the vote
                     -- (i.e: "yes", "no", "abstain").
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transaction.
  -> m ()
voteChangeProposal execConfig epochStateView sbe work prefix governanceActionTxId governanceActionIndex votes wallet = withFrozenCallStack $ do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  voteFiles <- generateVoteFiles execConfig baseDir "vote-files"
                                 governanceActionTxId governanceActionIndex
                                 [(defaultDRepKeyPair idx, vote) | (vote, idx) <- votes]

  voteTxBodyFp <- createVotingTxBody execConfig epochStateView sbe baseDir "vote-tx-body"
                                     voteFiles wallet

  let signingKeys = SomeKeyPair <$> (paymentKeyInfoPair wallet:(defaultDRepKeyPair . snd <$> votes))
  voteTxFp <- signTx execConfig cEra baseDir "signed-vote-tx" voteTxBodyFp signingKeys

  submitTx execConfig cEra voteTxFp
