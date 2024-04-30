{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.DRepActivity
  ( hprop_check_drep_activity
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Error (displayError)
import           Cardano.Api.Ledger (EpochInterval (EpochInterval), drepExpiry)

import           Cardano.Ledger.Conway.Core (curPParamsGovStateL)
import           Cardano.Ledger.Conway.PParams (ppDRepActivityL)
import           Cardano.Ledger.Shelley.API (NewEpochState (..))
import           Cardano.Ledger.Shelley.LedgerState (epochStateGovStateL, nesEpochStateL)
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Map as Map
import           Data.String
import qualified Data.Text as Text
import           Data.Word (Word32, Word64)
import           GHC.Stack (HasCallStack, callStack)
import           Lens.Micro ((^.))
import           System.FilePath ((</>))

import           Testnet.Components.DReps (createVotingTxBody, delegateToDRep, generateVoteFiles,
                   getLastPParamUpdateActionId, registerDRep, retrieveTransactionId, signTx,
                   submitTx)
import           Testnet.Components.Query (EpochStateView, checkDRepState,
                   findLargestUtxoForPaymentKey, getCurrentEpochNo, getEpochStateView,
                   getMinDRepDeposit)
import           Testnet.Defaults
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/DRep Activity/"'@
hprop_check_drep_activity :: Property
hprop_check_drep_activity = H.integrationWorkspace "test-activity" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
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

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  epochStateView <- getEpochStateView (File configurationFile) (File socketPath)

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- This proposal should pass
  let minEpochsToWaitIfChanging = 0 -- The change already provides a min bound
      minEpochsToWaitIfNotChanging = 3 -- We cannot wait for change since there is no change (we wait a bit)
      maxEpochsToWaitAfterProposal = 10 -- If it takes more than 10 epochs we give up in any case
      firstTargetDRepActivity = 3
  void $ activityChangeProposalTest execConfig epochStateView configurationFile socketPath ceo gov
                                    "firstProposal" wallet0 [(1, "yes")] firstTargetDRepActivity
                                    minEpochsToWaitIfChanging (Just firstTargetDRepActivity)
                                    maxEpochsToWaitAfterProposal

  -- Now we register two new DReps
  drep2 <- registerDRep execConfig epochStateView ceo work "drep2" wallet1
  delegateToDRep execConfig epochStateView configurationFile socketPath sbe work "drep2-delegation"
                 wallet2 (defaultDelegatorStakeKeyPair 2) drep2

  drep3 <- registerDRep execConfig epochStateView ceo work "drep3" wallet0
  delegateToDRep execConfig epochStateView configurationFile socketPath sbe work "drep3-delegation"
                 wallet1 (defaultDelegatorStakeKeyPair 3) drep3

  expirationDates <- checkDRepState epochStateView sbe $ \m ->
    if length m == 3
       then Just $ Map.map drepExpiry m
       else Nothing
  H.note_ $ "Expiration dates for the registered DReps: " ++ show expirationDates

  -- This proposal should fail because there is 2 DReps that don't vote (out of 3)
  -- and we have the stake distributed evenly
  let secondTargetDRepActivity = firstTargetDRepActivity + 1
  void $ activityChangeProposalTest execConfig epochStateView configurationFile socketPath ceo gov
                                    "failingProposal" wallet2 [(1, "yes")] secondTargetDRepActivity
                                    minEpochsToWaitIfNotChanging (Just firstTargetDRepActivity)
                                    maxEpochsToWaitAfterProposal

  -- We now send a bunch of proposals to make sure that the 2 new DReps expire.
  -- because DReps won't expire if there is not enough activity (opportunites to participate).
  -- This is accounted for by the dormant epoch count
  let numOfFillerProposals = 4 :: Int
  sequence_
    [activityChangeProposalTest execConfig epochStateView configurationFile socketPath ceo gov
                                ("fillerProposalNum" ++ show proposalNum) wallet [(1, "yes")]
                                (secondTargetDRepActivity + fromIntegral proposalNum)
                                minEpochsToWaitIfNotChanging Nothing
                                maxEpochsToWaitAfterProposal
     | (proposalNum, wallet) <- zip [1..numOfFillerProposals] (cycle [wallet0, wallet1, wallet2])]

  (EpochNo epochAfterTimeout) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after which we are going to test timeout: " <> show epochAfterTimeout

  -- Last proposal (set activity to something else again and it should pass, because of inactivity)
  -- Because 2 out of 3 DReps were inactive, prop should pass
  let lastTargetDRepActivity = secondTargetDRepActivity + fromIntegral numOfFillerProposals + 1
  void $ activityChangeProposalTest execConfig epochStateView configurationFile socketPath ceo gov
                                    "lastProposal" wallet0 [(1, "yes")] lastTargetDRepActivity
                                    minEpochsToWaitIfChanging (Just lastTargetDRepActivity)
                                    maxEpochsToWaitAfterProposal

activityChangeProposalTest
  :: (HasCallStack, MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t)
  => H.ExecConfig
  -> EpochStateView
  -> FilePath
  -> FilePath
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> FilePath
  -> PaymentKeyInfo
  -> t (Int, String)
  -> Word32
  -> Word64
  -> Maybe Word32
  -> Word64
  -> m (String, Word32)
activityChangeProposalTest execConfig epochStateView configurationFile socketPath ceo work prefix
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
    makeActivityChangeProposal execConfig epochStateView (File configurationFile) (File socketPath)
                               ceo baseDir "proposal" mPreviousProposalInfo change wallet (epochBeforeProp + maxWait)

  voteChangeProposal execConfig epochStateView sbe baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes wallet

  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp

  waitAndCheck epochAfterProp

  return thisProposal

  where
    waitAndCheck :: (MonadTest m, MonadIO m)
                 => Word64 -> m ()
    waitAndCheck epochAfterProp = do
      !eProposalResult
        <- evalIO . runExceptT $ foldEpochState
                                    (File configurationFile)
                                    (File socketPath)
                                    FullValidation
                                    (EpochNo (epochAfterProp + maxWait))
                                    ()
                                    (\epochState _ _ -> filterEpochState (isSuccess epochAfterProp) epochState)
      void $ evalEither eProposalResult

    filterEpochState :: (EpochNo -> EpochInterval -> Bool) -> AnyNewEpochState -> StateT () IO LedgerStateCondition
    filterEpochState f (AnyNewEpochState sbe newEpochState) =
        caseShelleyToBabbageOrConwayEraOnwards
          (const $ error "activityChangeProposalTest: Only conway era onwards supported")
          (const $ do
            let pParams = newEpochState ^. nesEpochStateL . epochStateGovStateL . curPParamsGovStateL . ppDRepActivityL
                currEpoch = nesEL newEpochState
            return (if f currEpoch pParams
                    then ConditionMet
                    else ConditionNotMet)
          )
          sbe

    isSuccess :: Word64 -> EpochNo -> EpochInterval -> Bool
    isSuccess epochAfterProp (EpochNo epochNo) (EpochInterval epochInterval) =
      (epochAfterProp + minWait <= epochNo) &&
      (case mExpected of
        Nothing -> True
        Just expected -> epochInterval == expected) &&
      (epochNo <= epochAfterProp + maxWait)


makeActivityChangeProposal
  :: (HasCallStack, H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> EpochStateView
  -> NodeConfigFile 'In
  -> SocketPath
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> String
  -> Maybe (String, Word32)
  -> Word32
  -> PaymentKeyInfo
  -> Word64
  -> m (String, Word32)
makeActivityChangeProposal execConfig epochStateView configurationFile socketPath
                           ceo work prefix prevGovActionInfo drepActivity wallet timeout = do

  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let stakeVkeyFp = baseDir </> "stake.vkey"
      stakeSKeyFp = baseDir </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen baseDir
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  proposalAnchorFile <- H.note $ baseDir </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo

  proposalFile <- H.note $ baseDir </> "sample-proposal-anchor"

  void $ H.execCli' execConfig $
    [ "conway", "governance", "action", "create-protocol-parameters-update"
    , "--testnet"
    , "--governance-action-deposit", show @Integer minDRepDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    ] ++ concatMap (\(prevGovernanceActionTxId, prevGovernanceActionIndex) ->
                      [ "--prev-governance-action-tx-id", prevGovernanceActionTxId
                      , "--prev-governance-action-index", show prevGovernanceActionIndex
                      ]) prevGovActionInfo ++
    [ "--drep-activity", show drepActivity
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--out-file", proposalFile
    ]

  proposalBody <- H.note $ baseDir </> "tx.body"
  txIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn txIn
    , "--proposal-file", proposalFile
    , "--out-file", proposalBody
    ]

  signedProposalTx <- signTx execConfig cEra baseDir "signed-proposal"
                             (File proposalBody) [paymentKeyInfoPair wallet]

  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  !propSubmittedResult <- findCondition (maybeExtractGovernanceActionIndex sbe (fromString governanceActionTxId))
                                        (unFile configurationFile)
                                        (unFile socketPath)
                                        (EpochNo timeout)

  governanceActionIndex <- case propSubmittedResult of
                             Left e ->
                               H.failMessage callStack
                                 $ "makeActivityChangeProposal failed waiting for gov action with: " <> displayError e
                             Right Nothing ->
                               H.failMessage callStack "Couldn't find proposal."
                             Right (Just a) -> return a

  return (governanceActionTxId, governanceActionIndex)

voteChangeProposal :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m, H.MonadAssertion m)
  => H.ExecConfig
  -> EpochStateView
  -> ShelleyBasedEra ConwayEra
  -> FilePath
  -> FilePath
  -> String
  -> Word32
  -> [([Char], Int)]
  -> PaymentKeyInfo
  -> m ()
voteChangeProposal execConfig epochStateView sbe work prefix governanceActionTxId governanceActionIndex votes wallet = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  voteFiles <- generateVoteFiles execConfig baseDir "vote-files"
                                 governanceActionTxId governanceActionIndex
                                 [(defaultDRepKeyPair idx, vote) | (vote, idx) <- votes]

  voteTxBodyFp <- createVotingTxBody execConfig epochStateView sbe baseDir "vote-tx-body"
                                     voteFiles wallet

  voteTxFp <- signTx execConfig cEra baseDir "signed-vote-tx" voteTxBodyFp
                     (paymentKeyInfoPair wallet:[defaultDRepKeyPair n | (_, n) <- votes])
  submitTx execConfig cEra voteTxFp
