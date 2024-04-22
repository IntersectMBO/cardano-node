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
import           Cardano.Api.Ledger (drepExpiry)

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as AL
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map as Map
import           Data.String
import qualified Data.Text as Text
import           Data.Word (Word32, Word64)
import           GHC.Stack (callStack)
import           Lens.Micro ((^?))
import           System.FilePath ((</>))

import           Testnet.Components.DReps (createVotingTxBody, delegateToDRep, generateVoteFiles,
                   registerDRep, retrieveTransactionId, signTx, submitTx)
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

  let sbe = ShelleyBasedEraConway
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

  -- First proposal (set activity to something feasible in the test)
  dRepActivityBeforeFirstProp <- getDRepActivityValue execConfig
  dRepActivityBeforeFirstProp === 100 -- This is the default value

  -- This proposal should pass
  firstProposalInfo <- activityChangeProposalTest execConfig epochStateView configurationFile socketPath sbe gov
                                                  "firstProposal" wallet0 Nothing [(1, "yes")] 10 10 3

  -- Now we register two new DReps
  drep2 <- registerDRep execConfig epochStateView sbe work "drep2" wallet1
  delegateToDRep execConfig epochStateView configurationFile socketPath sbe work "drep2-delegation"
                 wallet2 (defaultDelegatorStakeKeyPair 2) drep2

  drep3 <- registerDRep execConfig epochStateView sbe work "drep3" wallet0
  delegateToDRep execConfig epochStateView configurationFile socketPath sbe work "drep3-delegation"
                 wallet1 (defaultDelegatorStakeKeyPair 3) drep3

  expirationDates <- checkDRepState sbe (File configurationFile) (File socketPath) execConfig
                                    (\m -> if length m == 3 then Just $ Map.map drepExpiry m else Nothing)
  H.note_ $ "Expiration dates for the registered DReps: " ++ show expirationDates

  -- Second proposal (set activity to something else and it should fail, because of percentage being under 51)
  -- We expect to get what we set in first prop, second prop should fail
  void $ activityChangeProposalTest execConfig epochStateView configurationFile socketPath sbe gov
                                    "secondProposal" wallet2 (Just firstProposalInfo) [(1, "yes")] 7 10 30


  (EpochNo epochAfterTimeout) <- getCurrentEpochNo epochStateView sbe
  H.note_ $ "Epoch after which we are going to test timeout: " <> show epochAfterTimeout

  -- Third proposal (set activity to something else again and it should pass, because of inactivity)
  -- Because 2 out of 3 were inactive, prop should pass
  void $ activityChangeProposalTest execConfig epochStateView configurationFile socketPath sbe gov
                                    "thirdProposal" wallet0 (Just firstProposalInfo) [(1, "yes")] 8 8 2


activityChangeProposalTest
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t)
  => H.ExecConfig
  -> EpochStateView
  -> FilePath
  -> FilePath
  -> ShelleyBasedEra ConwayEra
  -> FilePath
  -> FilePath
  -> PaymentKeyInfo
  -> Maybe (String, Word32)
  -> t (Int, String)
  -> Word32
  -> Integer
  -> Word64
  -> m (String, Word32)
activityChangeProposalTest execConfig epochStateView configurationFile socketPath sbe work prefix
                           wallet previousProposalInfo votes change expected epochsToWait = do

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let propVotes :: [(String, Int)]
      propVotes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  (EpochNo epochBeforeProp) <- getCurrentEpochNo epochStateView sbe
  H.note_ $ "Epoch before \"" <> prefix <> "\" prop: " <> show epochBeforeProp

  thisProposal@(governanceActionTxId, governanceActionIndex) <-
    makeActivityChangeProposal execConfig epochStateView (File configurationFile) (File socketPath)
                               sbe baseDir "proposal" previousProposalInfo change wallet

  voteChangeProposal execConfig epochStateView sbe baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes wallet

  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView sbe
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp

  void $ waitUntilEpoch (File configurationFile) (File socketPath) (EpochNo (epochAfterProp + epochsToWait))
  dRepActivityAfterProp <- getDRepActivityValue execConfig

  dRepActivityAfterProp === expected

  return thisProposal

makeActivityChangeProposal
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> EpochStateView
  -> NodeConfigFile 'In
  -> SocketPath
  -> ShelleyBasedEra ConwayEra
  -> FilePath
  -> String
  -> Maybe (String, Word32)
  -> Word32
  -> PaymentKeyInfo
  -> m (String, Word32)
makeActivityChangeProposal execConfig epochStateView configurationFile socketPath
                           sbe work prefix prevGovActionInfo drepActivity wallet = do

  let era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let stakeVkeyFp = baseDir </> "stake.vkey"
      stakeSKeyFp = baseDir </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen baseDir
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  proposalAnchorFile <- H.note $ baseDir </> "sample-proposFal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  minDRepDeposit <- getMinDRepDeposit execConfig

  proposalFile <- H.note $ baseDir </> "sample-proposFal-anchor"

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
                                        (EpochNo 40)

  governanceActionIndex <- case propSubmittedResult of
                             Left e ->
                               H.failMessage callStack
                                 $ "findCondition failed with: " <> displayError e
                             Right Nothing ->
                               H.failMessage callStack "Couldn't find proposal."
                             Right (Just a) -> return a

  return (governanceActionTxId, governanceActionIndex)

voteChangeProposal :: (MonadTest m, MonadIO m, MonadCatch m, H.MonadAssertion m)
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


getDRepActivityValue :: (MonadTest m, MonadCatch m, MonadIO m) => H.ExecConfig -> m Integer
getDRepActivityValue execConfig = do
  govStateString <- H.execCli' execConfig
    [ "conway", "query", "gov-state"
    , "--volatile-tip"
    ]

  govStateJSON <- H.nothingFail (Aeson.decode (pack govStateString) :: Maybe Aeson.Value)
  let mDRepActivityValue :: Maybe Integer
      mDRepActivityValue = govStateJSON
                             ^? AL.key "currentPParams"
                              . AL.key "dRepActivity"
                              . AL._Integer
  evalMaybe mDRepActivityValue
