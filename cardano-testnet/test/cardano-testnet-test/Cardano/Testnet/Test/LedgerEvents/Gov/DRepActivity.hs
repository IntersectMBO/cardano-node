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
import           Cardano.Api.Ledger (DRepState (drepExpiry))

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
import           Data.Word (Word32)
import           GHC.Stack (callStack)
import           Lens.Micro ((^?))
import           System.FilePath ((</>))

import           Testnet.Components.DReps (createVotingTxBody, generateVoteFiles,
                   retrieveTransactionId, signTx, submitTx)
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
        , cardanoNumDReps = 10
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

  let firstPropVotes :: [(String, Int)]
      firstPropVotes = zip (concatMap (uncurry replicate) [(6, "yes"), (4, "no")]) [1..]
  annotateShow firstPropVotes

  fgaInfo@(firstGovernanceActionTxId, firstGovernanceActionIndex) <-
    makeActivityChangeProposal execConfig epochStateView (File configurationFile) (File socketPath)
                               sbe gov "proposal1" Nothing 4 wallet0

  voteChangeProposal execConfig epochStateView sbe gov "vote1"
                     firstGovernanceActionTxId firstGovernanceActionIndex firstPropVotes wallet0

  (EpochNo epochAfterFirstProp) <- getCurrentEpochNo epochStateView sbe
  H.note_ $ "Epoch after first prop: " <> show epochAfterFirstProp

  void $ waitUntilEpoch (File configurationFile) (File socketPath) (EpochNo (epochAfterFirstProp + 2))
  dRepActivityAfterFirstProp <- getDRepActivityValue execConfig

  dRepActivityAfterFirstProp === 4 -- This is what we just set, should work

  result1 <- checkDRepState sbe (File configurationFile) (File socketPath) execConfig
                            (Just . map (drepExpiry . snd) . Map.toList)
  H.note_ $ "DRep expiration dates: " <> show result1

  -- Second proposal (set activity to something else and it should fail, because of percentage being under 51)
  let secondPropVotes :: [(String, Int)]
      secondPropVotes = zip (concatMap (uncurry replicate) [(4, "yes")]) [1..]
  annotateShow secondPropVotes

  (secondGovernanceActionTxId, secondGovernanceActionIndex) <-
    makeActivityChangeProposal execConfig epochStateView (File configurationFile) (File socketPath)
                               sbe gov "proposal2" (Just fgaInfo) 7 wallet1

  voteChangeProposal execConfig epochStateView sbe gov "vote2"
                     secondGovernanceActionTxId secondGovernanceActionIndex secondPropVotes wallet1

  (EpochNo epochAfterSecondProp) <- getCurrentEpochNo epochStateView sbe
  H.note_ $ "Epoch after second prop: " <> show epochAfterSecondProp

  void $ waitUntilEpoch (File configurationFile) (File socketPath) (EpochNo (epochAfterSecondProp + 5))
  dRepActivityAfterSecondProp <- getDRepActivityValue execConfig

  dRepActivityAfterSecondProp === 4 -- This is what we set in first prop, second prop should fail

  result2 <- checkDRepState sbe (File configurationFile) (File socketPath) execConfig
                            (Just . map (drepExpiry . snd) . Map.toList)
  H.note_ $ "DRep expiration dates: " <> show result2

  -- Third proposal (set activity to something else again and it should pass, because of inactivity)
  let thirdPropVotes :: [(String, Int)]
      thirdPropVotes = zip (concatMap (uncurry replicate) [(4, "yes")]) [1..]
  annotateShow thirdPropVotes

  (thirdGovernanceActionTxId, thirdGovernanceActionIndex) <-
    makeActivityChangeProposal execConfig epochStateView (File configurationFile) (File socketPath)
                               sbe gov "proposal3" (Just fgaInfo) 8 wallet2

  voteChangeProposal execConfig epochStateView sbe gov "vote3"
                     thirdGovernanceActionTxId thirdGovernanceActionIndex thirdPropVotes wallet2

  (EpochNo epochAfterThirdProp) <- getCurrentEpochNo epochStateView sbe
  H.note_ $ "Epoch after third prop: " <> show epochAfterThirdProp

  void $ waitUntilEpoch (File configurationFile) (File socketPath) (EpochNo (epochAfterThirdProp + 2))
  dRepActivityAfterThirdProp <- getDRepActivityValue execConfig

  dRepActivityAfterThirdProp === 8 -- Because 6 out of 10 were inactive, third prop should pass

  result3 <- checkDRepState sbe (File configurationFile) (File socketPath) execConfig
                            (Just . map (drepExpiry . snd) . Map.toList)
  H.note_ $ "DRep expiration dates: " <> show result3


makeActivityChangeProposal
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m, Foldable f)
  => H.ExecConfig
  -> EpochStateView
  -> NodeConfigFile 'In
  -> SocketPath
  -> ShelleyBasedEra ConwayEra
  -> FilePath
  -> String
  -> f (String, Word32)
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
                                        (EpochNo 30)

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
