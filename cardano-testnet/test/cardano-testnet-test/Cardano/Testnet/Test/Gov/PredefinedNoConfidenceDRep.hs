{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.PredefinedNoConfidenceDRep
  ( hprop_check_predefined_no_confidence_drep
  ) where

import           Cardano.Api as Api
import           Cardano.Api.IO.Base (Socket)
import           Cardano.Api.Ledger (EpochInterval (..))

import           Cardano.Testnet
import           Cardano.Testnet.Test.Gov.PredefinedAbstainDRep (delegateToAutomaticDRep,
                   desiredPoolNumberProposalTest, getDesiredPoolNumberValue, voteChangeProposal)

import           Prelude

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as AL
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Data (Typeable)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word32)
import           Lens.Micro ((^?))
import           System.FilePath ((</>))

import           Testnet.Components.Query (EpochStateView, findLargestUtxoForPaymentKey,
                   getCurrentEpochNo, getEpochStateView, getMinDRepDeposit, watchEpochStateView)
import           Testnet.Defaults (defaultDelegatorStakeKeyPair)
import qualified Testnet.Process.Cli.Keys as P
import           Testnet.Process.Cli.Transaction (retrieveTransactionId, signTx, submitTx)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Util as H
import           Testnet.Types (KeyPair (..), PaymentKeyInfo (..), PoolNode (..),
                   SomeKeyPair (SomeKeyPair), StakingKey, TestnetRuntime (..), nodeSocketPath)

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Predefined No Confidence DRep/"'@
hprop_check_predefined_no_confidence_drep :: Property
hprop_check_predefined_no_confidence_drep = H.integrationWorkspace "test-activity" $ \tempAbsBasePath' -> do
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
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 3
        }

  _testnetRuntime@TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:wallet2:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- Do some proposal and vote yes with all the DReps
  -- and assert that proposal passes.
  initialDesiredNumberOfPools <- getDesiredPoolNumberValue epochStateView ceo

  let newNumberOfDesiredPools = fromIntegral (initialDesiredNumberOfPools + 1)

  firstProposalInfo <- desiredPoolNumberProposalTest execConfig epochStateView ceo gov "firstProposal"
                                                     wallet0 Nothing [(3, "yes")] newNumberOfDesiredPools 0 (Just newNumberOfDesiredPools) 10

  -- Take the last two stake delegators and delegate them to "No Confidence".
  delegateToAlwaysNoConfidence execConfig epochStateView sbe gov "delegateToAbstain1"
                               wallet1 (defaultDelegatorStakeKeyPair 2)
  delegateToAlwaysNoConfidence execConfig epochStateView sbe gov "delegateToAbstain2"
                               wallet1 (defaultDelegatorStakeKeyPair 3)

  -- Do some other proposal and vote yes with all the DReps
  -- and assert the new proposal does NOT pass
  let newNumberOfDesiredPools2 = fromIntegral (newNumberOfDesiredPools + 1)

  void $ desiredPoolNumberProposalTest execConfig epochStateView ceo gov "secondProposal"
                                       wallet2 (Just firstProposalInfo) [(3, "yes")] newNumberOfDesiredPools2 3 (Just newNumberOfDesiredPools) 10

  -- Create a no confidence proposal and vote "no" to the proposal with all DReps.
  -- Assert the no confidence proposal passes.
  void $ testNoConfidenceProposal execConfig epochStateView configurationFile socketPath ceo work "noConfidenceProposal"
                                  wallet0 firstProposalInfo [(3, "no")] 3

delegateToAlwaysNoConfidence
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Typeable era)
  => H.ExecConfig
  -> EpochStateView
  -> ShelleyBasedEra era
  -> FilePath
  -> String
  -> PaymentKeyInfo
  -> KeyPair StakingKey
  -> m ()
delegateToAlwaysNoConfidence execConfig epochStateView sbe work prefix =
  delegateToAutomaticDRep execConfig epochStateView sbe work prefix "--always-no-confidence"

testNoConfidenceProposal
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t)
  => H.ExecConfig
  -> EpochStateView
  -> NodeConfigFile 'In
  -> File Socket 'InOut
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> FilePath
  -> PaymentKeyInfo
  -> (String, Word32)
  -> t (Int, String)
  -> Integer
  -> m (String, Word32)
testNoConfidenceProposal execConfig epochStateView configurationFile socketPath ceo work prefix
                         wallet previousProposalInfo votes epochsToWait = do

  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let propVotes :: [(String, Int)]
      propVotes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  thisProposal@(governanceActionTxId, governanceActionIndex) <-
    makeNoConfidenceProposal execConfig epochStateView ceo baseDir
                             "proposal" previousProposalInfo wallet

  voteChangeProposal execConfig epochStateView sbe baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes wallet

  -- Wait two epochs
  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp
  void $ waitUntilEpoch configurationFile socketPath (EpochNo (epochAfterProp + fromIntegral epochsToWait))

  -- We check that no confidence proposal passes
  obtainedProposalId <- getLastEnactedCommitteeActionId execConfig
  obtainedProposalId === thisProposal

  return thisProposal

getLastEnactedCommitteeActionId :: (MonadTest m, MonadCatch m, MonadIO m) => H.ExecConfig -> m (String, Word32)
getLastEnactedCommitteeActionId execConfig = do
  govStateString <- H.execCli' execConfig
    [ "conway", "query", "gov-state"
    , "--volatile-tip"
    ]

  govStateJSON <- H.nothingFail (Aeson.decode (LBS.pack govStateString) :: Maybe Aeson.Value)

  let mLastCommitteeAction :: Maybe Aeson.Value
      mLastCommitteeAction = govStateJSON
                               ^? AL.key "nextRatifyState"
                                . AL.key "nextEnactState"
                                . AL.key "prevGovActionIds"
                                . AL.key "Committee"

  lastCommitteeAction <- evalMaybe mLastCommitteeAction

  let mLastCommitteeActionIx :: Maybe Integer
      mLastCommitteeActionIx = lastCommitteeAction ^? AL.key "govActionIx"
                                                    . AL._Integer

  lastCommitteeActionIx <- fromIntegral <$> evalMaybe mLastCommitteeActionIx

  let mLastCommitteeActionTxId :: Maybe Text
      mLastCommitteeActionTxId = lastCommitteeAction ^? AL.key "txId"
                                                      . AL._String

  lastCommitteeActionTxId <- Text.unpack <$> evalMaybe mLastCommitteeActionTxId

  return (lastCommitteeActionTxId, lastCommitteeActionIx)

makeNoConfidenceProposal
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> EpochStateView
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> String
  -> (String, Word32)
  -> PaymentKeyInfo
  -> m (String, Word32)
makeNoConfidenceProposal execConfig epochStateView
                         ceo work prefix (prevGovernanceActionTxId, prevGovernanceActionIndex) wallet = do

  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let stakeVkeyFp = baseDir </> "stake.vkey"
      stakeSKeyFp = baseDir </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen
         $ KeyPair { verificationKey = File stakeVkeyFp
                   , signingKey = File stakeSKeyFp
                   }

  proposalAnchorFile <- H.note $ baseDir </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo

  proposalFile <- H.note $ baseDir </> "sample-proposal-file"

  void $ H.execCli' execConfig
    [ "conway", "governance", "action", "create-no-confidence"
    , "--testnet"
    , "--governance-action-deposit", show @Integer minDRepDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--prev-governance-action-tx-id", prevGovernanceActionTxId
    , "--prev-governance-action-index", show prevGovernanceActionIndex
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
                             (File proposalBody) [SomeKeyPair $ paymentKeyInfoPair wallet]

  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  governanceActionIndex <- H.nothingFailM $ watchEpochStateView epochStateView (return . maybeExtractGovernanceActionIndex (fromString governanceActionTxId)) (EpochInterval 1)

  return (governanceActionTxId, governanceActionIndex)
