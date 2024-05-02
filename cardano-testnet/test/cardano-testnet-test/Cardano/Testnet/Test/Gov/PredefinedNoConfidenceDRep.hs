{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.PredefinedNoConfidenceDRep
  ( hprop_check_predefined_no_confidence_drep
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (EpochInterval (..), StrictMaybe (..))
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet
import           Cardano.Testnet.Test.Gov.PredefinedAbstainDRep (delegateToAutomaticDRep,
                   desiredPoolNumberProposalTest, getDesiredPoolNumberValue, voteChangeProposal)

import           Prelude

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Data.Data (Typeable)
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.Text as Text
import           Data.Word (Word32)
import           GHC.Stack (HasCallStack)
import           Lens.Micro ((^.))
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

  -- Create constitutional committee and check it exists
  constitutionalAction <- updateConstitutionalCommittee execConfig epochStateView ceo work
                                                        "committeeUpdate" wallet0 Nothing [(3, "yes")]

  -- Do some proposal and vote yes with all the DReps
  -- and assert that proposal passes.
  initialDesiredNumberOfPools <- getDesiredPoolNumberValue epochStateView ceo

  let newNumberOfDesiredPools = fromIntegral (initialDesiredNumberOfPools + 1)

  firstProposalInfo <- desiredPoolNumberProposalTest execConfig epochStateView ceo gov "firstProposal"
                                                     wallet1 Nothing [(3, "yes")] newNumberOfDesiredPools 0 (Just newNumberOfDesiredPools) 10

  -- Take the last two stake delegators and delegate them to "No Confidence".
  delegateToAlwaysNoConfidence execConfig epochStateView sbe gov "delegateToNoConfidence1"
                               wallet2 (defaultDelegatorStakeKeyPair 2)
  delegateToAlwaysNoConfidence execConfig epochStateView sbe gov "delegateToNoConfidence2"
                               wallet2 (defaultDelegatorStakeKeyPair 3)

  -- Do some other proposal and vote yes with all the DReps
  -- and assert the new proposal does NOT pass.
  let newNumberOfDesiredPools2 = fromIntegral (newNumberOfDesiredPools + 1)

  void $ desiredPoolNumberProposalTest execConfig epochStateView ceo gov "secondProposal"
                                       wallet0 (Just firstProposalInfo) [(3, "yes")] newNumberOfDesiredPools2 3 (Just newNumberOfDesiredPools) 10

  -- Create a no confidence proposal and vote "no" to the proposal with all DReps.
  -- Assert the no confidence proposal passes.
  void $ testNoConfidenceProposal execConfig epochStateView ceo gov "noConfidenceProposal"
                                  wallet1 constitutionalAction [(3, "no")]

filterCommittee :: AnyNewEpochState -> Maybe [(L.Credential L.ColdCommitteeRole L.StandardCrypto, EpochNo)]
filterCommittee (AnyNewEpochState sbe newEpochState) =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "filterNoCommittee: Only conway era supported")
    (const $ do
      let rState = L.extractDRepPulsingState $ newEpochState ^. L.newEpochStateGovStateL . L.drepPulsingStateGovStateL
          ensCommittee = rState ^. L.rsEnactStateL . L.ensCommitteeL
      case ensCommittee of
        SNothing -> Nothing
        SJust x | Map.null (L.committeeMembers x) -> Nothing
                | otherwise -> Just $ Map.toList $ L.committeeMembers x
    )
    sbe

updateConstitutionalCommittee
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t)
  => H.ExecConfig
  -> EpochStateView
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> FilePath
  -> PaymentKeyInfo
  -> Maybe (String, Word32)
  -> t (Int, String)
  -> m (String, Word32)
updateConstitutionalCommittee execConfig epochStateView ceo work prefix
                              wallet previousProposalInfo votes = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let propVotes :: [(String, Int)]
      propVotes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  let coldVKeyFile = baseDir </> "cold-key.vkey"
      coldSKeyFile = baseDir </> "cold-key.skey"

  void $ H.execCli' execConfig
      [ "conway", "governance", "committee", "key-gen-cold"
      , "--cold-verification-key-file", coldVKeyFile
      , "--cold-signing-key-file", coldSKeyFile
      ]

  coldKeyHash <- Text.unpack . Text.strip . Text.pack <$> H.execCli' execConfig
    [ "conway", "governance", "committee", "key-hash"
    , "--verification-key-file", coldVKeyFile
    ]

  thisProposal@(governanceActionTxId, governanceActionIndex) <-
    makeUpdateConstitutionalCommitteeProposal execConfig epochStateView ceo baseDir "proposal"
                                              previousProposalInfo [coldKeyHash] wallet

  voteChangeProposal execConfig epochStateView ceo baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes (zip (repeat "yes") [1..3]) wallet

  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp

  committee <- H.nothingFailM $ watchEpochStateView epochStateView (return . filterCommittee) (EpochInterval 1)

  H.note_ $ show committee

  return thisProposal

makeUpdateConstitutionalCommitteeProposal
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m, Foldable f)
  => H.ExecConfig
  -> EpochStateView
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> String
  -> Maybe (String, Word32)
  -> f String
  -> PaymentKeyInfo
  -> m (String, Word32)
makeUpdateConstitutionalCommitteeProposal execConfig epochStateView ceo work prefix
                                          prevGovActionInfo coldKeyHashes wallet = do

  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let stakeVkeyFp = baseDir </> "stake.vkey"
      stakeSKeyFp = baseDir </> "stake.skey"

  P.cliStakeAddressKeyGen
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

  void $ H.execCli' execConfig $
    [ "conway", "governance", "action", "update-committee"
    , "--testnet"
    , "--governance-action-deposit", show @Integer minDRepDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    ] ++ concatMap (\(prevGovernanceActionTxId, prevGovernanceActionIndex) ->
                      [ "--prev-governance-action-tx-id", prevGovernanceActionTxId
                      , "--prev-governance-action-index", show prevGovernanceActionIndex
                      ]) prevGovActionInfo ++
    [ "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    ] ++ concatMap (\keyHash ->
                    [ "--add-cc-cold-verification-key-hash", keyHash
                    , "--epoch", show (100 :: Int)
                    ]) coldKeyHashes ++
    [ "--threshold", "0"
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

delegateToAlwaysNoConfidence
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Typeable era, HasCallStack)
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
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t, HasCallStack)
  => H.ExecConfig
  -> EpochStateView
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> FilePath
  -> PaymentKeyInfo
  -> (String, Word32)
  -> t (Int, String)
  -> m (String, Word32)
testNoConfidenceProposal execConfig epochStateView ceo work prefix
                         wallet previousProposalInfo votes = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let propVotes :: [(String, Int)]
      propVotes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  thisProposal@(governanceActionTxId, governanceActionIndex) <-
    makeNoConfidenceProposal execConfig epochStateView ceo baseDir
                             "proposal" previousProposalInfo wallet

  voteChangeProposal execConfig epochStateView ceo baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes (zip (repeat "yes") [1..3]) wallet

  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp

  committee <- H.nothingFailM $ watchEpochStateView epochStateView (return . filterCommittee) (EpochInterval 1)

  H.note_ $ show committee

  return thisProposal


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
