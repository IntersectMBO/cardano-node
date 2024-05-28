{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.ProposeNewConstitution
  ( hprop_ledger_events_propose_new_constitution
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (EpochInterval (..))

import qualified Cardano.Crypto.Hash as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.Hashes as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.State.Strict (StateT)
import           Data.Maybe
import           Data.Maybe.Strict
import           Data.String
import qualified Data.Text as Text
import           GHC.Exts (IsList (..))
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import           Testnet.EpochStateProcessing (waitForGovActionVotes)
import           Testnet.Process.Cli.DRep
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/ProposeAndRatifyNewConstitution/"'@
hprop_ledger_events_propose_new_constitution :: Property
hprop_ledger_events_propose_new_constitution = integrationWorkspace "propose-new-constitution" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Generate model for votes
  let allVotes :: [(String, Int)]
      allVotes = zip (concatMap (uncurry replicate) [(4, "yes"), (3, "no"), (2, "abstain")]) [1..]
  annotateShow allVotes

  let numVotes :: Int
      numVotes = length allVotes
  annotateShow numVotes

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 200
        , cardanoNodeEra = cEra
        , cardanoNumDReps = numVotes
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:_
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

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ gov </> "sample-proposal-anchor"
  consitutionFile <- H.note $ gov </> "sample-constitution"
  constitutionActionFp <- H.note $ gov </> "constitution.action"

  H.writeFile proposalAnchorFile "dummy anchor data"
  H.writeFile consitutionFile "dummy constitution data"
  constitutionHash <- execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", consitutionFile
    ]

  proposalAnchorDataHash <- execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  cliStakeAddressKeyGen
    $ KeyPair { verificationKey = File stakeVkeyFp
              , signingKey = File stakeSKeyFp
              }

  -- Create constitution proposal
  guardRailScriptFp <- H.note $ work </> "guard-rail-script.plutusV3"
  H.writeFile guardRailScriptFp $ Text.unpack plutusV3NonSpendingScript
  -- TODO: Update help text for policyid. The script hash is not
  -- only useful for minting scripts
  constitutionScriptHash <- filter (/= '\n') <$>
    execCli' execConfig
      [ anyEraToString cEra, "transaction"
      , "policyid"
      , "--script-file", guardRailScriptFp
      ]

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo
  void $ execCli' execConfig
    [ "conway", "governance", "action", "create-constitution"
    , "--testnet"
    , "--governance-action-deposit", show minDRepDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", "https://tinyurl.com/2pahcy6z"
    , "--constitution-hash", constitutionHash
    , "--constitution-script-hash", constitutionScriptHash
    , "--out-file", constitutionActionFp
    ]

  txbodyFp <- H.note $ work </> "tx.body"

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  void $ execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_000
    , "--proposal-file", constitutionActionFp
    , "--out-file", txbodyFp
    ]

  signedProposalTx <- signTx execConfig cEra gov "signed-proposal"
                           (File txbodyFp) [SomeKeyPair $ paymentKeyInfoPair wallet1]

  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  governanceActionIndex <-
    H.nothingFailM . fmap snd . watchEpochStateUpdate epochStateView (EpochInterval 1) $ \(anyNewEpochState, _, _) -> do
      let r = maybeExtractGovernanceActionIndex (fromString governanceActionTxId) anyNewEpochState
      pure (if isJust r then ConditionMet else ConditionNotMet, r)

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  voteFiles <- generateVoteFiles execConfig work "vote-files"
                                 governanceActionTxId governanceActionIndex
                                 [(defaultDRepKeyPair idx, vote) | (vote, idx) <- allVotes]

  -- Submit votes
  voteTxBodyFp <- createVotingTxBody execConfig epochStateView sbe work "vote-tx-body"
                                     voteFiles wallet0

  let signingKeys = SomeKeyPair <$> (paymentKeyInfoPair wallet0:(defaultDRepKeyPair . snd <$> allVotes))
  voteTxFp <- signTx execConfig cEra gov "signed-vote-tx" voteTxBodyFp signingKeys

  submitTx execConfig cEra voteTxFp

  waitForGovActionVotes epochStateView (EpochInterval 1)

  -- Count votes before checking for ratification. It may happen that the proposal gets removed after
  -- ratification because of a long waiting time, so we won't be able to access votes.
  govState <- getGovState epochStateView ceo
  govActionState <- H.headM $ govState ^. L.cgsProposalsL . L.pPropsL . to toList
  let votes = govActionState ^. L.gasDRepVotesL . to toList

  length (filter ((== L.VoteYes) . snd) votes) === 4
  length (filter ((== L.VoteNo) . snd) votes) === 3
  length (filter ((== L.Abstain) . snd) votes) === 2
  length votes === numVotes

  -- We check that constitution was succcessfully ratified
  void . H.leftFailM . evalIO . runExceptT $
    foldEpochState
      configurationFile
      socketPath
      FullValidation
      (EpochNo 10)
      ()
      (\epochState _ _ -> foldBlocksCheckConstitutionWasRatified constitutionHash constitutionScriptHash epochState)

foldBlocksCheckConstitutionWasRatified
  :: String -- submitted constitution hash
  -> String -- submitted guard rail script hash
  -> AnyNewEpochState
  -> StateT s IO LedgerStateCondition -- ^ Accumulator at block i and fold status
foldBlocksCheckConstitutionWasRatified submittedConstitutionHash submittedGuardRailScriptHash anyNewEpochState =
  if filterRatificationState submittedConstitutionHash submittedGuardRailScriptHash anyNewEpochState
  then return ConditionMet
  else return ConditionNotMet

-- cgsDRepPulsingStateL . ratifyStateL
filterRatificationState
  :: String -- ^ Submitted constitution anchor hash
  -> String -- ^ Submitted guard rail script hash
  -> AnyNewEpochState
  -> Bool
filterRatificationState c guardRailScriptHash (AnyNewEpochState sbe newEpochState) = do
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "filterRatificationState: Only conway era supported")

    (const $ do
      let rState = Ledger.extractDRepPulsingState $ newEpochState ^. L.newEpochStateGovStateL . L.drepPulsingStateGovStateL
          constitution = rState ^. Ledger.rsEnactStateL . Ledger.ensConstitutionL
          constitutionAnchorHash = Ledger.anchorDataHash $ Ledger.constitutionAnchor constitution
          L.ScriptHash constitutionScriptHash = fromMaybe (error "filterRatificationState: consitution does not have a guardrail script")
                                                $ strictMaybeToMaybe $ constitution ^. Ledger.constitutionScriptL
      Text.pack c == renderSafeHashAsHex constitutionAnchorHash && L.hashToTextAsHex constitutionScriptHash == Text.pack guardRailScriptHash

    )
    sbe
