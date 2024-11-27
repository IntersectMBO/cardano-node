{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.ProposeNewConstitution
  ( hprop_ledger_events_propose_new_constitution
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Experimental (Some (..))
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
import           Data.Default.Class
import           Data.Maybe
import           Data.Maybe.Strict
import           Data.String
import qualified Data.Text as Text
import           GHC.Exts (IsList (..))
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Defaults
import           Testnet.EpochStateProcessing (waitForGovActionVotes)
import           Testnet.Process.Cli.DRep
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Cli.SPO (createStakeKeyRegistrationCertificate)
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Propose And Ratify New Constitution/"'@
hprop_ledger_events_propose_new_constitution :: Property
hprop_ledger_events_propose_new_constitution = integrationWorkspace "propose-new-constitution" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
      sbe = convert ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      eraName = eraToString sbe
      fastTestnetOptions = def
        { cardanoNodeEra = AnyShelleyBasedEra sbe
        , cardanoNumDReps = fromIntegral numVotes
        }
      shelleyOptions = def { genesisEpochLength = 200 }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets=wallet0:wallet1:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ gov </> "sample-proposal-anchor"
  constitutionFile <- H.note $ gov </> "sample-constitution"
  constitutionActionFp <- H.note $ gov </> "constitution.action"

  H.writeFile proposalAnchorFile $
    unlines [ "These are the reasons:  " , "" , "1. First" , "2. Second " , "3. Third" ]
  H.copyFile
    "test/cardano-testnet-test/files/input/sample-constitution.txt"
    constitutionFile
  constitutionHash <- execCli' execConfig
    [ "hash", "anchor-data", "--file-text", constitutionFile
    ]

  proposalAnchorDataHash <- execCli' execConfig
    [ "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  -- Register stake address
  let stakeCertFp = gov </> "stake.regcert"
      stakeKeys =  KeyPair { verificationKey = File $ gov </> "stake.vkey"
                           , signingKey = File $ gov </> "stake.skey"
                           }
  cliStakeAddressKeyGen stakeKeys
  keyDeposit <- getKeyDeposit epochStateView ceo
  createStakeKeyRegistrationCertificate
    tempAbsPath (AnyShelleyBasedEra sbe) (verificationKey stakeKeys) keyDeposit stakeCertFp

  stakeCertTxBodyFp <- H.note $ work </> "stake.registration.txbody"
  stakeCertTxSignedFp <- H.note $ work </> "stake.registration.tx"

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  void $ execCli' execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 10_000_000
    , "--certificate-file", stakeCertFp
    , "--witness-override", show @Int 2
    , "--out-file", stakeCertTxBodyFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", stakeCertTxBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet1
    , "--signing-key-file", signingKeyFp stakeKeys
    , "--out-file", stakeCertTxSignedFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", stakeCertTxSignedFp
    ]

  -- Create constitution proposal
  guardRailScriptFp <- H.note $ work </> "guard-rail-script.plutusV3"
  H.writeFile guardRailScriptFp $ Text.unpack plutusV3Script
  -- TODO: Update help text for policyid. The script hash is not
  -- only useful for minting scripts
  constitutionScriptHash <- filter (/= '\n') <$>
    execCli' execConfig
      [ eraToString sbe, "transaction"
      , "policyid"
      , "--script-file", guardRailScriptFp
      ]

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo
  void $ execCli' execConfig
    [ "conway", "governance", "action", "create-constitution"
    , "--testnet"
    , "--governance-action-deposit", show minDRepDeposit
    , "--deposit-return-stake-verification-key-file", verificationKeyFp stakeKeys
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", "https://tinyurl.com/2pahcy6z"
    , "--constitution-hash", constitutionHash
    , "--constitution-script-hash", constitutionScriptHash
    , "--out-file", constitutionActionFp
    ]

  txbodyFp <- H.note $ work </> "tx.body"

  H.noteShowM_ $ waitForBlocks epochStateView 1
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
                           (File txbodyFp) [Some $ paymentKeyInfoPair wallet1]

  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  governanceActionIndex <-
    H.nothingFailM . watchEpochStateUpdate epochStateView (EpochInterval 1) $ \(anyNewEpochState, _, _) ->
    pure $ maybeExtractGovernanceActionIndex (fromString governanceActionTxId) anyNewEpochState

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  voteFiles <- generateVoteFiles execConfig work "vote-files"
                                 governanceActionTxId governanceActionIndex
                                 [(defaultDRepKeyPair idx, vote) | (vote, idx) <- allVotes]

  -- Submit votes
  voteTxBodyFp <- createVotingTxBody execConfig epochStateView sbe work "vote-tx-body"
                                     voteFiles wallet0

  let signingKeys = Some <$> (paymentKeyInfoPair wallet0:(defaultDRepKeyPair . snd <$> allVotes))
  voteTxFp <- signTx execConfig cEra gov "signed-vote-tx" voteTxBodyFp signingKeys

  submitTx execConfig cEra voteTxFp

  waitForGovActionVotes epochStateView (EpochInterval 1)

  txid <- execCli' execConfig [ eraName, "transaction", "txid", "--tx-file", unFile signedProposalTx ]

  -- Uncomment this if you wanna compare the output with "query gov-state":
  -- H.noteM_ $ execCli' execConfig [ eraName, "query", "gov-state" ]
  let txNoNewline = Text.unpack (Text.strip (Text.pack txid))
  H.noteShow_ txNoNewline
  H.noteM_ $ execCli' execConfig [ eraName, "query", "proposals", "--governance-action-tx-id", txNoNewline 
                                 , "--governance-action-index", "0" ]
  -- Command returned:
  --
  -- []

  -- Count votes before checking for ratification. It may happen that the proposal gets removed after
  -- ratification because of a long waiting time, so we won't be able to access votes.
  govState <- getGovState epochStateView ceo
  govActionState <- H.headM $ govState ^. L.cgsProposalsL . L.pPropsL . to toList
  let votes = govActionState ^. L.gasDRepVotesL . to toList

  length (filter ((== L.VoteYes) . snd) votes) === 4
  length (filter ((== L.VoteNo) . snd) votes) === 3
  length (filter ((== L.Abstain) . snd) votes) === 2
  length votes === fromIntegral numVotes

  -- We check that constitution was succcessfully ratified
  void . H.leftFailM . H.evalIO . runExceptT $
    foldEpochState
      configurationFile
      socketPath
      FullValidation
      (EpochNo 10)
      ()
      (\epochState _ _ -> foldBlocksCheckConstitutionWasRatified constitutionHash constitutionScriptHash epochState)

  H.noteM_ $ execCli' execConfig [ eraName, "query", "proposals", "--governance-action-tx-id", txNoNewline 
                                 , "--governance-action-index", "0" ]
  -- Command returned:
  --
  -- [
  --   {
  --       "actionId": {
  --           "govActionIx": 0,
  --           "txId": "d3877f2694dcd3853abf44506977cbd25f94a26cf887e426f601b29fffb256c3"
  --       },
  --       "committeeVotes": {},
  --       "dRepVotes": {
  --           "keyHash-11885af93181919dd2749bf828de656045c78f5d5f511055eec54306": "VoteNo",
  --           "keyHash-42cdea9e46396ad3f145b7834c16675ec7962c11648b5c031210ceb1": "VoteYes",
  --           "keyHash-8453f733cb6e3d873993c9e94a423b3e4385547381f10a1f4da0b995": "VoteYes",
  --           "keyHash-85454551839aab47b09f25e75deff03403047d081ab5ecff6243cb0f": "Abstain",
  --           "keyHash-adbcfc9e9bcede4ab2db8d7d85d6f556fa57fa6e2897b31592d9819c": "VoteNo",
  --           "keyHash-bbe6163dd619fa83ae1ee3a061639cc50b8305e1d97f2837458fa995": "VoteYes",
  --           "keyHash-c4f60d2624acb06de54db22fb25e59818ee62d3e7c2464974d9af728": "Abstain",
  --           "keyHash-ec9ac276b3727015dfef084ad251ff6925353581b33e3453bda9fb2f": "VoteNo",
  --           "keyHash-f6173095513bf9a575f91251649280890ca9d3849859f28179ec2a63": "VoteYes"
  --       },
  --       "expiresAfter": 2,
  --       "proposalProcedure": {
  --           "anchor": {
  --               "dataHash": "0ddee8482655dcaf1471243432069483a029bce680457d01f547f6b5f097d73f",
  --               "url": "https://tinyurl.com/3wrwb2as"
  --           },
  --           "deposit": 1000000,
  --           "govAction": {
  --               "contents": [
  --                   null,
  --                   {
  --                       "anchor": {
  --                           "dataHash": "c9247db6ab4e0d795bec6aed1b565f92f056d235f777329df546689ca834c46e",
  --                           "url": "https://tinyurl.com/2pahcy6z"
  --                       },
  --                       "script": "186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4"
  --                   }
  --               ],
  --               "tag": "NewConstitution"
  --           },
  --           "returnAddr": {
  --               "credential": {
  --                   "keyHash": "74c5f83aa5ff896923939329a1e46aadc193a44894ef6a9e969a2e22"
  --               },
  --               "network": "Testnet"
  --           }
  --       },
  --       "proposedIn": 1,
  --       "stakePoolVotes": {}
  --   }
  -- ]

  assert False

foldBlocksCheckConstitutionWasRatified
  :: String -- submitted constitution hash
  -> String -- submitted guard rail script hash
  -> AnyNewEpochState
  -> StateT s IO ConditionResult -- ^ Accumulator at block i and fold status
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
filterRatificationState c guardRailScriptHash (AnyNewEpochState sbe newEpochState _) = do
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
