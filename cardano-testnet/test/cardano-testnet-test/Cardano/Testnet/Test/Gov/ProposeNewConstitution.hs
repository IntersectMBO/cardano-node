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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import           Data.Default.Class
import           Data.Maybe
import           Data.Maybe.Strict
import           Data.String
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as Vector
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
  let proposalAnchorUrl = "https://tinyurl.com/3wrwb2as"
  constitutionFile <- H.note $ gov </> "sample-constitution"
  let constitutionAnchorUrl = "https://tinyurl.com/2pahcy6z"
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
    , "--anchor-url", proposalAnchorUrl
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", constitutionAnchorUrl
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

  let txNoNewline = Text.unpack (Text.strip (Text.pack txid))
  H.noteShow_ txNoNewline

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

  proposalsJSON :: Aeson.Value <- execCliStdoutToJson execConfig
                                    [ eraName, "query", "proposals", "--governance-action-tx-id", txNoNewline
                                    , "--governance-action-index", "0"
                                    ]

  -- Display JSON returned in case of failure
  H.note_ $ Text.unpack . decodeUtf8 $ prettyPrintJSON proposalsJSON

  -- Check that the proposals array has only one element and fetch it
  proposalsArray <- H.evalMaybe $ proposalsJSON ^? Aeson._Array
  length proposalsArray === 1
  let proposal = proposalsArray Vector.! 0

  -- Check TxId returned is the same as the one we used
  proposalsTxId <- H.evalMaybe $ proposal ^? Aeson.key "actionId" . Aeson.key "txId" . Aeson._String
  proposalsTxId === Text.pack txNoNewline

  -- Check that committeeVotes is an empty object
  proposalsCommitteeVotes <- H.evalMaybe $ proposal ^? Aeson.key "committeeVotes" . Aeson._Object
  proposalsCommitteeVotes === mempty

  -- Check that dRepVotes has the expected number of votes
  proposalsDRepVotes <- H.evalMaybe $ proposal ^? Aeson.key "dRepVotes" . Aeson._Object
  length proposalsDRepVotes === numVotes

  -- Fetch proposalProcedure and anchor
  proposalsProcedure <- H.evalMaybe $ proposal ^? Aeson.key "proposalProcedure"
  proposalsAnchor <- H.evalMaybe $ proposalsProcedure ^? Aeson.key "anchor"

  -- Check the dataHash of the anchor is the expected one
  proposalsAnchorDataHash <- H.evalMaybe $ proposalsAnchor ^? Aeson.key "dataHash" . Aeson._String
  proposalsAnchorDataHash === Text.pack proposalAnchorDataHash

  -- Check the url of the anchor is the expected one
  proposalsAnchorUrl <- H.evalMaybe $ proposalsAnchor ^? Aeson.key "url" . Aeson._String
  proposalsAnchorUrl === Text.pack proposalAnchorUrl

  -- Check the deposit amount is the expected one
  proposalsDeposit <- H.evalMaybe $ proposalsProcedure ^? Aeson.key "deposit" . Aeson._Integer
  proposalsDeposit === 1_000_000

  -- Ensure there is only one non-null content in the proposalProcedure and fetch it
  proposalsContents <- H.evalMaybe $ proposalsProcedure ^? Aeson.key "govAction" . Aeson.key "contents" . Aeson._Array
  let nonEmptyContents = Vector.filter (/= Aeson.Null) proposalsContents
  length nonEmptyContents === 1
  let firstContent = nonEmptyContents Vector.! 0

  -- Check the constitution hash and url are the expected ones
  proposalsConstitutionAnchor <- H.evalMaybe $ firstContent ^? Aeson.key "anchor"
  proposalsConstitutionAnchorDataHash <- H.evalMaybe $ proposalsConstitutionAnchor ^? Aeson.key "dataHash" . Aeson._String
  proposalsConstitutionAnchorDataHash === Text.pack constitutionHash

  proposalsConstitutionAnchorUrl <- H.evalMaybe $ proposalsConstitutionAnchor ^? Aeson.key "url" . Aeson._String
  proposalsConstitutionAnchorUrl === Text.pack constitutionAnchorUrl

  -- Check the constitution script hash is the expected one
  proposalsScriptHash <- H.evalMaybe $ firstContent ^? Aeson.key "script" . Aeson._String
  proposalsScriptHash === Text.pack constitutionScriptHash

  -- Check the tag of the govAction is "NewConstitution"
  proposalsTag <- H.evalMaybe $ proposalsProcedure ^? Aeson.key "govAction" . Aeson.key "tag" . Aeson._String
  proposalsTag === "NewConstitution"

  -- Check the stake pool votes are empty
  proposalsStakePoolVotes <- H.evalMaybe $ proposal ^? Aeson.key "stakePoolVotes" . Aeson._Object
  proposalsStakePoolVotes === mempty

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
