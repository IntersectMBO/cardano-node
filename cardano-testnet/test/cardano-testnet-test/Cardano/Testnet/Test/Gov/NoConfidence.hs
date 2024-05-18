{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.NoConfidence
  ( hprop_gov_no_confidence
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Conway.Genesis as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict
import           Data.String
import qualified Data.Text as Text
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import qualified Testnet.Process.Cli.DRep as DRep
import           Testnet.Process.Cli.Keys
import qualified Testnet.Process.Cli.SPO as SPO
import           Testnet.Process.Cli.Transaction
import qualified Testnet.Process.Run as H
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Committee Motion Of No Confidence/"'@
-- Generate a testnet with a committee defined in the Conway genesis. Submit a motion of no confidence
-- and have the required threshold of SPOs and DReps vote yes on it.
hprop_gov_no_confidence :: Property
hprop_gov_no_confidence = integrationWorkspace "no-confidence" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do

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
        }
  execConfigOffline <- H.mkExecConfigOffline tempBaseAbsPath

  -- Step 1. Define generate and define a committee in the genesis file

  -- Create committee cold key
  H.createDirectoryIfMissing_ $ tempAbsPath' </> work </> "committee-keys"
  H.forConcurrently_ [1] $ \n -> do
    H.execCli' execConfigOffline
      [ anyEraToString cEra, "governance", "committee"
      , "key-gen-cold"
      , "--cold-verification-key-file", work </> defaultCommitteeVkeyFp n
      , "--cold-signing-key-file", work </> defaultCommitteeSkeyFp n
      ]

  committeeVkey1Fp <- H.noteShow $ work </> defaultCommitteeVkeyFp 1

  -- Read committee cold keys from disk to put into conway genesis

  comKeyHash1Str <- filter (/= '\n') <$> H.execCli' execConfigOffline
      [ anyEraToString cEra, "governance", "committee"
      , "key-hash"
      , "--verification-key-file", committeeVkey1Fp
      ]

  CommitteeColdKeyHash comKeyHash1 <-
    H.evalEither
      $ deserialiseFromRawBytesHex (AsHash AsCommitteeColdKey)
      $ BSC.pack comKeyHash1Str

  let comKeyCred1 = L.KeyHashObj comKeyHash1
      committeeThreshold = unsafeBoundedRational 0.5
      committee = L.Committee (Map.fromList [(comKeyCred1, EpochNo 100)]) committeeThreshold

  alonzoGenesis <- evalEither $ first prettyError defaultAlonzoGenesis
  (startTime, shelleyGenesis') <- getDefaultShelleyGenesis fastTestnetOptions
  let conwayGenesisWithCommittee =
        defaultConwayGenesis { L.cgCommittee = committee }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:_wallet1:_
    , configurationFile
    } <- cardanoTestnet
           fastTestnetOptions
           conf startTime shelleyGenesis'
           alonzoGenesis conwayGenesisWithCommittee

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath

  epochStateView <- getEpochStateView configurationFile (File socketPath)

  H.nothingFailM $ watchEpochStateView epochStateView (return . committeeIsPresent True) (EpochInterval 3)


  -- Step 2. Propose motion of no confidence. DRep and SPO voting thresholds must be met.

  -- Create proposal to add a new member to the committee

  proposalAnchorFile <- H.note $ work </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ eraToString era, "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]


  proposalFile <- H.note $ work </> "sample-proposal-anchor"
  stakeVkeyFp <- H.note $ work </> "stake.vkey"
  stakeSKeyFp <- H.note $ work </> "stake.skey"

  cliStakeAddressKeyGen
    $ KeyPair (File stakeVkeyFp) (File stakeSKeyFp)

  minActDeposit <- getMinGovActionDeposit epochStateView ceo

  void $ H.execCli' execConfig
    [ eraToString era, "governance", "action", "create-no-confidence"
    , "--testnet"
    , "--governance-action-deposit", show @Integer minActDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--out-file", proposalFile
    ]

  txbodyFp <- H.note $ work </> "tx.body"

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  void $ H.execCli' execConfig
    [ eraToString era, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_000
    , "--proposal-file", proposalFile
    , "--out-file", txbodyFp
    ]

  signedProposalTx <- signTx execConfig cEra work "signed-proposal"
                           (File txbodyFp) [SomeKeyPair $ paymentKeyInfoPair wallet0]

  submitTx execConfig cEra signedProposalTx

  -- Step 3. Create and submit votes on motion of no confidence proposal.
  -- Proposal was successfully submitted, now we vote on the proposal
  -- and confirm it was ratified.

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  governanceActionIndex <- H.nothingFailM $ watchEpochStateView epochStateView (return . maybeExtractGovernanceActionIndex (fromString governanceActionTxId)) (EpochInterval 10)

  let spoVotes :: [(String, Int)]
      spoVotes =  [("yes", 1), ("yes", 2), ("yes", 3)]
      drepVotes :: [(String, Int)]
      drepVotes = [("yes", 1), ("yes", 2), ("yes", 3)]

  spoVoteFiles <- SPO.generateVoteFiles ceo execConfig work "spo-vote-files"
                   governanceActionTxId governanceActionIndex
                   [(defaultSpoKeys idx, vote) | (vote, idx) <- spoVotes]
  drepVoteFiles <- DRep.generateVoteFiles execConfig work "drep-vote-files"
                    governanceActionTxId governanceActionIndex
                    [(defaultDRepKeyPair idx, vote) | (vote, idx) <- drepVotes]

  let allVoteFiles = spoVoteFiles ++ drepVoteFiles
  annotateShow allVoteFiles

  -- Submit votes
  voteTxBodyFp <- DRep.createVotingTxBody execConfig epochStateView sbe work "vote-tx-body"
                                     allVoteFiles wallet0
  let spoSigningKeys = [SomeKeyPair $ defaultSpoColdKeyPair n | (_, n) <- spoVotes]
      drepSigningKeys = [SomeKeyPair $ defaultDRepKeyPair n | (_, n) <- drepVotes]
      allVoteSigningKeys = spoSigningKeys ++ drepSigningKeys

  voteTxFp <- signTx execConfig cEra work "signed-vote-tx" voteTxBodyFp
                (SomeKeyPair (paymentKeyInfoPair wallet0) : allVoteSigningKeys)

  submitTx execConfig cEra voteTxFp

  -- Step 4. We confirm the no confidence motion has been ratified by checking
  -- for an empty constitutional committee.

  H.nothingFailM $ watchEpochStateView epochStateView (return . committeeIsPresent False) (EpochInterval 10)

-- | Checks if the committee is empty or not.
committeeIsPresent :: Bool -> AnyNewEpochState -> Maybe ()
committeeIsPresent committeeExists (AnyNewEpochState sbe newEpochState) =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "Constitutional committee does not exist pre-Conway era")
    (const $ let mCommittee = newEpochState
                                ^. L.nesEsL
                                 . L.esLStateL
                                 . L.lsUTxOStateL
                                 . L.utxosGovStateL
                                 . L.cgsCommitteeL
            in if committeeExists
               then if isSJust mCommittee
                    then Just () -- The committee is non empty and we terminate.
                    else Nothing
               else if mCommittee == SNothing
                    then Just ()  -- The committee is empty and we terminate.
                    else Nothing
    )
    sbe

