{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.CommitteeAddNew
  ( hprop_constitutional_committee_add_new
  ) where

import           Cardano.Api as Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (ShelleyLedgerEra)

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import qualified Data.Char as C
import qualified Data.Map as Map
import           Data.Maybe.Strict
import           Data.Set (Set)
import           Data.String
import qualified Data.Text as Text
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import           Testnet.EpochStateProcessing (waitForGovActionVotes)
import qualified Testnet.Process.Cli.DRep as DRep
import           Testnet.Process.Cli.Keys
import qualified Testnet.Process.Cli.SPO as SPO
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog.Extras as H

hprop_constitutional_committee_add_new :: Property
hprop_constitutional_committee_add_new = integrationWorkspace "constitutional-committee-add-new" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- how many votes to cast
  let drepVotes, spoVotes :: [(String, Int)]
      drepVotes = zip (concatMap (uncurry replicate) [(5, "yes"), (3, "no"), (2, "abstain")]) [1..]
      spoVotes = zip (concatMap (uncurry replicate) [(1, "yes")]) [1..]
  H.noteShow_ drepVotes

  let nDrepVotes :: Int
      nDrepVotes = length drepVotes
  H.noteShow_ nDrepVotes

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      eraName = eraToString era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 200
        , cardanoNodeEra = cEra
        , cardanoNumDReps = nDrepVotes
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  PoolNode{poolRuntime, poolKeys} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFp <- H.note $ gov </> "sample-proposal-anchor"
  proposalDataFp <- H.note $ gov </> "sample-proposal-data"
  updateCommitteeFp <- H.note $ gov </> "update-cc.action"

  H.writeFile proposalAnchorFp "dummy anchor data"
  H.writeFile proposalDataFp "dummy proposal data"

  proposalAnchorDataHash <- execCli' execConfig
    [ eraName, "governance" , "hash", "anchor-data"
    , "--file-text", proposalAnchorFp
    ]

  let ccColdSKeyFp n = gov </> "cc-" <> show n <> "-cold.skey"
      ccColdVKeyFp n = gov </> "cc-" <> show n <> "-cold.vkey"
      stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  cliStakeAddressKeyGen
    $ KeyPair { verificationKey = File stakeVkeyFp
              , signingKey = File stakeSKeyFp
              }

  minGovActDeposit <- getMinGovActionDeposit epochStateView ceo

  ccColdKeys <- H.noteShowM $
    H.forConcurrently [1..3] $ \(i :: Int) -> do
      let coldVKey = ccColdVKeyFp i
      _ <- execCli' execConfig
        [ eraName, "governance", "committee", "key-gen-cold"
        , "--cold-verification-key-file", ccColdVKeyFp i
        , "--cold-signing-key-file", ccColdSKeyFp i
        ]
      fmap (coldVKey, i,) $
        parseKeyHashCred =<< execCli' execConfig
          [ eraName, "governance", "committee", "key-hash"
          , "--verification-key-file", ccColdVKeyFp i
          ]
  let (ccColdKeyFps, _, ccCredentials) = unzip3 ccColdKeys

  EpochNo epochNo <- H.noteShowM $ getCurrentEpochNo epochStateView
  let ccExpiryEpoch = epochNo + 200

  _ <- execCli' execConfig $
    [ eraName, "governance", "action" , "update-committee"
    , "--testnet"
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--governance-action-deposit", show minGovActDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--threshold", "0.2"
    , "--out-file", updateCommitteeFp
    ]
    <> concatMap
         (\fp -> ["--add-cc-cold-verification-key-file", fp, "--epoch", show ccExpiryEpoch])
         ccColdKeyFps

  txbodyFp <- H.note $ work </> "tx.body"
  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  void $ execCli' execConfig
    [ eraToString era, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_000
    , "--proposal-file", updateCommitteeFp
    , "--out-file", txbodyFp
    ]

  -- double check that we're starting with an empty committee
  committeeMembers <- getCommitteeMembers epochStateView ceo
  committeeMembers `H.assertWith` null

  signedProposalTx <-
    signTx execConfig cEra work "signed-proposal" (File txbodyFp) [SomeKeyPair $ paymentKeyInfoPair wallet0]
  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- H.noteM $ retrieveTransactionId execConfig signedProposalTx

  governanceActionIx <- H.nothingFailM $ watchEpochStateView epochStateView (return . maybeExtractGovernanceActionIndex (fromString governanceActionTxId)) (L.EpochInterval 1)

  dRepVoteFiles <-
    DRep.generateVoteFiles
      execConfig work "vote-files" governanceActionTxId governanceActionIx
        [(defaultDRepKeyPair idx, vote) | (vote, idx) <- drepVotes]

  spoVoteFiles <-
    SPO.generateVoteFiles
      ceo execConfig work "vote-files" governanceActionTxId governanceActionIx
        [(poolKeys, vote) | (vote, _idx) <- spoVotes]

  let voteFiles = dRepVoteFiles <> spoVoteFiles

  voteTxBodyFp <-
    DRep.createVotingTxBody
      execConfig epochStateView sbe work "vote-tx-body" voteFiles wallet0

  -- FIXME: remove dependence of signTx on PaymentKeyPair
  let poolNodePaymentKeyPair = KeyPair
        { signingKey = File . signingKeyFp $ poolNodeKeysCold poolKeys
        , verificationKey = error "unused"
        }
      drepSKeys = map (defaultDRepKeyPair . snd) drepVotes
      signingKeys = SomeKeyPair <$> paymentKeyInfoPair wallet0:poolNodePaymentKeyPair:drepSKeys
  voteTxFp <- signTx
    execConfig cEra gov "signed-vote-tx" voteTxBodyFp signingKeys

  submitTx execConfig cEra voteTxFp

  waitForGovActionVotes epochStateView ceo (L.EpochInterval 1)

  govState <- getGovState epochStateView ceo
  govActionState <- H.headM $ govState ^. L.cgsProposalsL . L.pPropsL . to toList
  let gaDRepVotes = govActionState ^. L.gasDRepVotesL . to toList
      gaSpoVotes = govActionState ^. L.gasStakePoolVotesL . to toList

  length (filter ((== L.VoteYes) . snd) gaDRepVotes) === 5
  length (filter ((== L.VoteNo) . snd) gaDRepVotes) === 3
  length (filter ((== L.Abstain) . snd) gaDRepVotes) === 2
  length drepVotes === length gaDRepVotes
  length (filter ((== L.VoteYes) . snd) gaSpoVotes) === 1
  length spoVotes === length gaSpoVotes

  H.nothingFailM $ watchEpochStateView epochStateView (return . committeeIsPresent) (L.EpochInterval 1)

  -- show proposed committe meembers
  H.noteShow_ ccCredentials

  newCommitteeMembers :: Set (L.Credential L.ColdCommitteeRole L.StandardCrypto)
    <- fromList <$> getCommitteeMembers epochStateView ceo

  -- check that the committee is actually what we expect
  newCommitteeMembers === fromList ccCredentials

parseKeyHashCred :: MonadFail m => String -> m (L.Credential kr L.StandardCrypto)
parseKeyHashCred hash = L.parseCredential $ "keyHash-" <> Text.pack (trim hash)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile C.isSpace

getCommitteeMembers
  :: HasCallStack
  => H.MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ConwayEraOnwards era
  -> m [L.Credential L.ColdCommitteeRole (L.EraCrypto (ShelleyLedgerEra era))]
getCommitteeMembers epochStateView ceo = withFrozenCallStack $ do
  govState <- getGovState epochStateView ceo
  fmap (Map.keys . L.committeeMembers) . H.nothingFail $ strictMaybeToMaybe $ govState ^. L.cgsCommitteeL

committeeIsPresent :: AnyNewEpochState -> Maybe ()
committeeIsPresent (AnyNewEpochState sbe newEpochState) =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "Constitutional committee does not exist pre-Conway era")
    (\_ -> do
      let mCommittee = newEpochState
                       ^. L.nesEsL
                       . L.esLStateL
                       . L.lsUTxOStateL
                       . L.utxosGovStateL
                       . L.cgsCommitteeL
      members <- L.committeeMembers <$> strictMaybeToMaybe mCommittee
      when (Map.null members) Nothing
    )
    sbe
