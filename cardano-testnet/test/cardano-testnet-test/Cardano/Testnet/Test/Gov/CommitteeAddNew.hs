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
import           Cardano.Api.Experimental (Some (..))
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (ShelleyLedgerEra)

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import qualified Data.Char as C
import           Data.Default.Class
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
import           Testnet.Defaults
import           Testnet.EpochStateProcessing (waitForGovActionVotes)
import qualified Testnet.Process.Cli.DRep as DRep
import           Testnet.Process.Cli.Keys
import qualified Testnet.Process.Cli.SPO as SPO
import           Testnet.Process.Cli.SPO (createStakeKeyRegistrationCertificate)
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types (GenesisOptions (..), cardanoNumPools)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Committee Add New/"'@
hprop_constitutional_committee_add_new :: Property
hprop_constitutional_committee_add_new = integrationWorkspace "constitutional-committee-add-new" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- how many votes to cast
  let drepVotes, spoVotes :: [(String, Int)]
      drepVotes = mkVotes [(5, "yes"), (3, "no"), (2, "abstain")]
      spoVotes = mkVotes [(nSpos, "yes")]
      -- replicate votes requested number of times
      mkVotes :: [(Int, String)] -- ^ [(count, vote)]
              -> [(String, Int)] -- ^ [(vote, ordering number)]
      mkVotes votes = zip (concatMap (uncurry replicate) votes) [1..]
      nDrepVotes = length drepVotes
      nSpos = fromIntegral $ cardanoNumPools fastTestnetOptions
      ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      eraName = eraToString era
      fastTestnetOptions = def
        { cardanoNodeEra = AnyShelleyBasedEra sbe
        , cardanoNumDReps = fromIntegral nDrepVotes
        }
      shelleyOptions = def { genesisEpochLength = 200 }
  H.annotateShow drepVotes
  H.noteShow_ nDrepVotes

  runtime@TestnetRuntime
    { testnetMagic
    , wallets=wallet0:wallet1:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf

  node@TestnetNode{poolKeys=Just poolKeys} <- H.headM . filter isTestnetNodeSpo $ testnetNodes runtime
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFp <- H.note $ gov </> "sample-proposal-anchor"
  proposalDataFp <- H.note $ gov </> "sample-proposal-data"
  updateCommitteeFp <- H.note $ gov </> "update-cc.action"

  H.writeFile proposalAnchorFp $
    unlines [ "These are the reasons:  " , "" , "1. First" , "2. Second " , "3. Third" ]
  H.writeFile proposalDataFp "dummy proposal data"

  proposalAnchorDataHash <- execCli' execConfig
    [ "hash", "anchor-data"
    , "--file-text", proposalAnchorFp
    ]

  let ccColdSKeyFp n = gov </> "cc-" <> show n <> "-cold.skey"
      ccColdVKeyFp n = gov </> "cc-" <> show n <> "-cold.vkey"
      stakeCertFp = gov </> "stake.regcert"
      stakeKeys =  KeyPair { verificationKey = File $ gov </> "stake.vkey"
                           , signingKey = File $ gov </> "stake.skey"
                           }

  -- Register new stake address
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
    , "--deposit-return-stake-verification-key-file", verificationKeyFp stakeKeys
    , "--threshold", "0.2"
    , "--out-file", updateCommitteeFp
    ]
    <> concatMap
         (\fp -> ["--add-cc-cold-verification-key-file", fp, "--epoch", show ccExpiryEpoch])
         ccColdKeyFps

  txbodyFp <- H.note $ work </> "tx.body"
  txin1' <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  void $ execCli' execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1'
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_000
    , "--proposal-file", updateCommitteeFp
    , "--out-file", txbodyFp
    ]

  -- double check that we're starting with an empty committee
  committeeMembers <- getCommitteeMembers epochStateView ceo
  committeeMembers `H.assertWith` null

  signedProposalTx <-
    signTx execConfig cEra work "signed-proposal" (File txbodyFp) [Some $ paymentKeyInfoPair wallet0]
  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- H.noteM $ retrieveTransactionId execConfig signedProposalTx

  governanceActionIx <-
    H.nothingFailM . watchEpochStateUpdate epochStateView (L.EpochInterval 1) $ \(anyNewEpochState, _, _) ->
      pure $ maybeExtractGovernanceActionIndex (fromString governanceActionTxId) anyNewEpochState

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
      signingKeys = Some <$> paymentKeyInfoPair wallet0:poolNodePaymentKeyPair:drepSKeys
  voteTxFp <- signTx
    execConfig cEra gov "signed-vote-tx" voteTxBodyFp signingKeys

  submitTx execConfig cEra voteTxFp

  waitForGovActionVotes epochStateView (L.EpochInterval 1)

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

  H.nothingFailM $ watchEpochStateUpdate epochStateView (L.EpochInterval 1) (return . committeeIsPresent)

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

committeeIsPresent :: (AnyNewEpochState, SlotNo, BlockNo) -> Maybe ()
committeeIsPresent (AnyNewEpochState sbe newEpochState, _, _) =
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
