{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Gov.ProposeNewConstitutionSPO
  ( hprop_ledger_events_propose_new_constitution_spo
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad.Trans.State.Strict (put)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import           Testnet.Process.Cli.DRep
import           Testnet.Process.Cli.Keys
import qualified Testnet.Process.Cli.SPO as SPO
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Test that SPO cannot vote on a new constitution
-- Execute me with:
-- @cabal test cardano-testnet-test --test-options '-p "/ProposeNewConstitutionSPO/"'@
hprop_ledger_events_propose_new_constitution_spo :: Property
hprop_ledger_events_propose_new_constitution_spo = integrationWorkspace "propose-new-constitution-spo" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) }
    <- mkConf tempAbsBasePath'
  let tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        , cardanoNodeEra = cEra
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:_
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
  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposal-anchor"
  constitutionFile <- H.note $ work </> gov </> "sample-constitution"
  constitutionActionFp <- H.note $ work </> gov </> "constitution.action"

  H.writeFile proposalAnchorFile "dummy anchor data"
  H.writeFile constitutionFile "dummy constitution data"
  constitutionHash <- execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", constitutionFile
    ]

  proposalAnchorDataHash <- execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  cliStakeAddressKeyGen
     $ KeyPair { verificationKey = File stakeVkeyFp
               , signingKey= File stakeSKeyFp
               }

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo

  -- Create constitution proposal
  H.noteM_ $ execCli' execConfig
    [ "conway", "governance", "action", "create-constitution"
    , "--testnet"
    , "--governance-action-deposit", show minDRepDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", "https://tinyurl.com/2pahcy6z"
    , "--constitution-hash", constitutionHash
    , "--out-file", constitutionActionFp
    ]

  txBodyFp <- H.note $ work </> "proposal-tx-body.body"

  txIn1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  H.noteM_ $ execCli' execConfig
    [ "conway", "transaction", "build"
    , "--tx-in", Text.unpack $ renderTxIn txIn1
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--proposal-file", constitutionActionFp
    , "--out-file", txBodyFp
    ]

  txBodySigned <- signTx execConfig cEra work "proposal-signed-tx" (File txBodyFp) [SomeKeyPair $ paymentKeyInfoPair wallet0]

  submitTx execConfig cEra txBodySigned

  txIdString <- retrieveTransactionId execConfig txBodySigned

  currentEpoch <- getCurrentEpochNo epochStateView

  -- Proposal should be there already, so don't wait a lot:
  let terminationEpoch = succ . succ $ currentEpoch

  mGovActionId <- getConstitutionProposal configurationFile socketPath terminationEpoch
  govActionId <- H.evalMaybe mGovActionId

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified

  let L.GovActionIx governanceActionIndex = L.gaidGovActionIx govActionId

  votes <- SPO.generateVoteFiles ceo execConfig work "vote-files" txIdString governanceActionIndex
             [(defaultSpoKeys n, "yes") | n <- [1..3]]

  -- Submit votes
  votesTxBody <- createVotingTxBody execConfig epochStateView sbe work "vote-tx-body" votes wallet0

  votesSignedTx <- signTx execConfig cEra work "vote-signed-tx"
                     votesTxBody (SomeKeyPair (paymentKeyInfoPair wallet0)
                                  :[SomeKeyPair $ defaultSpoColdKeyPair n | n <- [1..3]])

  -- Call should fail, because SPOs are unallowed to vote on the constitution
  failToSubmitTx execConfig cEra votesSignedTx "DisallowedVoters"

getConstitutionProposal
  :: (HasCallStack, MonadIO m, MonadTest m)
  => NodeConfigFile In
  -> SocketPath
  -> EpochNo -- ^ The termination epoch: the constitution proposal must be found *before* this epoch
  -> m (Maybe (L.GovActionId StandardCrypto))
getConstitutionProposal nodeConfigFile socketPath maxEpoch = do
  result <- H.evalIO . runExceptT $ foldEpochState nodeConfigFile socketPath QuickValidation maxEpoch Nothing
      $ \(AnyNewEpochState actualEra newEpochState) _slotNb _blockNb ->
        caseShelleyToBabbageOrConwayEraOnwards
          (error $ "Expected Conway era onwards, got state in " <> docToString (pretty actualEra))
          (\cEra -> conwayEraOnwardsConstraints cEra $ do
            let proposals = newEpochState
                      ^. L.nesEsL
                      . L.esLStateL
                      . L.lsUTxOStateL
                      . L.utxosGovStateL
                      . L.cgsProposalsL
                govActions = Map.toList $ L.proposalsActionsMap proposals
            case map (second L.gasAction) govActions of
              (govActionId, L.NewConstitution _ _) : _ -> do
                put $ Just govActionId
                pure ConditionMet
              _ ->
                pure ConditionNotMet
          ) actualEra
  (_, mGovAction) <- H.evalEither result
  return mGovAction
