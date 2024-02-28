{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{- HLINT ignore "Use head" -}

module Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitutionSPO
  ( hprop_ledger_events_propose_new_constitution_spo
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api
import           Cardano.Api.Ledger

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput (QueryTipLocalStateOutput),
                   mEpoch)
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad.Trans.State.Strict (put)
import           Data.Bifunctor (Bifunctor (..))
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Word
import           GHC.Stack (HasCallStack)
import           Lens.Micro
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath ((</>))

import           Testnet.Components.Query
import qualified Testnet.Process.Cli as P
import           Testnet.Process.Cli (execCliStdoutToJson)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

-- | Test that SPO cannot vote on a new constitution
-- Execute me with:
-- @cabal test cardano-testnet-test --test-options '-p "/ProposeNewConstitutionSPO/"'@
hprop_ledger_events_propose_new_constitution_spo :: Property
hprop_ledger_events_propose_new_constitution_spo = H.integrationWorkspace "propose-new-constitution-spo" $ \tempAbsBasePath' -> do
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) }
    <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  let sbe = ShelleyBasedEraConway
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
    , wallets
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

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposal-anchor"
  constitutionFile <- H.note $ work </> gov </> "sample-constitution"
  constitutionActionFp <- H.note $ work </> gov </> "constitution.action"

  H.writeFile proposalAnchorFile "dummy anchor data"
  H.writeFile constitutionFile "dummy constitution data"
  constitutionHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", constitutionFile
    ]

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  let spoColdVkeyFp :: Int -> FilePath
      spoColdVkeyFp n = tempAbsPath' </> "pools-keys" </> "pool" <> show n </> "cold.vkey"

      spoColdSkeyFp :: Int -> FilePath
      spoColdSkeyFp n = tempAbsPath' </> "pools-keys" </> "pool" <> show n </> "cold.skey"

  -- Create constitution proposal
  H.noteM_ $ H.execCli' execConfig
    [ "conway", "governance", "action", "create-constitution"
    , "--testnet"
    , "--governance-action-deposit", show @Int 0 -- TODO: Get this from the node
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", "https://tinyurl.com/2pahcy6z"
    , "--constitution-hash", constitutionHash
    , "--out-file", constitutionActionFp
    ]

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe $ wallets !! 0

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
    , "--proposal-file", constitutionActionFp
    , "--out-file", txbodyFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 0
    , "--out-file", txbodySignedFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]

  txidString <- mconcat . lines <$> H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]

  QueryTipLocalStateOutput{mEpoch} <- execCliStdoutToJson execConfig [ "query", "tip" ]
  currentEpoch <- H.evalMaybe mEpoch
  -- Proposal should be there already, so don't wait a lot:
  let terminationEpoch = succ . succ $ currentEpoch

  mGovActionId <- getConstitutionProposal (Api.File configurationFile) (Api.File socketPath) terminationEpoch
  govActionId <- H.evalMaybe mGovActionId
  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified

  let L.GovActionIx governanceActionIndex = L.gaidGovActionIx govActionId

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  H.forConcurrently_ [1..3] $ \n -> do
    H.execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--cold-verification-key-file", spoColdVkeyFp n
      , "--out-file", voteFp n
      ]

  -- We need more UTxOs
  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe $ wallets !! 0

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"

  -- Submit votes
  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
    , "--vote-file", voteFp 1
    , "--vote-file", voteFp 2
    , "--vote-file", voteFp 3
    , "--witness-override", show @Int 4
    , "--out-file", voteTxBodyFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 0
    , "--signing-key-file", spoColdSkeyFp 1
    , "--signing-key-file", spoColdSkeyFp 2
    , "--signing-key-file", spoColdSkeyFp 3
    , "--out-file", voteTxFp
    ]

  -- Call should fail, because SPOs are unallowed to vote on the constitution
  (exitCode, _, stderr) <- H.execCliAny execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", voteTxFp
    ]

  exitCode H./== ExitSuccess -- Dit it fail?
  H.assert $ "DisallowedVoters" `isInfixOf` stderr -- Did it fail for the expected reason?

getConstitutionProposal
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => NodeConfigFile In
  -> SocketPath
  -> EpochNo -- ^ The termination epoch: the constitution proposal must be found *before* this epoch
  -> m (Maybe (L.GovActionId StandardCrypto))
getConstitutionProposal nodeConfigFile socketPath maxEpoch = do
  result <- runExceptT $ foldEpochState nodeConfigFile socketPath QuickValidation maxEpoch Nothing
      $ \(AnyNewEpochState actualEra newEpochState) ->
        caseShelleyToBabbageOrConwayEraOnwards
          (error $ "Expected Conway era onwards, got state in " <> docToString (pretty actualEra))
          (\cEra -> conwayEraOnwardsConstraints cEra $ do
            let proposals = newEpochState
                      ^. L.nesEsL
                      . L.esLStateL
                      . L.lsUTxOStateL
                      . L.utxosGovStateL
                      . L.cgProposalsL
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
