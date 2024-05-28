{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.InfoAction
  ( hprop_ledger_events_info_action
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Error (displayError)
import           Cardano.Api.Ledger (EpochInterval (EpochInterval))
import           Cardano.Api.Shelley

import           Cardano.Ledger.Conway.Governance (RatifyState (..))
import qualified Cardano.Ledger.Conway.Governance as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Bifunctor (first)
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String
import qualified Data.Text as Text
import           Data.Word
import           GHC.Stack
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/InfoAction/'@
hprop_ledger_events_info_action :: Property
hprop_ledger_events_info_action = integrationRetryWorkspace 0 "info-hash" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do


  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      era = toCardanoEra sbe
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 200
        , cardanoNodeEra = AnyCardanoEra era
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

  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposal-anchor"
  infoActionFp <- H.note $ work </> gov </> "info.action"

  H.writeFile proposalAnchorFile "dummy anchor data"

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

  -- Create info action proposal

  void $ execCli' execConfig
    [ "conway", "governance", "action", "create-info"
    , "--testnet"
    , "--governance-action-deposit", show @Int 1_000_000 -- TODO: Get this from the node
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--out-file", infoActionFp
    ]

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  H.noteM_ $ execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_000
    , "--proposal-file", infoActionFp
    , "--out-file", txbodyFp
    ]

  void $ execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet1
    , "--out-file", txbodySignedFp
    ]

  void $ execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]

  txidString <- mconcat . lines <$> execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]

  governanceActionIndex <-
    H.nothingFailM . fmap snd $ watchEpochStateUpdate epochStateView (EpochInterval 1) $ \(anyNewEpochState, _, _) -> do
      let r =  maybeExtractGovernanceActionIndex (fromString txidString) anyNewEpochState
      pure (if isJust r then ConditionMet else ConditionNotMet, r)

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  H.forConcurrently_ [1..3] $ \n -> do
    execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--drep-verification-key-file", verificationKeyFp $ defaultDRepKeyPair n
      , "--out-file", voteFp n
      ]

  -- We need more UTxOs
  txin3 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"

  -- Submit votes
  void $ execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin3
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 3_000_000
    , "--vote-file", voteFp 1
    , "--vote-file", voteFp 2
    , "--vote-file", voteFp 3
    , "--witness-override", show @Int 4
    , "--out-file", voteTxBodyFp
    ]


  void $ execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet0
    , "--signing-key-file", signingKeyFp $ defaultDRepKeyPair 1
    , "--signing-key-file", signingKeyFp $ defaultDRepKeyPair 2
    , "--signing-key-file", signingKeyFp $ defaultDRepKeyPair 3
    , "--out-file", voteTxFp
    ]

  void $ execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", voteTxFp
    ]

  -- We check that info action was succcessfully ratified
  !meInfoRatified
    <- H.timeout 120_000_000 $ runExceptT $ foldBlocks
                      configurationFile
                      socketPath
                      FullValidation
                      (InfoActionState False False)  -- Initial accumulator state
                      (foldBlocksCheckInfoAction (tempAbsPath' </> "events.log") governanceActionIndex )

  H.nothingFail meInfoRatified >>= \case
    Left e ->
      H.failMessage callStack
        $ "foldBlocksCheckInfoAction failed with: " <> displayError e
    Right _events -> success

-- | Fold accumulator for checking action state
data InfoActionState = InfoActionState
  { hasReceivedVotes :: !Bool
  , isRemoved :: !Bool
  } deriving Show

foldBlocksCheckInfoAction
  :: FilePath -- ^ Where to store debug logs
  -> Word32 -- ^ gov action index
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> InfoActionState -- ^ Accumulator at block i - 1
  -> IO (InfoActionState, FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksCheckInfoAction df govActionIdx _ ls allEvents _ acc = do
  -- for debugging purposes - makes investigation of a failure easier
  appendFile df "#### BLOCK\n"
  appendFile df $ show ls <> "\n"
  appendFile df $ unlines $ map show allEvents
  pure $ case foldl' checkRatification acc allEvents of
    ias@(InfoActionState True True) -> (ias, StopFold)
    ias -> (ias, ContinueFold)
  where
    checkRatification :: InfoActionState -> LedgerEvent -> InfoActionState
    checkRatification !ias = \case
      EpochBoundaryRatificationState (AnyRatificationState RatifyState{rsExpired}) -> do
        -- find out if the action was removed
        let w32indices = [i | L.GovActionIx i <- L.gaidGovActionIx <$> toList rsExpired ]
        ias{isRemoved = isRemoved ias || govActionIdx `elem` w32indices}
      NewGovernanceProposals _ (AnyProposals proposals) -> do
        -- find out if the action was voted for 3 times
        let actions =
              map snd
              $ filter ((== govActionIdx) . fst)
              $ map (first (\gai -> do
                  let (L.GovActionIx i) = L.gaidGovActionIx gai
                  i
                ))
              $ Map.assocs $ L.proposalsActionsMap proposals
        case actions of
          [L.GovActionState{L.gasDRepVotes=votes}]
            | length votes == 3 -> ias{hasReceivedVotes = True}
          _ -> ias
      _ -> ias

