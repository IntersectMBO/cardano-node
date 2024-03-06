{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.TreasuryWithdrawal
  ( hprop_ledger_events_treasury_withdrawal
  ) where

import           Cardano.Api
import           Cardano.Api.Error
import           Cardano.Api.Ledger (StandardCrypto)
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.State.Class
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack, callStack)
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Query
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

hprop_ledger_events_treasury_withdrawal:: Property
hprop_ledger_events_treasury_withdrawal = H.integrationRetryWorkspace 0 {- FIXME set to 2 -} "treasury-withdrawal" $ \tempAbsBasePath' -> do
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        }
      -- TODO: Get this from protocol params
      govActionDeposit = 1_000_000 :: Int

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:_
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

  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposal-anchor"
  treasuryWithdrawalActionFp <- H.note $ work </> gov </> "treasury-withdrawal.action"

  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  let drepVkeyFp :: Int -> FilePath
      drepVkeyFp n = gov </> "drep-keys" <>"drep" <> show n <> ".vkey"

      drepSKeyFp :: Int -> FilePath
      drepSKeyFp n = gov </> "drep-keys" <>"drep" <> show n <> ".skey"

  -- {{{ Create DReps
  -- TODO: Refactor with shelleyKeyGen
  H.forConcurrently_ [1..3] $ \n -> do
    H.execCli' execConfig
      [ "conway", "governance", "drep", "key-gen"
      , "--verification-key-file", drepVkeyFp n
      , "--signing-key-file", drepSKeyFp n
      ]

  -- Create Drep registration certificates
  let drepCertFile :: Int -> FilePath
      drepCertFile n = gov </> "drep-keys" <>"drep" <> show n <> ".regcert"
  H.forConcurrently_ [1..3] $ \n -> do
    H.execCli' execConfig
       [ "conway", "governance", "drep", "registration-certificate"
       , "--drep-verification-key-file", drepVkeyFp n
       , "--key-reg-deposit-amt", show govActionDeposit
       , "--out-file", drepCertFile n
       ]


  -- Retrieve UTxOs for registration submission
  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  drepRegTxbodyFp <- H.note $ work </> "drep.registration.txbody"
  drepRegTxSignedFp <- H.note $ work </> "drep.registration.tx"

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 15_000_000
    , "--certificate-file", drepCertFile 1
    , "--certificate-file", drepCertFile 2
    , "--certificate-file", drepCertFile 3
    , "--witness-override", show @Int 4
    , "--out-file", drepRegTxbodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", drepRegTxbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", drepRegTxSignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", drepRegTxSignedFp
    ]
  -- }}}

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  -- {{{ Register stake address
  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"
      stakeCertFp = gov </> "stake.regcert"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  void $ H.execCli' execConfig
    [ "conway", "stake-address", "registration-certificate"
    , "--stake-verification-key-file", stakeVkeyFp
    , "--key-reg-deposit-amt", show @Int 0 -- TODO: why this needs to be 0????
    , "--out-file", stakeCertFp
    ]

  -- let trim xs = dropSpaceTail "" $ dropWhile isSpace xs
  --
  --     dropSpaceTail _ "" = ""
  --     dropSpaceTail maybeStuff (x:xs)
  --             | isSpace x = dropSpaceTail (x:maybeStuff) xs
  --             | null maybeStuff = x : dropSpaceTail "" xs
  --             | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
  --
  -- stakeAddress <- fmap trim $ H.noteM $ H.execCli' execConfig
  --   [ "conway", "stake-address", "build"
  --   , "--stake-verification-key-file", stakeVkeyFp
  --   ]
  --
  -- H.noteM_ $ H.execCli' execConfig
  --   [ "conway", "query", "stake-address-info"
  --   , "--address", stakeAddress
  --   , "--cardano-mode"
  --   ]

  stakeCertTxBodyFp <- H.note $ work </> "stake.registration.txbody"
  stakeCertTxSignedFp <- H.note $ work </> "stake.registration.tx"

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 10_000_000
    , "--certificate-file", stakeCertFp
    , "--witness-override", show @Int 2
    , "--out-file", stakeCertTxBodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", stakeCertTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet1
    , "--signing-key-file", stakeSKeyFp
    , "--out-file", stakeCertTxSignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", stakeCertTxSignedFp
    ]
  -- }}}

  -- {{{ Create treasury withdrawal
  void $ H.execCli' execConfig
    [ "conway", "governance", "action", "create-treasury-withdrawal"
    , "--testnet"
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--governance-action-deposit", show govActionDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--transfer", show @Int 3_300_777
    , "--funds-receiving-stake-verification-key-file", stakeVkeyFp
    , "--out-file", treasuryWithdrawalActionFp
    ]


  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  txin3 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin3
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 5_000_000
    , "--proposal-file", treasuryWithdrawalActionFp
    , "--out-file", txbodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--whole-utxo"
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]
  -- }}}

  txidString <- mconcat . lines <$> H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]

  QueryTipLocalStateOutput{mEpoch} <- queryTip
  currentEpoch <- H.nothingFail mEpoch
  -- Proposal should be there already, so don't wait a lot:
  let terminationEpoch = succ . succ $ currentEpoch
  L.GovActionIx governanceActionIndex <- fmap L.gaidGovActionIx . H.nothingFailM $ getTreasuryWithdrawalProposal (File configurationFile) (File socketPath) terminationEpoch

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  H.forConcurrently_ [1..3] $ \n -> do
    H.execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show governanceActionIndex
      , "--drep-verification-key-file", drepVkeyFp n
      , "--out-file", voteFp n
      ]

  txin4 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"
  -- {{{ Submit votes
  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin4
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 3_000_000
    , "--vote-file", voteFp 1
    , "--vote-file", voteFp 2
    , "--vote-file", voteFp 3
    , "--witness-override", show @Int 4
    , "--out-file", voteTxBodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet1
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", voteTxFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", voteTxFp
    ]
  -- }}}

  -- H.noteM_ $ H.execCli' execConfig
  --   [ "conway", "query", "gov-state"
  --   ]
  --
  -- ledgerStateFp <- H.note $ work </> "ledgerstate.cbor"
  -- H.noteM_ $ H.execCli' execConfig
  --   [ "query", "ledger-state"
  --   , "--out-file", ledgerStateFp
  --   ]
  --
  -- H.noteM_ $ H.execCli' execConfig
  --   [ "query", "ledger-state"
  --   -- , "--out-file", ledgerStateFp
  --   ]

  --
  -- v' <- H.leftFailM . H.readJsonFile $ ledgerStateFp
  --
  -- H.note_ . show $ v' ^? A.key "stateBefore" . A.key "esAccountState"

  -- H.noteM_ $ H.execCli' execConfig
  --   [ "conway", "query", "committee-state"
  --   ]
  --
  --

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--whole-utxo"
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "stake-pools"
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "tip"
    ]

  !meTreasuryWithdrawed
    <- H.timeout 35_000_000
        $ runExceptT $ foldBlocks
            (File configurationFile)
            (File socketPath)
            FullValidation
            [] -- Initial accumulator state
            (foldBlocksCheckTreasuryWithdrawalWasRatified $ tempAbsPath' </> "events.log")

  H.noteM_ $ H.execCli' execConfig
    [ "query", "stake-snapshot"
    , "--all-stake-pools"
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "gov-state"
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "drep-stake-distribution", "--all-dreps"
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--whole-utxo"
    ]

  -- H.noteM_ $ H.execCli' execConfig
  --   [ "conway", "query", "stake-address-info"
  --   , "--address", stakeAddress
  --   , "--cardano-mode"
  --   ]
  --
  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "stake-pools"
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "tip"
    ]

  eTreasuryWithdrawed <- H.nothingFail meTreasuryWithdrawed

  case eTreasuryWithdrawed of
    Left e ->
      H.failMessage callStack
        $ "foldBlocksCheckTreasuryWithdrawalWasRatified failed with: " <> displayError e
    Right _events -> success

  pure ()


foldBlocksCheckTreasuryWithdrawalWasRatified
  :: FilePath
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> [LedgerEvent] -- ^ Accumulator at block i - 1
  -> IO ([LedgerEvent], FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksCheckTreasuryWithdrawalWasRatified df _ ls allEvents _ _ = do
  appendFile df "#### BLOCK\n"
  appendFile df $ show ls <> "\n"
  appendFile df $ unlines $ map show allEvents
  if any filterRatificationState allEvents
  then return (allEvents , StopFold)
  else return ([], ContinueFold)

filterRatificationState
  :: LedgerEvent
  -> Bool
filterRatificationState (EpochBoundaryRatificationState (AnyRatificationState rState)) = do
  let withdrawals = rState ^. Ledger.rsEnactStateL . Ledger.ensWithdrawalsL
  not $ M.null withdrawals
filterRatificationState _ = False


getTreasuryWithdrawalProposal
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => NodeConfigFile In
  -> SocketPath
  -> EpochNo -- ^ The termination epoch: the constitution proposal must be found *before* this epoch
  -> m (Maybe (L.GovActionId StandardCrypto))
getTreasuryWithdrawalProposal nodeConfigFile socketPath maxEpoch = do
  fmap snd . H.leftFailM . runExceptT $ foldEpochState nodeConfigFile socketPath QuickValidation maxEpoch Nothing
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
              (govActionId, L.TreasuryWithdrawals _ _) : _ -> do
                put $ Just govActionId
                pure ConditionMet
              _ ->
                pure ConditionNotMet
          ) actualEra

