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
import           Cardano.Api.Ledger (EpochInterval (EpochInterval), KeyRole (Staking),
                   StandardCrypto)
import           Cardano.Api.ReexposeLedger (Coin, Credential)

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.State.Class
import           Data.Bifunctor (Bifunctor (..))
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import           GHC.Stack
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime
import           Testnet.Start.Types (eraToString)

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

hprop_ledger_events_treasury_withdrawal:: Property
hprop_ledger_events_treasury_withdrawal = H.integrationRetryWorkspace 1  "treasury-withdrawal" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      eraName = eraToString era
      cEra = AnyCardanoEra era

      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        , cardanoActiveSlotsCoeff = 0.3
        }

  testnetRuntime@TestnetRuntime
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

  startLedgerNewEpochStateLogging testnetRuntime tempAbsPath'

  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposal-anchor"
  treasuryWithdrawalActionFp <- H.note $ work </> gov </> "treasury-withdrawal.action"

  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ eraName, "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

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
    [ eraName, "stake-address", "registration-certificate"
    , "--stake-verification-key-file", stakeVkeyFp
    , "--key-reg-deposit-amt", show @Int 0 -- TODO: why this needs to be 0????
    , "--out-file", stakeCertFp
    ]

  stakeCertTxBodyFp <- H.note $ work </> "stake.registration.txbody"
  stakeCertTxSignedFp <- H.note $ work </> "stake.registration.tx"

  void $ H.execCli' execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 10_000_000
    , "--certificate-file", stakeCertFp
    , "--witness-override", show @Int 2
    , "--out-file", stakeCertTxBodyFp
    ]

  void $ H.execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", stakeCertTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet1
    , "--signing-key-file", stakeSKeyFp
    , "--out-file", stakeCertTxSignedFp
    ]

  void $ H.execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", stakeCertTxSignedFp
    ]
  -- }}}

  -- {{{ Create treasury withdrawal
  let withdrawalAmount = 3_300_777 :: Integer
  govActionDeposit <- getMinDRepDeposit execConfig ceo
  void $ H.execCli' execConfig
    [ eraName, "governance", "action", "create-treasury-withdrawal"
    , "--testnet"
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--governance-action-deposit", show govActionDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--transfer", show withdrawalAmount
    , "--funds-receiving-stake-verification-key-file", stakeVkeyFp
    , "--out-file", treasuryWithdrawalActionFp
    ]


  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  -- wait for an epoch before using wallet0 again
  void $ waitForEpochs execConfig (File configurationFile) (File socketPath) (EpochInterval 1)

  txin3 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  void $ H.execCli' execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin3
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 5_000_000
    , "--proposal-file", treasuryWithdrawalActionFp
    , "--out-file", txbodyFp
    ]

  void $ H.execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp
    ]

  void $ H.execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]
  -- }}}

  txidString <- mconcat . lines <$> H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]

  currentEpoch <- H.nothingFailM $ mEpoch <$> queryTip execConfig
  let terminationEpoch = succ . succ $ currentEpoch
  L.GovActionIx governanceActionIndex <- fmap L.gaidGovActionIx . H.nothingFailM $
    getTreasuryWithdrawalProposal (File configurationFile) (File socketPath) terminationEpoch

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  H.forConcurrently_ [1..3] $ \n -> do
    H.execCli' execConfig
      [ eraName, "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show governanceActionIndex
      , "--drep-verification-key-file", defaultDRepVkeyFp n
      , "--out-file", voteFp n
      ]

  txin4 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"
  -- {{{ Submit votes
  void $ H.execCli' execConfig
    [ eraName, "transaction", "build"
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
    [ eraName, "transaction", "sign"
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet1
    , "--signing-key-file", defaultDRepSkeyFp 1
    , "--signing-key-file", defaultDRepSkeyFp 2
    , "--signing-key-file", defaultDRepSkeyFp 3
    , "--out-file", voteTxFp
    ]

  void $ H.execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", voteTxFp
    ]
  -- }}}

  withdrawals <- H.nothingFailM $
    H.nothingFailM (mEpoch <$> queryTip execConfig) >>=
      getAnyWithdrawals (File configurationFile) (File socketPath) . (`L.addEpochInterval` EpochInterval 5)

  H.noteShow_ withdrawals
  (L.unCoin . snd <$> M.toList withdrawals) === [withdrawalAmount]


getAnyWithdrawals
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => NodeConfigFile In
  -> SocketPath
  -> EpochNo
  -> m (Maybe (Map (Credential Staking StandardCrypto) Coin))
getAnyWithdrawals nodeConfigFile socketPath maxEpoch = withFrozenCallStack $ do
  fmap snd . H.leftFailM . evalIO . runExceptT $ foldEpochState nodeConfigFile socketPath FullValidation maxEpoch Nothing
    $ \(AnyNewEpochState actualEra newEpochState) ->
      caseShelleyToBabbageOrConwayEraOnwards
        (error $ "Expected Conway era onwards, got state in " <> docToString (pretty actualEra))
        (\cEra _ _ -> conwayEraOnwardsConstraints cEra $ do
          let withdrawals = newEpochState
                ^. L.newEpochStateGovStateL
                . L.drepPulsingStateGovStateL
                . to L.extractDRepPulsingState
                . L.rsEnactStateL
                . L.ensWithdrawalsL
          if M.null withdrawals
            then pure ConditionNotMet
            else do
              put $ Just withdrawals
              pure ConditionMet
        ) actualEra


getTreasuryWithdrawalProposal
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => NodeConfigFile In
  -> SocketPath
  -> EpochNo -- ^ The termination epoch: the withdrawal proposal must be found *before* this epoch
  -> m (Maybe (L.GovActionId StandardCrypto))
getTreasuryWithdrawalProposal nodeConfigFile socketPath maxEpoch = withFrozenCallStack $ do
  fmap snd . H.leftFailM . evalIO . runExceptT $ foldEpochState nodeConfigFile socketPath QuickValidation maxEpoch Nothing
      $ \(AnyNewEpochState actualEra newEpochState) ->
        caseShelleyToBabbageOrConwayEraOnwards
          (error $ "Expected Conway era onwards, got state in " <> docToString (pretty actualEra))
          (\cEra _ _ -> conwayEraOnwardsConstraints cEra $ do
            let proposals = newEpochState
                      ^. L.newEpochStateGovStateL
                      . L.cgsProposalsL
                govActions = M.toList $ L.proposalsActionsMap proposals
            case map (second L.gasAction) govActions of
              (govActionId, L.TreasuryWithdrawals _ _): _ -> do
                put $ Just govActionId
                pure ConditionMet
              _ ->
                pure ConditionNotMet
          ) actualEra

