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

module Cardano.Testnet.Test.Gov.TreasuryWithdrawal
  ( hprop_ledger_events_treasury_withdrawal
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (EpochInterval (EpochInterval), KeyRole (Staking),
                   StandardCrypto)
import           Cardano.Api.ReexposeLedger (Coin, Credential)

import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.State.Class
import           Data.Bifunctor (Bifunctor (..))
import           Data.Default.Class
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import           GHC.Stack
import           Lens.Micro
import           System.Directory (makeAbsolute)
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Hash (serveFilesWhile)
import           Testnet.Components.Query
import           Testnet.Defaults
import           Testnet.Process.Cli.Keys (cliStakeAddressKeyGen)
import           Testnet.Process.Cli.SPO (createStakeKeyRegistrationCertificate)
import           Testnet.Process.Run (addEnvVarsToConfig, execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog.Extras as H

hprop_ledger_events_treasury_withdrawal:: Property
hprop_ledger_events_treasury_withdrawal = integrationRetryWorkspace 2  "treasury-withdrawal" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      era = toCardanoEra sbe
      eraName = eraToString era

      fastTestnetOptions = def { cardanoNodeEra = AnyShelleyBasedEra sbe }
      shelleyOptions = def { genesisEpochLength = 200
                           , genesisActiveSlotsCoeff = 0.3
                           }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets=wallet0:wallet1:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf

  node@TestnetNode{nodeSprocket} <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow nodeSprocket
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  let proposalAnchorDataIpfsHash = "QmexFJuEn5RtnHEqpxDcqrazdHPzAwe7zs2RxHLfMH5gBz"
  proposalAnchorFile <- H.noteM $ liftIO $ makeAbsolute $ "test" </> "cardano-testnet-test" </> "files" </> "sample-proposal-anchor"

  treasuryWithdrawalActionFp <- H.note $ work </> gov </> "treasury-withdrawal.action"

  proposalAnchorDataHash <- execCli' execConfig
    [ "hash", "anchor-data", "--file-binary", proposalAnchorFile
    ]

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  -- {{{ Register stake address
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

  void $ execCli' execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin2
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
  -- }}}

  -- {{{ Create treasury withdrawal
  let withdrawalAmount = 3_300_777 :: Integer
  govActionDeposit <- getMinDRepDeposit epochStateView ceo

  let relativeUrl = ["ipfs", proposalAnchorDataIpfsHash]

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"


  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, proposalAnchorFile)]
    ( \port -> do
      let execConfig' = addEnvVarsToConfig execConfig [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
      void $ execCli' execConfig'
        [ eraName, "governance", "action", "create-treasury-withdrawal"
        , "--testnet"
        , "--anchor-url", "ipfs://" ++ proposalAnchorDataIpfsHash
        , "--anchor-data-hash", proposalAnchorDataHash
        , "--governance-action-deposit", show govActionDeposit
        , "--deposit-return-stake-verification-key-file", verificationKeyFp stakeKeys
        , "--transfer", show withdrawalAmount
        , "--funds-receiving-stake-verification-key-file", verificationKeyFp stakeKeys
        , "--out-file", treasuryWithdrawalActionFp
        ]

      -- wait for one block before using wallet0 again
      _ <- waitForBlocks epochStateView 1

      txin3 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

      void $ execCli' execConfig'
        [ eraName, "transaction", "build"
        , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
        , "--tx-in", Text.unpack $ renderTxIn txin3
        , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 5_000_000
        , "--proposal-file", treasuryWithdrawalActionFp
        , "--out-file", txbodyFp
        ]
    )

  void $ execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]
-- }}}

  txidString <- mconcat . lines <$> execCli' execConfig
    [ "latest", "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]

  currentEpoch <- getCurrentEpochNo epochStateView
  let terminationEpoch = succ . succ $ currentEpoch
  L.GovActionIx governanceActionIndex <- fmap L.gaidGovActionIx . H.nothingFailM $
    getTreasuryWithdrawalProposal configurationFile socketPath terminationEpoch

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  H.forConcurrently_ [1..3] $ \n -> do
    execCli' execConfig
      [ eraName, "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show governanceActionIndex
      , "--drep-verification-key-file", verificationKeyFp $ defaultDRepKeyPair n
      , "--out-file", voteFp n
      ]

  txin4 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"
  -- {{{ Submit votes
  void $ execCli' execConfig
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

  void $ execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet1
    , "--signing-key-file", signingKeyFp $ defaultDRepKeyPair 1
    , "--signing-key-file", signingKeyFp $ defaultDRepKeyPair 2
    , "--signing-key-file", signingKeyFp $ defaultDRepKeyPair 3
    , "--out-file", voteTxFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", voteTxFp
    ]
  -- }}}

  withdrawals <- H.nothingFailM $
    getCurrentEpochNo epochStateView >>=
      getAnyWithdrawals configurationFile socketPath . (`L.addEpochInterval` EpochInterval 5)

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
    $ \(AnyNewEpochState actualEra newEpochState _) ->
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
      $ \(AnyNewEpochState actualEra newEpochState _) ->
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
