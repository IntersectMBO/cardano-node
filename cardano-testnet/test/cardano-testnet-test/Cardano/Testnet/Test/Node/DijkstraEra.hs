{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Node.DijkstraEra
  ( hprop_hardfork_to_dijkstra
  , hardForkToDijkstra
  , getProtocolMajorVersion
  , assertNodeInDijkstra
  ) where

import           Cardano.Api
import           Cardano.Api.Experimental (Some (..))
import           Cardano.Api.Ledger (EpochInterval (..))

import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet
import           Cardano.Testnet.Test.Utils (nodesProduceBlocks)

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default.Class (def)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as Text
import           Data.Word (Word16)
import           GHC.Stack
import           Lens.Micro
import           Numeric.Natural (Natural)
import           System.Directory (makeAbsolute)
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Hash (serveFilesWhile)
import           Testnet.Components.Query
import           Testnet.Defaults (defaultDRepKeyPair, defaultSpoColdKeyPair, defaultSpoKeys)
import           Testnet.Process.Cli.DRep (generateVoteFiles)
import           Testnet.Process.Cli.Keys (cliStakeAddressKeyGen)
import qualified Testnet.Process.Cli.SPO as SPO
import           Testnet.Process.Cli.SPO (createStakeKeyRegistrationCertificate)
import           Testnet.Process.Cli.Transaction (retrieveTransactionId, signTx, submitTx)
import           Testnet.Process.Run (addEnvVarsToConfig, execCli', mkExecConfig)
import           Testnet.Process.RunIO (liftIOAnnotated)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Hedgehog.Internal.Property (MonadTest)

-- | Starts a cluster in Conway at protocol version 10 and hard forks it into the
-- Dijkstra era through governance, the same way mainnet will:
--
-- 1. a @HardForkInitiation@ action to PV 11.0 (Conway intra-era hard fork),
--    voted for by the default DReps and SPO, ratified and enacted;
-- 2. a second @HardForkInitiation@ action to PV 12.0, chained to the first one,
--    voted, ratified and enacted;
--
-- and then checks that:
--
-- * @cardano-cli query tip@ reports the Dijkstra era over node-to-client,
-- * the ledger state served by the node is a Dijkstra 'NewEpochState',
-- * the SPO node keeps producing blocks and shuts down cleanly.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Hard fork to Dijkstra/"'@
hprop_hardfork_to_dijkstra :: Property
hprop_hardfork_to_dijkstra = integrationRetryWorkspace 2 "hardfork-to-dijkstra" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let sbe = ShelleyBasedEraConway
      creationOptions = def
        { creationEra = AnyShelleyBasedEra sbe
        , creationGenesisOptions = def
            { genesisEpochLength = 300
            , genesisActiveSlotsCoeff = 0.3
            }
        }

  runtime@TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets = wallet0:wallet1:_
    , configurationFile
    } <- createAndRunTestnet creationOptions def conf

  let node@TestnetNode{nodeSprocket} = NEL.head testnetNodes
      socketPath = nodeSocketPath node
  execConfig <- mkExecConfig tempBaseAbsPath nodeSprocket testnetMagic
  epochStateView <- getEpochStateView configurationFile socketPath

  hardForkToDijkstra execConfig epochStateView tempAbsPath work wallet0 wallet1

  assertNodeInDijkstra execConfig

  H.note_ "the ledger state served by the node is a Dijkstra NewEpochState"
  AnyNewEpochState sbe' _ _ <- getEpochState epochStateView
  shelleyBasedEraConstraints sbe' $
    AnyShelleyBasedEra sbe' H.=== AnyShelleyBasedEra ShelleyBasedEraDijkstra

  nodesProduceBlocks tempAbsBasePath' runtime

-- | Take a cluster that was started in Conway at protocol version 10 into the
-- Dijkstra era through governance: register a stake key to receive the action
-- deposits, then propose, vote and enact a hard fork to PV 11.0 (Conway
-- intra-era) followed by a chained hard fork to PV 12.0 (Dijkstra).
hardForkToDijkstra
  :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m, H.MonadAssertion m, H.MonadBaseControl IO m)
  => H.ExecConfig
  -> EpochStateView
  -> TmpAbsolutePath
  -> FilePath -- ^ Working directory
  -> PaymentKeyInfo -- ^ Wallet paying for the governance actions and votes
  -> PaymentKeyInfo -- ^ Wallet paying for the stake key registration
  -> m ()
hardForkToDijkstra execConfig epochStateView tempAbsPath work wallet0 wallet1 = withFrozenCallStack $ do
  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      eraName = eraToString $ toCardanoEra sbe

  H.note_ "the cluster starts in Conway at protocol version 10"
  startPv <- getProtocolMajorVersion epochStateView
  startPv H.=== 10

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- Register a stake address to receive the governance action deposits back.
  let stakeCertFp = gov </> "stake.regcert"
      stakeKeys = KeyPair { verificationKey = File $ gov </> "stake.vkey"
                          , signingKey = File $ gov </> "stake.skey"
                          }
  cliStakeAddressKeyGen stakeKeys
  keyDeposit <- getKeyDeposit epochStateView ceo
  createStakeKeyRegistrationCertificate
    tempAbsPath (AnyShelleyBasedEra sbe) (verificationKey stakeKeys) keyDeposit stakeCertFp

  stakeCertTxBodyFp <- H.note $ gov </> "stake.registration.txbody"
  stakeCertTxSignedFp <- H.note $ gov </> "stake.registration.tx"
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
  _ <- waitForBlocks epochStateView 1

  H.note_ "hard fork 10.0 -> 11.0 (Conway intra-era hard fork)"
  hf11 <- proposeAndVoteHardFork execConfig epochStateView ceo (gov </> "hardfork-11")
            Nothing 11 stakeKeys wallet0
  H.note_ "waiting for protocol major version 11"
  _ <- retryUntilM epochStateView (WaitForEpochs (EpochInterval 4))
         (getProtocolMajorVersion epochStateView) (== 11)

  H.note_ "hard fork 11.0 -> 12.0 (Conway -> Dijkstra)"
  _ <- proposeAndVoteHardFork execConfig epochStateView ceo (gov </> "hardfork-12")
         (Just hf11) 12 stakeKeys wallet0
  H.note_ "waiting for protocol major version 12"
  void $ retryUntilM epochStateView (WaitForEpochs (EpochInterval 4))
         (getProtocolMajorVersion epochStateView) (== 12)

-- | Submit a @HardForkInitiation@ governance action to the given major protocol
-- version (minor version 0) and vote @yes@ on it with the three default DReps and
-- the default SPO. Ratification happens at the next epoch boundary and enactment
-- at the one after; the caller waits for it. Returns the action's transaction id
-- and index so that the next hard fork action can be chained to it.
proposeAndVoteHardFork
  :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m, H.MonadAssertion m, H.MonadBaseControl IO m)
  => H.ExecConfig
  -> EpochStateView
  -> ConwayEraOnwards ConwayEra
  -> FilePath -- ^ Working directory for this hard fork's files
  -> Maybe (TxId, Word16) -- ^ Previous @HardForkInitiation@ action, if any
  -> Natural -- ^ Target major protocol version
  -> KeyPair StakeKey -- ^ Registered stake key that receives the deposit back
  -> PaymentKeyInfo -- ^ Wallet paying for the transactions
  -> m (TxId, Word16)
proposeAndVoteHardFork execConfig epochStateView ceo work prevAction targetMajor stakeKeys wallet = withFrozenCallStack $ do
  let sbe = convert ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      eraName = eraToString era

  baseDir <- H.createDirectoryIfMissing work

  let proposalAnchorDataIpfsHash = "QmexFJuEn5RtnHEqpxDcqrazdHPzAwe7zs2RxHLfMH5gBz"
  proposalAnchorFile <- H.noteM $ liftIOAnnotated $ makeAbsolute $
    "test" </> "cardano-testnet-test" </> "files" </> "sample-proposal-anchor"
  proposalAnchorDataHash <- execCli' execConfig
    [ "hash", "anchor-data", "--file-binary", proposalAnchorFile ]

  govActionDeposit <- getMinGovActionDeposit epochStateView ceo
  proposalFile <- H.note $ baseDir </> "hardfork.action"
  proposalBody <- H.note $ baseDir </> "proposal.txbody"

  -- The CLI verifies the anchor against its URL, so serve the anchor file from a
  -- temporary HTTP server standing in for an IPFS gateway.
  serveFilesWhile
    [(["ipfs", proposalAnchorDataIpfsHash], proposalAnchorFile)]
    ( \port -> do
        let execConfig' = addEnvVarsToConfig execConfig
              [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
        void $ execCli' execConfig' $
          [ eraName, "governance", "action", "create-hardfork"
          , "--testnet"
          , "--governance-action-deposit", show govActionDeposit
          , "--deposit-return-stake-verification-key-file", verificationKeyFp stakeKeys
          ] ++ concatMap (\(prevTxId, prevIx) ->
                            [ "--prev-governance-action-tx-id", prettyShow prevTxId
                            , "--prev-governance-action-index", show prevIx
                            ]) prevAction ++
          [ "--anchor-url", "ipfs://" ++ proposalAnchorDataIpfsHash
          , "--anchor-data-hash", proposalAnchorDataHash
          , "--check-anchor-data"
          , "--protocol-major-version", show targetMajor
          , "--protocol-minor-version", "0"
          , "--out-file", proposalFile
          ]

        -- `transaction build` re-verifies the proposal's anchor, so it must also run
        -- while the gateway stand-in is up.
        txIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet
        void $ execCli' execConfig'
          [ eraName, "transaction", "build"
          , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
          , "--tx-in", Text.unpack $ renderTxIn txIn
          , "--proposal-file", proposalFile
          , "--out-file", proposalBody
          ]
    )
  signedProposalTx <- signTx execConfig cEra baseDir "signed-proposal"
                        (File proposalBody) [Some $ paymentKeyInfoPair wallet]
  submitTx execConfig cEra signedProposalTx
  govActionTxId <- H.noteShowM $ retrieveTransactionId execConfig signedProposalTx

  govActionIndex <- retryUntilJustM epochStateView (WaitForEpochs (EpochInterval 2)) $
    maybeExtractGovernanceActionIndex govActionTxId <$> getEpochState epochStateView

  -- Hard fork initiation needs both the DRep and the SPO thresholds to be met.
  drepVotes <- generateVoteFiles execConfig baseDir "drep-votes" govActionTxId govActionIndex
                 [ (defaultDRepKeyPair n, "yes") | n <- [1..3] ]
  spoVotes <- SPO.generateVoteFiles ceo execConfig baseDir "spo-votes" govActionTxId govActionIndex
                [ (defaultSpoKeys 1, "yes") ]
  let voteFiles = drepVotes ++ spoVotes

  voteTxBodyFp <- H.note $ baseDir </> "votes.txbody"
  voteTxIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet
  void $ execCli' execConfig $
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn voteTxIn
    ] ++ concat [ ["--vote-file", voteFile] | File voteFile <- voteFiles ] ++
    [ "--witness-override", show (length voteFiles + 1)
    , "--out-file", voteTxBodyFp
    ]
  signedVoteTx <- signTx execConfig cEra baseDir "signed-votes" (File voteTxBodyFp) $
    Some (paymentKeyInfoPair wallet)
      : Some (defaultSpoColdKeyPair 1)
      : [ Some (defaultDRepKeyPair n) | n <- [1..3] ]
  submitTx execConfig cEra signedVoteTx

  pure (govActionTxId, govActionIndex)

-- | The major protocol version currently in the ledger's protocol parameters,
-- read from whatever era the node is in. This must not fix the era, because the
-- version being awaited is the one whose enactment moves the node into Dijkstra.
getProtocolMajorVersion
  :: (HasCallStack, MonadTest m, MonadIO m, H.MonadAssertion m)
  => EpochStateView
  -> m Natural
getProtocolMajorVersion epochStateView = withFrozenCallStack $ do
  AnyNewEpochState sbe nes _ <- getEpochState epochStateView
  pure $ shelleyBasedEraConstraints sbe $
    L.getVersion $ L.pvMajor $ nes ^. L.nesEsL . L.curPParamsEpochStateL . L.ppProtocolVersionL

-- | Assert that the node reports the Dijkstra era over node-to-client.
--
-- Needs ouroboros-consensus >= 5.0: earlier versions enact protocol version 12 but
-- stay in Conway, because 'protocolInfoCardano' builds the Conway ledger config with
-- 'TriggerHardForkNotDuringThisExecution' and ignores the Dijkstra trigger.
assertNodeInDijkstra
  :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m)
  => H.ExecConfig
  -> m ()
assertNodeInDijkstra execConfig = withFrozenCallStack $ do
  H.note_ "query tip reports the Dijkstra era"
  tipStr <- H.noteM $ execCli' execConfig [ "query", "tip", "--output-json" ]
  tip :: A.Object <- H.nothingFail $ A.decode $ B.pack tipStr
  case KM.lookup "era" tip of
    Just (A.String "Dijkstra") -> pure ()
    other -> H.failMessage callStack $ unlines
      [ "The node is not in the Dijkstra era after protocol version 12 was enacted: " <> show other
      , "The pinned ouroboros-consensus does not hard fork out of Conway (the Conway ledger config"
      , "uses TriggerHardForkNotDuringThisExecution); ouroboros-consensus >= 5.0 is needed."
      ]
