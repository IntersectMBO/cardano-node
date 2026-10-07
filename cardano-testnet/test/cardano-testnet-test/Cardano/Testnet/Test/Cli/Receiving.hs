{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Cli.Receiving (hprop_receiving_lifecycle) where

import           Cardano.Api
import           Cardano.Api.Experimental (Some (Some))
import           Cardano.Api.Ledger (EpochInterval (..))
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Node.Testnet.Paths (defaultNamedNodeDataDir, defaultNodeTopologyFile)
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Default.Class (Default (def))
import           Data.Foldable (toList)
import           Data.List (isInfixOf, sort)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Lens.Micro ((^.))
import           System.Directory (makeAbsolute)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import           System.Process (interruptProcessGroupOf)

import           Testnet.Components.Query (TestnetWaitPeriod (..), findAllUtxos,
                   findLargestUtxoForPaymentKey, getEpochStateView, retryUntilJustM)
import           Testnet.Process.Cli.Transaction (failToSubmitTx, retrieveTransactionId, signTx,
                   submitTx)
import           Testnet.Process.Run (execCli', execCliStdoutToJson, mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Runtime (startNode)
import           Testnet.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

-- | Two admitted transactions exercise Receiving and later Spending through the
-- real node, with a rejected recipient-signature variant of the same first body.
-- Run with: DISABLE_RETRIES=1 cabal test cardano-testnet-test
--   --test-options '-p "/Dijkstra Receiving lifecycle/"'
hprop_receiving_lifecycle :: Property
hprop_receiving_lifecycle = integrationRetryWorkspace 2 "dijkstra-receiving" $ \base -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf base
  let tempPath = unTmpAbsPath tempAbsPath
      sbe = ShelleyBasedEraDijkstra
      cEra = AnyCardanoEra DijkstraEra
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
  fixture <- H.evalIO $ makeAbsolute "test/cardano-testnet-test/files/plutus/v4/receiving-even-datum.plutus"
  work <- H.createDirectoryIfMissing $ tempPath </> "work"
  runtime@TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes
    , wallets = sender : collateral : recipient : _
    } <- createAndRunTestnet creationOptions def conf
  let node = NEL.head testnetNodes
      recipientAddress = Text.unpack (paymentKeyInfoAddr recipient)
      senderAddress = Text.unpack (paymentKeyInfoAddr sender)
  execConfig <- mkExecConfig (makeTmpBaseAbsPath (TmpAbsolutePath tempPath)) (nodeSprocket node) testnetMagic
  let queryNodeUTxO queryExecConfig queryName = do
        let queryFile = work </> queryName <> "-utxo.json"
        void $ execCli' queryExecConfig
          [ "dijkstra", "query", "utxo", "--whole-utxo", "--cardano-mode"
          , "--out-file", queryFile
          ]
        queried :: UTxO DijkstraEra <- H.readJsonFileOk queryFile
        pure queried
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node)
  fundingInput <- findLargestUtxoForPaymentKey epochStateView sbe sender
  collateralInput <- findLargestUtxoForPaymentKey epochStateView sbe collateral
  recipientHash <- Text.unpack . Text.strip . Text.pack <$> execCli' execConfig
    [ "dijkstra", "address", "key-hash"
    , "--payment-verification-key-file", verificationKeyFp (paymentKeyInfoPair recipient)
    ]
  let nativeFile = work </> "recipient-native.json"
  H.writeFile nativeFile $ "{\"type\":\"sig\",\"keyHash\":\"" <> recipientHash <> "\"}"
  nativeAddress <- Text.unpack . Text.strip . Text.pack <$> execCli' execConfig
    [ "dijkstra", "address", "build", "--payment-script-file", nativeFile
    , "--testnet-magic", show testnetMagic
    ]
  plutusAddress <- Text.unpack . Text.strip . Text.pack <$> execCli' execConfig
    [ "dijkstra", "address", "build", "--payment-script-file", fixture
    , "--testnet-magic", show testnetMagic
    ]
  [keyProtected, nativeProtected, plutusProtected] <- mapM
    (\address -> Text.unpack . Text.strip . Text.pack <$> execCli' execConfig
      ["dijkstra", "address", "protect", "--address", address])
    [recipientAddress, nativeAddress, plutusAddress]
  nativeHash <- Text.unpack . Text.strip . Text.pack <$> execCli' execConfig
    ["dijkstra", "transaction", "policyid", "--script-file", nativeFile]
  plutusHash <- Text.unpack . Text.strip . Text.pack <$> execCli' execConfig
    ["dijkstra", "transaction", "policyid", "--script-file", fixture]
  void $ H.note $ "Native Receiving script hash: " <> nativeHash
  void $ H.note $ "V4 Receiving script hash: " <> plutusHash
  let creationBody = File (work </> "creation.txbody")
      creationOutputs =
        [ "--tx-out", keyProtected <> "+5000000"
        , "--tx-out", nativeProtected <> "+6000000"
        , "--tx-out", plutusProtected <> "+7000000", "--tx-out-inline-datum-value", "2"
        , "--tx-out", plutusProtected <> "+8000000", "--tx-out-inline-datum-value", "4"
        ]
  void $ execCli' execConfig $
    [ "dijkstra", "transaction", "build"
    , "--tx-in", Text.unpack (renderTxIn fundingInput)
    , "--tx-in-collateral", Text.unpack (renderTxIn collateralInput)
    , "--change-address", senderAddress
    ] <> creationOutputs <>
    [ "--receiving-output-index", "1", "--receiving-script-file", nativeFile
    , "--receiving-output-index", "2", "--receiving-script-file", fixture
    , "--receiving-redeemer-value", "0"
    , "--receiving-output-index", "3", "--receiving-script-file", fixture
    , "--receiving-redeemer-value", "0"
    , "--out-file", unFile creationBody
    ]
  creationEnvelope <- H.readJsonFileOk (unFile creationBody)
  unsignedCreation :: Tx DijkstraEra <- H.evalEither $ deserialiseFromTextEnvelope creationEnvelope
  let creationId = txIdFor unsignedCreation
      expectedCreated = outputsByInput unsignedCreation
      protectedCreated = Map.filter isProtectedOutput expectedCreated
      expectedSpecs =
        [ (Text.pack keyProtected, Nothing)
        , (Text.pack nativeProtected, Nothing)
        , (Text.pack plutusProtected, Just 2)
        , (Text.pack plutusProtected, Just 4)
        ]
  sort (map outputSpec (Map.elems protectedCreated)) === sort expectedSpecs
  length protectedCreated === 4
  -- The native witness is shared authorization. Each protected V4 output
  -- has its own original body index, redeemer and execution budget.
  creationView :: Aeson.Value <- execCliStdoutToJson execConfig
    ["debug", "transaction", "view", "--tx-body-file", unFile creationBody, "--output-json"]
  case creationView of
    Aeson.Object fields -> case KeyMap.lookup "redeemers" fields of
      Just (Aeson.Array redeemers) -> do
        length redeemers === 2
        let pointers =
              [ KeyMap.lookup "redeemer pointer" redeemerFields
              | Aeson.Object redeemerFields <- toList redeemers
              ]
            receivingPointer :: Int -> Aeson.Value
            receivingPointer index = Aeson.object
              [ "kind" Aeson..= ("DijkstraReceiving" :: Text.Text)
              , "value" Aeson..= Aeson.object ["index" Aeson..= index]
              ]
        pointers === map (Just . receivingPointer) [2, 3]
      _ -> H.failure
    _ -> H.failure
  before@(UTxO beforeMap) <- findAllUtxos epochStateView sbe
  H.assert $ Map.member fundingInput beforeMap && Map.member collateralInput beforeMap
  H.assert $ all (\input -> not (Map.member input beforeMap)) (Map.keys expectedCreated)
  nodeBefore@(UTxO nodeBeforeMap) <- queryNodeUTxO execConfig "before-rejected-creation"
  H.assert $ Map.member fundingInput nodeBeforeMap && Map.member collateralInput nodeBeforeMap
  H.assert $ all (\input -> not (Map.member input nodeBeforeMap)) (Map.keys expectedCreated)
  missingRecipient <- signTx execConfig cEra work "missing-recipient" creationBody
    [Some (paymentKeyInfoPair sender), Some (paymentKeyInfoPair collateral)]
  retrieveTransactionId execConfig missingRecipient >>= (=== creationId)
  failToSubmitTx execConfig cEra missingRecipient "MissingVKeyWitnessesUTXOW"
  after <- findAllUtxos epochStateView sbe
  after === before
  nodeAfter <- queryNodeUTxO execConfig "after-rejected-creation"
  nodeAfter === nodeBefore
  signedCreation <- signTx execConfig cEra work "creation" creationBody
    [ Some (paymentKeyInfoPair sender)
    , Some (paymentKeyInfoPair collateral)
    , Some (paymentKeyInfoPair recipient)
    ]
  retrieveTransactionId execConfig signedCreation >>= (=== creationId)
  submitTx execConfig cEra signedCreation
  confirmedCreation@(UTxO confirmedMap) <- retryUntilJustM epochStateView (WaitForEpochs (EpochInterval 2)) $ do
    actual@(UTxO actualMap) <- findAllUtxos epochStateView sbe
    pure $ if Map.restrictKeys actualMap (Map.keysSet expectedCreated) == expectedCreated then Just actual else Nothing
  H.assert $ not (Map.member fundingInput confirmedMap)
  Map.lookup collateralInput confirmedMap === Map.lookup collateralInput beforeMap
  -- Independently replay the chain through the API fold from genesis. Exact
  -- TxIns, complete values, protected addresses and datums must match the body.
  Map.restrictKeys confirmedMap (Map.keysSet expectedCreated) === expectedCreated
  sort (map outputSpec (Map.elems (Map.restrictKeys confirmedMap (Map.keysSet protectedCreated)))) === sort expectedSpecs
  H.noteShow_ confirmedCreation
  -- Query the live node via Local State Query as separate evidence from replay.
  UTxO nodeCreatedMap <- retryUntilJustM epochStateView (WaitForEpochs (EpochInterval 2)) $ do
    queried@(UTxO queriedMap) <- queryNodeUTxO execConfig "confirmed-creation"
    pure $ if Map.restrictKeys queriedMap (Map.keysSet expectedCreated) == expectedCreated then Just queried else Nothing
  Map.restrictKeys nodeCreatedMap (Map.keysSet expectedCreated) === expectedCreated
  sort (map outputSpec (Map.elems (Map.restrictKeys nodeCreatedMap (Map.keysSet protectedCreated)))) === sort expectedSpecs
  H.assert $ not (Map.member fundingInput nodeCreatedMap)
  Map.lookup collateralInput nodeCreatedMap === Map.lookup collateralInput nodeBeforeMap
  -- Keep the producer and its API replay running while one relay is stopped.
  -- A fresh socket/log name preserves its original logs; its database and
  -- topology paths, TCP port, configuration and genesis files stay the same.
  relay : _ <- pure (relayNodes runtime)
  relayExecConfig <- mkExecConfig (makeTmpBaseAbsPath tempAbsPath) (nodeSprocket relay) testnetMagic
  relayBefore@(UTxO relayBeforeMap) <- retryUntilJustM epochStateView (WaitForEpochs (EpochInterval 2)) $ do
    queried@(UTxO queriedMap) <- queryNodeUTxO relayExecConfig "relay-before-restart"
    pure $ if Map.restrictKeys queriedMap (Map.keysSet expectedCreated) == expectedCreated then Just queried else Nothing
  Map.restrictKeys relayBeforeMap (Map.keysSet protectedCreated) === protectedCreated
  H.evalIO $ interruptProcessGroupOf (nodeProcessHandle relay)
  shutdown <- H.waitSecondsForProcess 5 (nodeProcessHandle relay)
  case shutdown of
    Right (ExitFailure _) -> pure ()
    other -> H.annotateShow other >> H.failure
  oldRelayLogs <- H.readFile (nodeStdout relay)
  H.assert $ "\"kind\":\"TraceOpenEvent.ClosedDB\"" `isInfixOf` oldRelayLogs
  restarted <- H.evalEither =<< runExceptT (retryOnAddressInUseError $
    startNode tempAbsPath (nodeName relay <> "-restored") (nodeIpv4 relay) (nodePort relay) testnetMagic Nothing
      [ "run"
      , "--config", unFile configurationFile
      , "--topology", tempPath </> defaultNodeTopologyFile (nodeName relay)
      , "--database-path", tempPath </> defaultNamedNodeDataDir (nodeName relay) </> "db"
      ])
  restartedExecConfig <- mkExecConfig (makeTmpBaseAbsPath tempAbsPath) (nodeSprocket restarted) testnetMagic
  restartedView <- getEpochStateView configurationFile (nodeSocketPath restarted)
  relayRestored <- retryUntilJustM restartedView (WaitForEpochs (EpochInterval 2)) $ do
    queried@(UTxO queriedMap) <- queryNodeUTxO restartedExecConfig "relay-after-restart"
    pure $ if Map.restrictKeys queriedMap (Map.keysSet expectedCreated) == expectedCreated then Just queried else Nothing
  relayRestored === relayBefore
  restoredReplay <- retryUntilJustM restartedView (WaitForEpochs (EpochInterval 2)) $ do
    actual@(UTxO actualMap) <- findAllUtxos restartedView sbe
    pure $ if Map.restrictKeys actualMap (Map.keysSet expectedCreated) == expectedCreated then Just actual else Nothing
  restoredReplay === relayBefore
  -- This is same-database restart/replay evidence. Snapshot restoration requires
  -- matching snapshot creation/replay events and is not inferred from restart.
  let keyInputs = Map.keys (Map.filter ((== Text.pack keyProtected) . fst . outputSpec) protectedCreated)
      nativeInputs = Map.keys (Map.filter ((== Text.pack nativeProtected) . fst . outputSpec) protectedCreated)
      plutusInputs = Map.keys (Map.filter ((== Text.pack plutusProtected) . fst . outputSpec) protectedCreated)
      spendingBody = File (work </> "spending.txbody")
  length keyInputs === 1
  length nativeInputs === 1
  length plutusInputs === 2
  void $ execCli' restartedExecConfig $
    [ "dijkstra", "transaction", "build"
    , "--change-address", recipientAddress
    , "--tx-in-collateral", Text.unpack (renderTxIn collateralInput)
    , "--tx-out", senderAddress <> "+5000000"
    ] <>
    concatMap (\input -> ["--tx-in", Text.unpack (renderTxIn input)]) keyInputs <>
    concatMap (\input -> ["--tx-in", Text.unpack (renderTxIn input), "--tx-in-script-file", nativeFile]) nativeInputs <>
    concatMap (\input ->
      [ "--tx-in", Text.unpack (renderTxIn input), "--tx-in-script-file", fixture
      , "--tx-in-inline-datum-present", "--tx-in-redeemer-value", "0"
      ]) plutusInputs <>
    ["--out-file", unFile spendingBody]
  spendingEnvelope <- H.readJsonFileOk (unFile spendingBody)
  unsignedSpending :: Tx DijkstraEra <- H.evalEither $ deserialiseFromTextEnvelope spendingEnvelope
  let expectedSpent = outputsByInput unsignedSpending
  H.assert $ not (any isProtectedOutput (Map.elems expectedSpent))
  signedSpending <- signTx restartedExecConfig cEra work "spending" spendingBody
    [Some (paymentKeyInfoPair collateral), Some (paymentKeyInfoPair recipient)]
  submitTx restartedExecConfig cEra signedSpending
  UTxO spentMap <- retryUntilJustM epochStateView (WaitForEpochs (EpochInterval 2)) $ do
    actual@(UTxO actualMap) <- findAllUtxos epochStateView sbe
    pure $ if all (\input -> not (Map.member input actualMap)) (Map.keys protectedCreated)
        && Map.restrictKeys actualMap (Map.keysSet expectedSpent) == expectedSpent
      then Just actual else Nothing
  Map.restrictKeys spentMap (Map.keysSet expectedSpent) === expectedSpent
  H.assert $ all (\input -> not (Map.member input spentMap)) (Map.keys protectedCreated)
  Map.lookup collateralInput spentMap === Map.lookup collateralInput beforeMap
  UTxO nodeSpentMap <- retryUntilJustM epochStateView (WaitForEpochs (EpochInterval 2)) $ do
    queried@(UTxO queriedMap) <- queryNodeUTxO restartedExecConfig "confirmed-spending"
    pure $ if all (\input -> not (Map.member input queriedMap)) (Map.keys protectedCreated)
        && Map.restrictKeys queriedMap (Map.keysSet expectedSpent) == expectedSpent
      then Just queried else Nothing
  Map.restrictKeys nodeSpentMap (Map.keysSet expectedSpent) === expectedSpent
  H.assert $ all (\input -> not (Map.member input nodeSpentMap)) (Map.keys protectedCreated)
  Map.lookup collateralInput nodeSpentMap === Map.lookup collateralInput nodeBeforeMap

outputsByInput :: Tx DijkstraEra -> Map.Map TxIn (TxOut CtxUTxO DijkstraEra)
outputsByInput tx@(ShelleyTx sbe ledgerTx) = Map.fromList
  [ (TxIn (txIdFor tx) (TxIx index), fromShelleyTxOut sbe txOut)
  | (index, txOut) <- zip [0 ..] (toList (ledgerTx ^. L.bodyTxL . L.outputsTxBodyL))
  ]

txIdFor :: Tx DijkstraEra -> TxId
txIdFor (ShelleyTx _ ledgerTx) = fromShelleyTxId (Ledger.txIdTxBody (ledgerTx ^. L.bodyTxL))

isProtectedOutput :: TxOut CtxUTxO DijkstraEra -> Bool
isProtectedOutput (TxOut (AddressInEra addressType address) _ _ _) = case addressType of
  ShelleyAddressInEra{} -> isProtectedShelleyAddress address
  ByronAddressInAnyEra -> False

outputSpec :: TxOut CtxUTxO DijkstraEra -> (Text.Text, Maybe Integer)
outputSpec (TxOut address _ datum _) = (serialiseAddress address, case datum of
  TxOutDatumInline _ value -> case getScriptData value of
    ScriptDataNumber number -> Just number
    _ -> Nothing
  _ -> Nothing)
