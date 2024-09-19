{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.KesPeriodInfo
  ( hprop_kes_period_info
  ) where

import           Cardano.Api as Api

import           Cardano.CLI.Types.Output
import           Cardano.Node.Configuration.Topology
import           Cardano.Testnet
import           Cardano.Testnet.Test.Misc

import           Prelude

import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import           Data.Default.Class
import           Data.Function
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           GHC.Stack (callStack)
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Configuration
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Cli.SPO
import           Testnet.Process.Run (execCli, execCli', mkExecConfig)
import           Testnet.Property.Util (decodeEraUTxO, integrationRetryWorkspace)
import           Testnet.Runtime
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Hedgehog.Extras (threadDelay)
import           Hedgehog.Extras.Stock (sprocketSystemName)
import qualified Hedgehog.Extras.Stock.IO.Network.Port as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/kes-period-info/"'@
hprop_kes_period_info :: Property
hprop_kes_period_info = integrationRetryWorkspace 2 "kes-period-info" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  H.note_ SYS.os
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) }
    -- TODO: Move yaml filepath specification into individual node options
    <- mkConf tempAbsBasePath'

  let tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath
      sbe = ShelleyBasedEraConway
      asbe = AnyShelleyBasedEra sbe
      eraString = eraToString sbe
      cTestnetOptions = def { cardanoNodeEra = asbe }

  runTime@TestnetRuntime
    { configurationFile
    , testnetMagic
    , wallets=wallet0:_
    , poolNodes
    } <- cardanoTestnetDefault cTestnetOptions def conf
  node1sprocket <- H.headM $ poolSprockets runTime
  execConfig <- mkExecConfig tempBaseAbsPath node1sprocket testnetMagic

  -- We get our UTxOs from here
  let utxoAddr = Text.unpack $ paymentKeyInfoAddr wallet0
      utxoSKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
  void $ execCli' execConfig
    [ eraString, "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--out-file", work </> "utxo-1.json"
    ]

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ decodeEraUTxO sbe utxo1Json
  txin1 <- H.noteShow =<< H.headM (Map.keys utxo1)

  let node1SocketPath = Api.File $ IO.sprocketSystemName node1sprocket
      termEpoch = EpochNo 3
  (stakePoolId, stakePoolColdSigningKey, stakePoolColdVKey, _, _)
    <- registerSingleSpo asbe 1 tempAbsPath
         configurationFile
         node1SocketPath
         termEpoch
         testnetMagic
         execConfig
         (txin1, utxoSKeyFile, utxoAddr)
  
  H.noteShow_ $ "Test SPO stake pool id: " <> stakePoolId 
  
  -- Create test stake address to delegate to the new stake pool
  -- NB: We need to fund the payment credential of the overall address
  --------------------------------------------------------------

  let testStakeDelegator = work </> "test-delegator"

  H.createDirectoryIfMissing_ testStakeDelegator
  let testDelegatorVkeyFp = testStakeDelegator </> "test-delegator.vkey"
      testDelegatorSKeyFp = testStakeDelegator </> "test-delegator.skey"
      testDelegatorPaymentVKeyFp = testStakeDelegator </> "test-delegator-payment.vkey"
      testDelegatorPaymentSKeyFp = testStakeDelegator </> "test-delegator-payment.skey"
      testDelegatorRegCertFp = testStakeDelegator </> "test-delegator.regcert"
      testDelegatorDelegCert = testStakeDelegator </> "test-delegator.delegcert"

  cliStakeAddressKeyGen
    $ KeyPair (File testDelegatorVkeyFp) (File testDelegatorSKeyFp)
  cliAddressKeyGen
    $ KeyPair (File testDelegatorPaymentVKeyFp) (File testDelegatorPaymentSKeyFp)

  -- NB: We must include the stake credential
  testDelegatorPaymentAddr <- execCli
                [ "address", "build"
                , "--testnet-magic", show @Int testnetMagic
                , "--payment-verification-key-file", testDelegatorPaymentVKeyFp
                , "--stake-verification-key-file", testDelegatorVkeyFp
                ]
  testDelegatorStakeAddress
    <- filter (/= '\n')
         <$> execCli
               [ "stake-address", "build"
               , "--stake-verification-key-file", testDelegatorVkeyFp
               , "--testnet-magic", show @Int testnetMagic
               ]

  -- Test stake address registration cert
  createStakeKeyRegistrationCertificate
    tempAbsPath
    (cardanoNodeEra cTestnetOptions)
    testDelegatorVkeyFp
    0
    testDelegatorRegCertFp

  -- Test stake address deleg  cert
  createStakeDelegationCertificate
    tempAbsPath
    sbe
    testDelegatorVkeyFp
    stakePoolId
    testDelegatorDelegCert

  -- TODO: Refactor getting valid UTxOs into a function
  H.note_  "Get updated UTxO"

  void $ execCli' execConfig
    [ eraString, "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--out-file", work </> "utxo-2.json"
    ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ decodeEraUTxO sbe utxo2Json
  txin2 <- H.noteShow =<< H.headM (Map.keys utxo2)

  let delegRegTestDelegatorTxBodyFp = work </> "deleg-register-test-delegator.txbody"

  void $ execCli' execConfig
    [ eraString
    , "transaction", "build"
    , "--change-address", testDelegatorPaymentAddr -- NB: A large balance ends up at our test delegator's address
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", utxoAddr <> "+" <> show @Int 5_000_000
    , "--witness-override", show @Int 3
    , "--certificate-file", testDelegatorRegCertFp
    , "--certificate-file", testDelegatorDelegCert
    , "--out-file", delegRegTestDelegatorTxBodyFp
    ]

  let delegRegTestDelegatorTxFp = work </> "deleg-register-test-delegator.tx"
  void $ execCli
    [ "transaction", "sign"
    , "--tx-body-file", delegRegTestDelegatorTxBodyFp
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", testDelegatorSKeyFp
    , "--out-file", delegRegTestDelegatorTxFp
    ]

  H.note_ "Submitting test delegator registration and delegation certificates..."

  void $ execCli' execConfig
           [ "transaction", "submit"
           , "--tx-file", delegRegTestDelegatorTxFp
           ]

  let testDelegatorStakeAddressInfoOutFp = work </> "test-delegator-stake-address-info.json"
  void $ checkStakeKeyRegistered
           tempAbsPath
           configurationFile
           node1SocketPath
           termEpoch
           execConfig
           testDelegatorStakeAddress
           testDelegatorStakeAddressInfoOutFp

  -- TODO: We need a separate function that allows us to run single nodes after
  -- we have started a cluster with create-staked
  let testSpoDir = work </> "test-spo"
      topologyFile = testSpoDir </> "topology.json"
  H.createDirectoryIfMissing_ testSpoDir
  let valency = 1
      topology = RealNodeTopology $
        flip map poolNodes $ \PoolNode{poolRuntime=NodeRuntime{nodeIpv4,nodePort}} ->
            RemoteAddress (showIpv4Address nodeIpv4) nodePort valency
  H.lbsWriteFile topologyFile $ Aeson.encode topology

  let testSpoVrfVKey = work </> "vrf.vkey"
      testSpoVrfSKey = work </> "vrf.skey"
      testSpoKesVKey = work </> "kes.vkey"
      testSpoKesSKey = work </> "kes.skey"

  cliNodeKeyGenVrf
    $ KeyPair (File testSpoVrfVKey) (File testSpoVrfSKey)
  cliNodeKeyGenKes
    $ KeyPair (File testSpoKesVKey) (File testSpoKesSKey)
  let testSpoOperationalCertFp = testSpoDir </> "node-operational.cert"

  void $ execCli' execConfig
    [ "node", "new-counter"
    , "--cold-verification-key-file", stakePoolColdVKey
    , "--counter-value", "0"
    , "--operational-certificate-issue-counter-file", testSpoOperationalCertFp
    ]


  void $ execCli' execConfig
      [ "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file", testSpoKesVKey
      , "--cold-signing-key-file", stakePoolColdSigningKey
      , "--operational-certificate-issue-counter-file", testSpoOperationalCertFp
      , "--out-file", testSpoOperationalCertFp
      ]

  jsonBS <- createConfigJson tempAbsPath sbe
  H.lbsWriteFile (unFile configurationFile) jsonBS
  newNodePortNumber <- H.randomPort testnetDefaultIpv4Address
  eRuntime <- runExceptT . retryOnAddressInUseError $
    startNode tempAbsPath "test-spo" testnetDefaultIpv4Address newNodePortNumber testnetMagic
        [ "run"
        , "--config", unFile configurationFile
        , "--topology", topologyFile
        , "--database-path", testSpoDir </> "db"
        , "--shelley-kes-key", testSpoKesSKey
        , "--shelley-vrf-key", testSpoVrfSKey
        , "--shelley-operational-certificate", testSpoOperationalCertFp
        ]
  NodeRuntime{ nodeStdout } <- H.evalEither eRuntime

  threadDelay 5_000_000

  stakeSnapshot1 <- execCli' execConfig
     [ "query", "stake-snapshot"
     , "--all-stake-pools"
     ]
  H.writeFile (work </> "stake-snapshot-1.json") stakeSnapshot1

  ledgerStateJson <- execCli' execConfig
    [ "query", "ledger-state"
    , "--cardano-mode"
    ]
  H.writeFile (work </> "ledger-state-1.json") ledgerStateJson

  let kesPeriodInfoOutput = testSpoDir </> "kes-period-info-expected-success.json"
  void $ execCli' execConfig
    [ "query", "kes-period-info"
    , "--op-cert-file", testSpoOperationalCertFp
    , "--out-file", kesPeriodInfoOutput
    ]
  kesPeriodInfoExpectedSuccess <- H.leftFailM $ H.readJsonFile kesPeriodInfoOutput
  kesPeriodOutputSuccess <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryKesPeriodInfoOutput kesPeriodInfoExpectedSuccess


  prop_op_cert_valid_kes_period testSpoOperationalCertFp kesPeriodOutputSuccess

  H.note_ $ mconcat
    [ "Wait for the node to mint blocks. This will be in the following epoch so lets wait"
    , " until the END of the following epoch."
    ]

  void $ execCli' execConfig
    [ "query",  "tip"
    , "--out-file", work </> "current-tip.json"
    ]

  tipJSON <- H.leftFailM . H.readJsonFile $ work </> "current-tip.json"
  tip <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryTipLocalStateOutput tipJSON
  currEpoch <-
    case mEpoch tip of
      Nothing ->
        H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch -> return currEpoch

  let nodeHasMintedEpoch = currEpoch & succ & succ & succ
  currentEpoch <- waitUntilEpoch
                   configurationFile
                   (Api.File $ sprocketSystemName node1sprocket)
                   nodeHasMintedEpoch

  H.note_ "Check we have reached at least 3 epochs ahead"
  if currentEpoch >= nodeHasMintedEpoch
  then H.success
  else H.failMessage
       callStack $ "We have not reached our target epoch. Target epoch: " <> show nodeHasMintedEpoch <>
                   " Current epoch: " <> show currentEpoch



  void $ execCli' execConfig
    [ "query",  "tip"
    , "--out-file", work </> "current-tip-2.json"
    ]

  tip2JSON <- H.leftFailM . H.readJsonFile $ work </> "current-tip-2.json"
  tip2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryTipLocalStateOutput tip2JSON

  currEpoch2 <-
    case mEpoch tip2 of
      Nothing ->
        H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch2 -> return currEpoch2

  H.note_ $ "Current Epoch: " <> show currEpoch2

  H.note_ $ mconcat
    [ "Check to see if the node has minted blocks. This confirms that the operational"
    , " certificate is valid"
    ]
  stakeSnapshot2 <- execCli' execConfig
     [ "query", "stake-snapshot"
     , "--all-stake-pools"
     ]

  -- TODO: Create a check here that confirms there are four stake pools and each has stake!
  H.writeFile (work </> "stake-snapshot-2.json") stakeSnapshot2

  ledgerStateJson2 <- execCli' execConfig
    [ "query", "ledger-state"
    , "--cardano-mode"
    ]
  H.writeFile (work </> "ledger-state-2.json") ledgerStateJson2

  spoLogFile <- H.note nodeStdout
  prop_node_minted_block spoLogFile
