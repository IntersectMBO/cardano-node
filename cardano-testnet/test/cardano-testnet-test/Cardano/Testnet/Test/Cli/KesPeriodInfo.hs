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

import           Cardano.Api hiding (cardanoEra)

import           Cardano.CLI.Types.Output
import           Cardano.Testnet
import           Cardano.Testnet.Test.Misc

import           Prelude

import           Control.Monad (void)
import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           GHC.Stack (callStack)
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Hedgehog.Extras (threadDelay)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

import           Testnet.Components.Configuration
import           Testnet.Components.SPO
import           Testnet.Process.Cli
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

{- HLINT ignore "Use underscore -}

hprop_kes_period_info :: Property
hprop_kes_period_info = H.integrationRetryWorkspace 2 "kes-period-info" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf@Conf { tempAbsPath }
    -- TODO: Move yaml filepath specification into individual node options
    <- H.noteShowM $ mkConf tempAbsBasePath'

  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath
      era = BabbageEra
      cTestnetOptions = cardanoDefaultTestnetOptions
                          { cardanoNodes = cardanoDefaultTestnetNodeOptions
                          , cardanoEpochLength = 1_000
                          , cardanoSlotLength = 0.02
                          , cardanoActiveSlotsCoeff = 0.1
                          , cardanoEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
                          }
      fastTestnetOptions = CardanoOnlyTestnetOptions cTestnetOptions

  sbe <- case cardanoEraStyle era of
           ShelleyBasedEra era' -> return era'

  runTime@TestnetRuntime { testnetMagic } <- testnet fastTestnetOptions conf

  execConfig <- H.headM (poolSprockets runTime) >>= H.mkExecConfig tempBaseAbsPath

  -- First we note all the relevant files
  work <- H.note tempAbsPath'

  -- We get our UTxOs from here
  utxoVKeyFile <- H.note $ tempAbsPath' </> "utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath' </> "utxo-keys/utxo1.skey"

  utxoAddr <- execCli
                [ "address", "build"
                , "--testnet-magic", show @Int testnetMagic
                , "--payment-verification-key-file", utxoVKeyFile
                ]

  void $ execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-1.json"
      ]

  H.cat $ work </> "utxo-1.json"

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ decodeEraUTxO sbe utxo1Json
  txin <- H.noteShow =<< H.headM (Map.keys utxo1)

  (stakePoolId, stakePoolColdSigningKey, stakePoolColdVKey, _, _)
    <- registerSingleSpo 1 tempAbsPath cTestnetOptions execConfig (txin, utxoSKeyFile, utxoAddr)

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

  _ <- cliStakeAddressKeyGen tempAbsPath'
    $ KeyNames testDelegatorVkeyFp testDelegatorSKeyFp
  _ <- cliAddressKeyGen tempAbsPath'
    $ KeyNames testDelegatorPaymentVKeyFp testDelegatorPaymentSKeyFp

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
    (cardanoEra cTestnetOptions)
    testDelegatorVkeyFp
    testDelegatorRegCertFp

  -- Test stake address deleg  cert
  createStakeDelegationCertificate
    tempAbsPath
    (cardanoEra cTestnetOptions)
    testDelegatorVkeyFp
    stakePoolId
    testDelegatorDelegCert

  -- TODO: Refactor getting valid UTxOs into a function
  H.note_  "Get updated UTxO"

  void $ execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-2.json"
      ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ H.noteShowM $ decodeEraUTxO sbe utxo2Json
  txin2 <- H.noteShow =<< H.headM (Map.keys utxo2)

  let eraFlag = convertToEraFlag $ cardanoEra cTestnetOptions
      delegRegTestDelegatorTxBodyFp = work </> "deleg-register-test-delegator.txbody"

  void $ execCli' execConfig
    [ "transaction", "build"
    , eraFlag
    , "--testnet-magic", show @Int testnetMagic
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
           , "--testnet-magic", show @Int testnetMagic
           ]

  threadDelay 15_000000
  let testDelegatorStakeAddressInfoOutFp = work </> "test-delegator-stake-address-info.json"
  void $ checkStakeKeyRegistered
           tempAbsPath
           execConfig
           cTestnetOptions
           testDelegatorStakeAddress
           testDelegatorStakeAddressInfoOutFp

  -- TODO: We need a separate function that allows us to run single nodes after
  -- we have started a cluster with create-staked
  let testSpoDir = work </> "test-spo"
      topologyFile = testSpoDir </> "topology.json"
  H.createDirectoryIfMissing_ testSpoDir
  -- TODO: We need a way to automatically create this based on
  -- the existing testnet
  H.lbsWriteFile topologyFile $ Aeson.encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3002
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3001
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3003
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]
  let testSpoVrfVKey = work </> "vrf.vkey"
      testSpoVrfSKey = work </> "vrf.skey"
      testSpoKesVKey = work </> "kes.vkey"
      testSpoKesSKey = work </> "kes.skey"

  _ <- cliNodeKeyGenVrf tempAbsPath'
         $ KeyNames testSpoVrfVKey testSpoVrfSKey
  _ <- cliNodeKeyGenKes tempAbsPath'
         $ KeyNames testSpoKesVKey testSpoKesSKey
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

  yamlBs <- createConfigYaml tempAbsPath (cardanoEra cTestnetOptions)
  H.lbsWriteFile (work </> "configuration.yaml") yamlBs
  _runtime <- startNode (TmpAbsolutePath tempAbsPath') "test-spo" 3005
        [ "run"
        , "--config", (work </> "configuration.yaml")
        , "--topology", topologyFile
        , "--database-path", testSpoDir </> "db"
        , "--shelley-kes-key", testSpoKesSKey
        , "--shelley-vrf-key", testSpoVrfSKey
        , "--shelley-operational-certificate", testSpoOperationalCertFp
        ]
  threadDelay 5_000000

  stakeSnapshot1 <- execCli' execConfig
     [ "query", "stake-snapshot"
     , "--testnet-magic", show @Int testnetMagic
     , "--all-stake-pools"
     ]
  H.writeFile (work </> "stake-snapshot-1.json") stakeSnapshot1

  ledgerStateJson <- execCli' execConfig
    [ "query", "ledger-state"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    ]
  H.writeFile (work </> "ledger-state-1.json") ledgerStateJson

  let kesPeriodInfoOutput = testSpoDir </> "kes-period-info-expected-success.json"
  void $ execCli' execConfig
    [ "query", "kes-period-info"
    , "--testnet-magic", show @Int testnetMagic
    , "--op-cert-file", testSpoOperationalCertFp
    , "--out-file", kesPeriodInfoOutput
    ]
  kesPeriodInfoExpectedSuccess <- H.leftFailM $ H.readJsonFile kesPeriodInfoOutput
  kesPeriodOutputSuccess <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryKesPeriodInfoOutput kesPeriodInfoExpectedSuccess

  -- We check if the operational certificate is valid for the current KES period
  prop_op_cert_valid_kes_period testSpoOperationalCertFp kesPeriodOutputSuccess

  H.note_ $ mconcat
    [ "Wait for the node to mint blocks. This will be in the following epoch so lets wait"
    , " until the END of the following epoch."
    ]

  void $ execCli' execConfig
    [ "query",  "tip"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "current-tip.json"
    ]

  tipJSON <- H.leftFailM . H.readJsonFile $ work </> "current-tip.json"
  tip <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryTipLocalStateOutput tipJSON
  currEpoch <-
    case mEpoch tip of
      Nothing ->
        H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch -> return currEpoch

  let nodeHasMintedEpoch = currEpoch + 3
  currentEpoch <- waitUntilEpoch
                   (work </> "current-tip.json")
                   testnetMagic
                   execConfig
                   nodeHasMintedEpoch

  H.note_ "Check we have reached at least 3 epochs ahead"
  if currentEpoch >= nodeHasMintedEpoch
  then H.success
  else H.failMessage
       callStack $ "We have not reached our target epoch. Target epoch: " <> show nodeHasMintedEpoch <>
                   " Current epoch: " <> show currentEpoch



  void $ execCli' execConfig
    [ "query",  "tip"
    , "--testnet-magic", show @Int testnetMagic
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
     , "--testnet-magic", show @Int testnetMagic
     , "--all-stake-pools"
     ]
  H.writeFile (work </> "stake-snapshot-2.json") stakeSnapshot2

  ledgerStateJson2 <- execCli' execConfig
    [ "query", "ledger-state"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    ]
  H.writeFile (work </> "ledger-state-2.json") ledgerStateJson2
  -- TODO: Linking to the node log file like this is fragile.
  spoLogFile <- H.note $ tempAbsPath' </> "logs/test-spo.stdout.log"
  prop_node_minted_block spoLogFile
