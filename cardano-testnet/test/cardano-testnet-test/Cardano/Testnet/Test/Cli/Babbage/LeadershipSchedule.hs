{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Use let" -}
{- HLINT ignore "Use underscore" -}


module Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule
  ( hprop_leadershipSchedule
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput (..))
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.List ((\\))
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Clock as DTC
import           GHC.Stack (callStack)
import qualified GHC.Stack as GHC
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Extras (threadDelay)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

import           Testnet.Components.Configuration
import           Testnet.Components.SPO
import           Testnet.Process.Cli
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import           Testnet.Property.Assert
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

hprop_leadershipSchedule :: Property
hprop_leadershipSchedule = H.integrationRetryWorkspace 2 "babbage-leadership-schedule" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath


  let era = BabbageEra
      cTestnetOptions = cardanoDefaultTestnetOptions
                          { cardanoNodes = cardanoDefaultTestnetNodeOptions
                          , cardanoSlotLength = 0.1
                          , cardanoActiveSlotsCoeff = 0.1
                          , cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
                          }

  tr@TestnetRuntime
    { testnetMagic
    -- , wallets
    -- , delegators
    } <- cardanoTestnet cTestnetOptions conf

  execConfig <- H.headM (poolSprockets tr) >>= H.mkExecConfig tempBaseAbsPath

  let sbe = shelleyBasedEra @BabbageEra
  work <- H.note tempAbsPath'

  ----------------Need to register an SPO------------------

 -- We get our UTxOs from here
  utxoVKeyFile <- H.note $ work </> "utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ work </> "utxo-keys/utxo1.skey"

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

  (stakePoolIdNewSpo, stakePoolColdSigningKey, stakePoolColdVKey, vrfSkey, _)
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
    (cardanoNodeEra cTestnetOptions)
    testDelegatorVkeyFp
    testDelegatorRegCertFp

  -- Test stake address deleg  cert
  createStakeDelegationCertificate
    tempAbsPath
    (cardanoNodeEra cTestnetOptions)
    testDelegatorVkeyFp
    stakePoolIdNewSpo
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

  let eraFlag = convertToEraFlag $ cardanoNodeEra cTestnetOptions
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

  -------------------------------------------------------------------

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
  let testSpoKesVKey = work </> "kes.vkey"
      testSpoKesSKey = work </> "kes.skey"


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

  yamlBs <- createConfigYaml tempAbsPath (cardanoNodeEra cTestnetOptions)
  H.lbsWriteFile (work </> "configuration.yaml") yamlBs
  eRuntime <- lift . lift . runExceptT $ startNode (TmpAbsolutePath tempAbsPath') "test-spo" 3005 testnetMagic
        [ "run"
        , "--config", work </> "configuration.yaml"
        , "--topology", topologyFile
        , "--database-path", testSpoDir </> "db"
        , "--shelley-kes-key", testSpoKesSKey
        , "--shelley-vrf-key", vrfSkey
        , "--shelley-operational-certificate", testSpoOperationalCertFp
        ]
  testPoolStdOutFp <- case eRuntime of
                       Left e -> H.failMessage GHC.callStack $ "Failed to start node: " <> show e
                       Right runtime -> return $ nodeStdout runtime
  threadDelay 5_000000


  tipDeadline <- H.noteShowM $ DTC.addUTCTime 210 <$> H.noteShowIO DTC.getCurrentTime

  H.byDeadlineM 10 tipDeadline "Wait for two epochs" $ do
    void $ execCli' execConfig
      [ "query", "tip"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "current-tip.json"
      ]

    tipJson <- H.leftFailM . H.readJsonFile $ work </> "current-tip.json"
    tip <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryTipLocalStateOutput tipJson

    currEpoch <- case mEpoch tip of
      Nothing -> H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch -> return currEpoch

    H.note_ $ "Current Epoch: " <> show currEpoch
    H.assert $ currEpoch > 2

  id do
    currentLeaderShipScheduleFile <- H.noteTempFile tempAbsPath' "current-schedule.log"

    leadershipScheduleDeadline <- H.noteShowM $ DTC.addUTCTime 180 <$> H.noteShowIO DTC.getCurrentTime

    H.byDeadlineM 5 leadershipScheduleDeadline "Failed to query for leadership schedule" $ do
      void $ execCli' execConfig
        [ "query", "leadership-schedule"
        , "--testnet-magic", show @Int testnetMagic
        , "--genesis", shelleyGenesisFile tr
        , "--stake-pool-id", stakePoolIdNewSpo
        , "--vrf-signing-key-file", vrfSkey
        , "--out-file", currentLeaderShipScheduleFile
        , "--current"
        ]

    currentScheduleJson <- H.leftFailM $ H.readJsonFile currentLeaderShipScheduleFile

    expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) currentScheduleJson

    maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers

    H.assert $ not (L.null expectedLeadershipSlotNumbers)

    leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

    -- We need enough time to pass such that the expected leadership slots generated by the
    -- leadership-schedule command have actually occurred.
    (leaderSlots, notLeaderSlots) <- H.byDeadlineM 10 leadershipDeadline "Wait for chain to surpass all expected leadership slots" $ do
      (someLeaderSlots, someNotLeaderSlots) <- getRelevantSlots testPoolStdOutFp (minimum expectedLeadershipSlotNumbers)
      if L.null someLeaderSlots
        then H.failure
        else do
          maxActualSlot <- H.noteShow $ maximum someLeaderSlots
          H.assert $ maxActualSlot >= maxSlotExpected
          pure (someLeaderSlots, someNotLeaderSlots)

    H.noteShow_ expectedLeadershipSlotNumbers
    H.noteShow_ leaderSlots
    H.noteShow_ notLeaderSlots

    -- Double check that we've seen all slots
    H.noteShow_ ("Slots not seen as TraceNodeIsLeader nor TraceNodeNotLeader" :: Text)
    ([minimum expectedLeadershipSlotNumbers .. maxSlotExpected] \\ leaderSlots) \\ notLeaderSlots === []

    -- As there are no BFT nodes, the next leadership schedule should match slots assigned exactly
    H.noteShow_ (expectedLeadershipSlotNumbers \\ leaderSlots)
    H.assert $ L.null (expectedLeadershipSlotNumbers \\ leaderSlots)
    -- TODO: Re-enable --next leadership schedule test
    {-

  id do
    nextLeaderShipScheduleFile <- H.noteTempFile tempAbsPath' "next-schedule.log"

    leadershipScheduleDeadline <- H.noteShowM $ DTC.addUTCTime 180 <$> H.noteShowIO DTC.getCurrentTime
    -- TODO: Current works, next is failing

    H.byDeadlineM 5 leadershipScheduleDeadline "Failed to query for leadership schedule" $ do
      void $ execCli' execConfig
        [ "query", "leadership-schedule"
        , "--testnet-magic", show @Int testnetMagic
        , "--genesis", shelleyGenesisFile tr
        , "--stake-pool-id", stakePoolIdNewSpo
        , "--vrf-signing-key-file", vrfSkey
        , "--out-file", nextLeaderShipScheduleFile
        , "--next"
        ]

    scheduleJson <- H.leftFailM $ H.readJsonFile nextLeaderShipScheduleFile

    expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) scheduleJson
    maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers

    H.assert $ not (L.null expectedLeadershipSlotNumbers)

    leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

    -- We need enough time to pass such that the expected leadership slots generated by the
    -- leadership-schedule command have actually occurred.
    (leaderSlots, notLeaderSlots) <- H.byDeadlineM 10 leadershipDeadline "Wait for chain to surpass all expected leadership slots" $ do
      (someLeaderSlots, someNotLeaderSlots) <- getRelevantSlots testPoolStdOutFp (minimum expectedLeadershipSlotNumbers)
      if L.null someLeaderSlots
        then H.failure
        else do
          maxActualSlot <- H.noteShow $ maximum someLeaderSlots
          H.assert $ maxActualSlot >= maxSlotExpected
      pure (someLeaderSlots, someNotLeaderSlots)

    H.noteShow_ expectedLeadershipSlotNumbers
    H.noteShow_ leaderSlots
    H.noteShow_ notLeaderSlots

    -- Double check that we've seen all slots
    H.noteShow_ ("Slots not seen as TraceNodeIsLeader nor TraceNodeNotLeader" :: Text)
    ([minimum expectedLeadershipSlotNumbers .. maxSlotExpected] \\ leaderSlots) \\ notLeaderSlots === []

    -- As there are no BFT nodes, the next leadership schedule should match slots assigned exactly
    H.noteShow_ (expectedLeadershipSlotNumbers \\ leaderSlots)
    H.assert $ L.null (expectedLeadershipSlotNumbers \\ leaderSlots)
-}
