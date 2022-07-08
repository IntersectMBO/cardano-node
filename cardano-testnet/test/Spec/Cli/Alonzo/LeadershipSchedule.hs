{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use let" -}

module Spec.Cli.Alonzo.LeadershipSchedule
  ( hprop_leadershipSchedule
  ) where

import           Cardano.Api (AlonzoEra, SerialiseAddress (serialiseAddress), UTxO (UTxO))
import qualified Cardano.Api as Api
import           Cardano.Api.Shelley (PoolId)
import           Cardano.CLI.Shelley.Output (QueryTipLocalStateOutput (mEpoch))
import           Cardano.CLI.Shelley.Run.Query (DelegationsAndRewards, mergeDelegsAndRewards)
import           Control.Monad (void)
import qualified Data.Aeson.Types as J
import           Data.List ((\\))
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Data.Monoid (Last (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time.Clock as DTC
import           GHC.Stack (callStack)
import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import           Prelude
import qualified System.Directory as IO
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))
import qualified System.Info as SYS
import qualified Test.Assert as H
import qualified Test.Base as H
import qualified Test.Process as H
import qualified Test.Runtime as TR
import           Test.Runtime (LeadershipSlot (..))
import qualified Testnet.Cardano as TC
import           Testnet.Cardano (TestnetOptions (..), TestnetRuntime (..))
import qualified Testnet.Conf as H
import           Testnet.Utils (waitUntilEpoch)

hprop_leadershipSchedule :: Property
hprop_leadershipSchedule = H.integration . H.runFinallies . H.workspace "alonzo" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $
    H.mkConf (H.ProjectBase base) (H.YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  fastTestnetOptions <- pure TC.defaultTestnetOptions
    { epochLength = 500
    , slotLength = 0.01
    , activeSlotsCoeff = 0.1
    , nodeLoggingFormat = TR.NodeLoggingFormatAsJson
    }
  tr@TC.TestnetRuntime
    { testnetMagic
    , poolNodes
    } <- TC.testnet fastTestnetOptions conf

  poolNode1 <- H.headM poolNodes

  env <- H.evalIO getEnvironment

  execConfig <- H.noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName $ head $ TR.bftSprockets tr)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }

  -- First we note all the relevant files
  H.note_ base
  work <- H.note tempAbsPath

  -- We get our UTxOs from here
  utxoVKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
  utxoVKeyFile2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2.vkey"
  utxoSKeyFile2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2.skey"

  utxoAddr <- H.execCli
    [ "address", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--payment-verification-key-file", utxoVKeyFile
    ]

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  H.cat $ work </> "utxo-1.json"

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo1Json
  txin <- H.noteShow $ head $ Map.keys utxo1

  -- Staking keys
  utxoStakingVkey2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2-stake.vkey"
  utxoStakingSkey2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2-stake.skey"

  utxoaddrwithstaking <- H.execCli
    [ "address", "build"
    , "--payment-verification-key-file", utxoVKeyFile2
    , "--stake-verification-key-file", utxoStakingVkey2
    , "--testnet-magic", show @Int testnetMagic
    ]

  utxostakingaddr <- filter (/= '\n') <$> H.execCli
    [ "stake-address", "build"
    , "--stake-verification-key-file", utxoStakingVkey2
    , "--testnet-magic", show @Int testnetMagic
    ]

  -- Stake pool related
  H.createDirectoryIfMissing $ tempAbsPath </> "addresses"
  poolownerstakekey <- H.note $ tempAbsPath </> "addresses/pool-owner1-stake.vkey"
  poolownerverkey <- H.note $ tempAbsPath </> "addresses/pool-owner1.vkey"
  poolownerstakeaddr <- filter (/= '\n') <$> H.execCli
    [ "stake-address", "build"
    , "--stake-verification-key-file", poolownerstakekey
    , "--testnet-magic", show @Int testnetMagic
    ]

  poolowneraddresswstakecred <- H.execCli
    [ "address", "build"
    , "--payment-verification-key-file", poolownerverkey
    , "--stake-verification-key-file", poolownerstakekey
    , "--testnet-magic", show @Int testnetMagic
    ]

  poolcoldVkey <- H.note $ tempAbsPath </> "node-pool1/shelley/operator.vkey"
  poolcoldSkey <- H.note $ tempAbsPath </> "node-pool1/shelley/operator.skey"

  stakePoolId <- filter ( /= '\n') <$> H.execCli
    [ "stake-pool", "id"
    , "--cold-verification-key-file", poolcoldVkey
    ]

  -- REGISTER PLEDGER POOL

  -- Create pledger registration certificate
  void $ H.execCli
    [ "stake-address", "registration-certificate"
    , "--stake-verification-key-file", poolownerstakekey
    , "--out-file", work </> "pledger.regcert"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address",  utxoAddr
    , "--tx-in", T.unpack $ Api.renderTxIn txin
    , "--tx-out", poolowneraddresswstakecred <> "+" <> show @Int 5000000
    , "--tx-out", utxoaddrwithstaking <> "+" <> show @Int 5000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "pledger.regcert"
    , "--out-file", work </> "pledge-registration-cert.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "pledge-registration-cert.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "pledge-registration-cert.tx"
    ]

  H.note_ "Submitting pool owner/pledge stake registration cert and funding stake pool owner address..."

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "pledge-registration-cert.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  -- Things take long on non-linux machines
  if H.isLinux
  then H.threadDelay 5000000
  else H.threadDelay 10000000

  -- Check to see if pledge's stake address was registered

  void $ H.execCli' execConfig
    [ "query", "stake-address-info"
    , "--address", poolownerstakeaddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pledgeownerregistration.json"
    ]

  pledgerStakeInfo <- H.leftFailM . H.readJsonFile $ work </> "pledgeownerregistration.json"
  delegsAndRewardsMap <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards pledgerStakeInfo
  let delegsAndRewards = mergeDelegsAndRewards delegsAndRewardsMap

  length delegsAndRewards === 1

  let (pledgerSAddr, _rewards, _poolId) = head delegsAndRewards

  -- Pledger and owner are and can be the same
  T.unpack (serialiseAddress pledgerSAddr) === poolownerstakeaddr

  H.note_ $ "Register staking key: " <> show utxoStakingVkey2

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoaddrwithstaking
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-addr-with-staking-1.json"
    ]

  H.cat $ work </> "utxo-addr-with-staking-1.json"

  utxoWithStaking1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-addr-with-staking-1.json"
  UTxO utxoWithStaking1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxoWithStaking1Json
  txinForStakeReg <- H.noteShow $ head $ Map.keys utxoWithStaking1

  void $ H.execCli
    [ "stake-address", "registration-certificate"
    , "--stake-verification-key-file", utxoStakingVkey2
    , "--out-file", work </> "stakekey.regcert"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoaddrwithstaking
    , "--tx-in", T.unpack (Api.renderTxIn txinForStakeReg)
    , "--tx-out", utxoaddrwithstaking <> "+" <> show @Int 1000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "stakekey.regcert"
    , "--out-file", work </> "key-registration-cert.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "key-registration-cert.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoStakingSkey2
    , "--signing-key-file", utxoSKeyFile2
    , "--out-file", work </> "key-registration-cert.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "key-registration-cert.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.note_ $ "Check to see if " <> utxoStakingVkey2 <> " was registered..."
  H.threadDelay 10000000

  void $ H.execCli' execConfig
    [ "query", "stake-address-info"
    , "--address", utxostakingaddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "stake-address-info-utxo-staking-vkey-2.json"
    ]

  userStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "stake-address-info-utxo-staking-vkey-2.json"
  delegsAndRewardsMapUser <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards userStakeAddrInfoJSON
  let delegsAndRewardsUser = mergeDelegsAndRewards delegsAndRewardsMapUser
      userStakeAddrInfo = filter (\(sAddr,_,_) -> utxostakingaddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsUser
      (userSAddr, _rewards, _poolId) = head userStakeAddrInfo


  H.note_ $ "Check staking key: " <> show utxoStakingVkey2 <> " was registered"
  T.unpack (serialiseAddress userSAddr) === utxostakingaddr

  H.note_  "Get updated UTxO"

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo2Json
  txin2 <- H.noteShow $ head $ Map.keys utxo2

  H.note_ "Create delegation certificate of pledger"

  void $ H.execCli
    [ "stake-address", "delegation-certificate"
    , "--stake-verification-key-file", poolownerstakekey
    , "--cold-verification-key-file", poolcoldVkey
    , "--out-file", work </> "pledger.delegcert"
    ]

  H.note_ "Register stake pool and delegate pledger to stake pool in a single tx"

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address",  utxoAddr
    , "--tx-in", T.unpack $ Api.renderTxIn txin2
    , "--tx-out", utxoAddr <> "+" <> show @Int 10000000
    , "--witness-override", show @Int 3
    , "--certificate-file", tempAbsPath </> "node-pool1/registration.cert"
    , "--certificate-file", work </> "pledger.delegcert"
    , "--out-file", work </> "register-stake-pool.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "register-stake-pool.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", poolcoldSkey
    , "--signing-key-file", tempAbsPath </> "node-pool1/owner.skey"
    , "--out-file", work </> "register-stake-pool.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "register-stake-pool.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  if H.isLinux
  then H.threadDelay 5000000
  else H.threadDelay 20000000

  void $ H.execCli' execConfig
    [ "query", "stake-pools"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "current-registered.pools.json"
    ]

  currRegPools <- H.leftFailM . H.readJsonFile $ work </> "current-registered.pools.json"
  poolIds <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(Set PoolId) currRegPools
  poolId <- H.noteShow $ head $ Set.toList poolIds

  H.note_ "Check stake pool was successfully registered"
  T.unpack (Api.serialiseToBech32 poolId) === stakePoolId

  H.note_ "Check pledge was successfully delegated"
  void $ H.execCli' execConfig
    [ "query", "stake-address-info"
    , "--address", poolownerstakeaddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pledge-stake-address-info.json"
    ]

  pledgeStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "pledge-stake-address-info.json"
  delegsAndRewardsMapPledge <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards pledgeStakeAddrInfoJSON
  let delegsAndRewardsPledge = mergeDelegsAndRewards delegsAndRewardsMapPledge
      pledgeStakeAddrInfo = filter (\(sAddr,_,_) -> poolownerstakeaddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsPledge
      (pledgeSAddr, _rewards, pledgerDelegPoolId) = head pledgeStakeAddrInfo

  H.note_ "Check pledge has been delegated to pool"
  case pledgerDelegPoolId of
    Nothing               -> H.failMessage callStack "Pledge was not delegated to pool"
    Just pledgerDelagator -> T.unpack (Api.serialiseToBech32 pledgerDelagator) === stakePoolId
  T.unpack (serialiseAddress pledgeSAddr) === poolownerstakeaddr

  H.note_ "Get updated UTxO"

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-3.json"
    ]

  H.threadDelay 10000000

  H.note_ "Get updated UTxO"

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-4.json"
    ]

  H.cat $ work </> "utxo-4.json"

  -- Wait 5 seconds
  H.threadDelay 5000000

  H.note_ "Wait for rewards to be paid out. This will be current epoch + 4"

  void $ H.execCli' execConfig
    [ "query", "tip"
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

  let rewardsEpoch = currEpoch + 4

  waitedEpoch <- waitUntilEpoch
    (work </> "current-tip.json")
    testnetMagic
    execConfig
    rewardsEpoch

  H.note_ "Check we have reached 4 epochs ahead"
  waitedEpoch === rewardsEpoch

  void $ H.execCli' execConfig
    [ "query", "tip"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "current-tip-2.json"
    ]

  tip2JSON <- H.leftFailM . H.readJsonFile $ work </> "current-tip-2.json"
  tip2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryTipLocalStateOutput tip2JSON

  currEpoch2 <- case mEpoch tip2 of
    Nothing -> H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
    Just currEpoch2 -> return currEpoch2

  H.note_ $ "Current Epoch: " <> show currEpoch2

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  H.assertByDeadlineMCustom "stdout does not contain \"until genesis start time\"" deadline $ do
    H.threadDelay 1000000
    void $ H.execCli' execConfig
      [ "query", "tip"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "current-tip-3.json"
      ]

    tip3JSON <- H.leftFailM . H.readJsonFile $ work </> "current-tip-3.json"
    tip3 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryTipLocalStateOutput tip3JSON

    currEpoch3 <- case mEpoch tip3 of
      Nothing -> H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch3 -> return currEpoch3

    H.note_ $ "Current Epoch: " <> show currEpoch3
    return (currEpoch3 > currEpoch2 + 1)

  ledgerStateJson <- H.execCli' execConfig
    [ "query", "ledger-state"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.note_ ledgerStateJson

  H.note_ "Done"

  let poolVrfSkey = TR.poolNodeKeysVrfSkey $ TR.poolNodeKeys poolNode1
  scheduleFile <- H.noteTempFile tempAbsPath "schedule.log"

  void $ H.execCli' execConfig
    [ "query", "leadership-schedule"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis", TC.shelleyGenesisFile tr
    , "--stake-pool-id", stakePoolId
    , "--vrf-signing-key-file", poolVrfSkey
    , "--out-file", scheduleFile
    , "--current"
    ]

  scheduleJson <- H.leftFailM $ H.readJsonFile scheduleFile

  expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) scheduleJson

  H.assert $ not (L.null expectedLeadershipSlotNumbers)

  leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

  H.assertByDeadlineMCustom "Leader schedule is correct" leadershipDeadline $ do
    leaderSlots <- H.getRelevantLeaderSlots (TR.poolNodeStdout poolNode1) (minimum expectedLeadershipSlotNumbers)
    maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers
    maxActualSlot <- H.noteShow $ maximum leaderSlots
    return $ maxActualSlot >= maxSlotExpected

  leaderSlots <- H.getRelevantLeaderSlots (TR.poolNodeStdout poolNode1) (minimum expectedLeadershipSlotNumbers)

  -- It's possible for some slots to not be assigned in TPraos when BFT nodes are running.
  -- TODO Remove BFT nodes from testnet and assert the schedule is equal to actual slots
  H.assert $ L.length (expectedLeadershipSlotNumbers \\ leaderSlots) <= 1
