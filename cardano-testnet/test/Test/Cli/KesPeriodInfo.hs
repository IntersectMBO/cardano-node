{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.KesPeriodInfo
  ( hprop_kes_period_info
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley (PoolId)
import           Control.Monad (void)
import           Data.Monoid (Last (..))
import           Data.Set (Set)
import           GHC.Stack (callStack)
import           Hedgehog (Property, (===))
import           Prelude
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))

import           Cardano.CLI.Shelley.Output
import           Cardano.CLI.Shelley.Run.Query

import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Info as SYS

import           Test.Misc
import           Cardano.Testnet
import           Testnet.Util.Process
import           Testnet.Util.Runtime

hprop_kes_period_info :: Property
hprop_kes_period_info = integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  base <- H.note =<< H.evalIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate
    <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"

  conf@Conf { tempBaseAbsPath, tempAbsPath }
    <- H.noteShowM $ mkConf (ProjectBase base) (YamlFilePath configurationTemplate)
                              tempAbsBasePath' Nothing

  let fastTestnetOptions = CardanoOnlyTestnetOptions $ cardanoDefaultTestnetOptions
                             { cardanoBftNodeOptions = replicate 1 cardanoDefaultTestnetNodeOptions
                             , cardanoEpochLength = 500
                             , cardanoSlotLength = 0.02
                             , cardanoActiveSlotsCoeff = 0.1
                             }
  runTime@TestnetRuntime { testnetMagic } <- testnet fastTestnetOptions conf
  let sprockets = bftSprockets runTime
  env <- H.evalIO getEnvironment

  execConfig <- H.noteShow H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName (head sprockets))
          ]
          -- The environment must be passed onto child process on Windows in order to
          -- successfully start that process.
          <> env
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  -- First we note all the relevant files
  work <- H.note tempAbsPath

  -- We get our UTxOs from here
  utxoVKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
  utxoVKeyFile2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2.vkey"
  utxoSKeyFile2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2.skey"

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
  UTxO utxo1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo1Json
  txin <- H.noteShow =<< H.headM (Map.keys utxo1)

  -- Staking keys
  utxoStakingVkey2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2-stake.vkey"
  utxoStakingSkey2 <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo2-stake.skey"

  utxoaddrwithstaking <- execCli [ "address", "build"
                                   , "--payment-verification-key-file", utxoVKeyFile2
                                   , "--stake-verification-key-file", utxoStakingVkey2
                                   , "--testnet-magic", show @Int testnetMagic
                                   ]

  utxostakingaddr <- filter (/= '\n')
                       <$> execCli
                             [ "stake-address", "build"
                             , "--stake-verification-key-file", utxoStakingVkey2
                             , "--testnet-magic", show @Int testnetMagic
                             ]


  -- Stake pool related
  poolownerstakekey <- H.note $ tempAbsPath </> "addresses/pool-owner1-stake.vkey"
  poolownerverkey <- H.note $ tempAbsPath </> "addresses/pool-owner1.vkey"
  poolownerstakeaddr <- filter (/= '\n')
                          <$> execCli
                                [ "stake-address", "build"
                                , "--stake-verification-key-file", poolownerstakekey
                                , "--testnet-magic", show @Int testnetMagic
                                ]

  poolowneraddresswstakecred <- execCli [ "address", "build"
                                          , "--payment-verification-key-file", poolownerverkey
                                          , "--stake-verification-key-file",  poolownerstakekey
                                          , "--testnet-magic", show @Int testnetMagic
                                          ]
  poolcoldVkey <- H.note $ tempAbsPath </> "node-pool1/shelley/operator.vkey"
  poolcoldSkey <- H.note $ tempAbsPath </> "node-pool1/shelley/operator.skey"

  stakePoolId <- filter ( /= '\n') <$>
                   execCli [ "stake-pool", "id"
                             , "--cold-verification-key-file", poolcoldVkey
                             ]

  -- REGISTER PLEDGER POOL

  -- Create pledger registration certificate
  void $ execCli
            [ "stake-address", "registration-certificate"
            , "--stake-verification-key-file", poolownerstakekey
            , "--out-file", work </> "pledger.regcert"
            ]

  void $ execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address",  utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin
    , "--tx-out", poolowneraddresswstakecred <> "+" <> show @Int 5000000
    , "--tx-out", utxoaddrwithstaking <> "+" <> show @Int 5000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "pledger.regcert"
    , "--out-file", work </> "pledge-registration-cert.txbody"
    ]

  void $ execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "pledge-registration-cert.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "pledge-registration-cert.tx"
    ]

  H.note_ "Submitting pool owner/pledge stake registration cert and funding stake pool owner address..."

  void $ execCli' execConfig
               [ "transaction", "submit"
               , "--tx-file", work </> "pledge-registration-cert.tx"
               , "--testnet-magic", show @Int testnetMagic
               ]

  delegsAndRewards <- H.byDurationM 3 12 "Pledge's stake address was not registered" $ do
    -- Check to see if pledge's stake address was registered

    void $ execCli' execConfig
      [ "query", "stake-address-info"
      , "--address", poolownerstakeaddr
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "pledgeownerregistration.json"
      ]

    pledgerStakeInfo <- H.leftFailM . H.readJsonFile $ work </> "pledgeownerregistration.json"
    delegsAndRewardsMap <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards pledgerStakeInfo
    delegsAndRewards <- H.noteShow $ mergeDelegsAndRewards delegsAndRewardsMap

    length delegsAndRewards === 1
    return delegsAndRewards

  (pledgerSAddr, _rewards, _poolId) <- H.headM delegsAndRewards

  -- Pledger and owner are and can be the same
  T.unpack (serialiseAddress pledgerSAddr) === poolownerstakeaddr

  H.note_ $ "Register staking key: " <> show utxoStakingVkey2

  void $ execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoaddrwithstaking
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-addr-with-staking-1.json"
      ]

  H.cat $ work </> "utxo-addr-with-staking-1.json"

  utxoWithStaking1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-addr-with-staking-1.json"
  UTxO utxoWithStaking1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxoWithStaking1Json
  txinForStakeReg <- H.noteShow =<< H.headM (Map.keys utxoWithStaking1)

  void $ execCli [ "stake-address", "registration-certificate"
                   , "--stake-verification-key-file", utxoStakingVkey2
                   , "--out-file", work </> "stakekey.regcert"
                   ]

  void $ execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoaddrwithstaking
    , "--tx-in", T.unpack (renderTxIn txinForStakeReg)
    , "--tx-out", utxoaddrwithstaking <> "+" <> show @Int 1000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "stakekey.regcert"
    , "--out-file", work </> "key-registration-cert.txbody"
    ]

  void $ execCli [ "transaction", "sign"
                   , "--tx-body-file", work </> "key-registration-cert.txbody"
                   , "--testnet-magic", show @Int testnetMagic
                   , "--signing-key-file", utxoStakingSkey2
                   , "--signing-key-file", utxoSKeyFile2
                   , "--out-file", work </> "key-registration-cert.tx"
                   ]


  void $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "key-registration-cert.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.note_ $ "Check to see if " <> utxoStakingVkey2 <> " was registered..."

  userSAddr <- H.byDurationM 3 12 "Failed to query stake address info" $ do
    void $ execCli' execConfig
      [ "query", "stake-address-info"
      , "--address", utxostakingaddr
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "stake-address-info-utxo-staking-vkey-2.json"
      ]

    userStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "stake-address-info-utxo-staking-vkey-2.json"
    delegsAndRewardsMapUser <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards userStakeAddrInfoJSON
    delegsAndRewardsUser <- H.noteShow $ mergeDelegsAndRewards delegsAndRewardsMapUser
    userStakeAddrInfo <- H.noteShow $ filter (\(sAddr,_,_) -> utxostakingaddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsUser
    (userSAddr, _rewards, _poolId) <- H.headM userStakeAddrInfo
    return userSAddr

  H.note_ $ "Check staking key: " <> show utxoStakingVkey2 <> " was registered"
  T.unpack (serialiseAddress userSAddr) === utxostakingaddr

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
  UTxO utxo2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo2Json
  txin2 <- H.noteShow =<< H.headM (Map.keys utxo2)

  H.note_ "Create delegation certificate of pledger"

  void $ execCli
    [ "stake-address", "delegation-certificate"
    , "--stake-verification-key-file", poolownerstakekey
    , "--cold-verification-key-file", poolcoldVkey
    , "--out-file", work </> "pledger.delegcert"
    ]

  H.note_ "Register stake pool and delegate pledger to stake pool in a single tx"

  void $ execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address",  utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin2
    , "--tx-out", utxoAddr <> "+" <> show @Int 10000000
    , "--witness-override", show @Int 3
    , "--certificate-file", tempAbsPath </> "node-pool1/registration.cert"
    , "--certificate-file", work </> "pledger.delegcert"
    , "--out-file", work </> "register-stake-pool.txbody"
    ]

  void $ execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "register-stake-pool.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", poolcoldSkey
    , "--signing-key-file", tempAbsPath </> "node-pool1/owner.skey"
    , "--out-file", work </> "register-stake-pool.tx"
    ]

  void $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "register-stake-pool.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.byDurationM 3 12 "Stake pool was not registered" $ do
    void $ execCli' execConfig
      [ "query", "stake-pools"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "current-registered.pools.json"
      ]

    currRegPools <- H.leftFailM . H.readJsonFile $ work </> "current-registered.pools.json"
    poolIds <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(Set PoolId) currRegPools
    poolId <- H.noteShow =<< H.headM (Set.toList poolIds)

    H.note_ "Check stake pool was successfully registered"
    T.unpack (serialiseToBech32 poolId) === stakePoolId

  H.note_ "Check pledge was successfully delegated"
  void $ execCli' execConfig
      [ "query", "stake-address-info"
      , "--address", poolownerstakeaddr
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "pledge-stake-address-info.json"
      ]

  pledgeStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "pledge-stake-address-info.json"
  delegsAndRewardsMapPledge <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards pledgeStakeAddrInfoJSON
  delegsAndRewardsPledge <- H.noteShow $ mergeDelegsAndRewards delegsAndRewardsMapPledge
  pledgeStakeAddrInfo <- H.noteShow $ filter (\(sAddr,_,_) -> poolownerstakeaddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsPledge

  (pledgeSAddr, _rewards, pledgerDelegPoolId) <- H.headM pledgeStakeAddrInfo

  H.note_ "Check pledge has been delegated to pool"
  case pledgerDelegPoolId of
    Nothing -> H.failMessage callStack "Pledge was not delegated to pool"
    Just pledgerDelagator ->  T.unpack (serialiseToBech32 pledgerDelagator) === stakePoolId
  T.unpack (serialiseAddress pledgeSAddr) === poolownerstakeaddr

  H.note_ "We have a fully functioning stake pool at this point."

  -- TODO: Linking directly to the node certificate is fragile
  nodeOperationalCertFp <- H.note $ tempAbsPath </> "node-pool1/shelley/node.cert"

  void $ execCli' execConfig
    [ "query", "kes-period-info"
    , "--testnet-magic", show @Int testnetMagic
    , "--op-cert-file", nodeOperationalCertFp
    , "--out-file", work </> "kes-period-info-expected-success.json"
    ]

  kesPeriodInfoExpectedSuccess <- H.leftFailM . H.readJsonFile $ work </> "kes-period-info-expected-success.json"
  kesPeriodOutputSuccess <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryKesPeriodInfoOutput kesPeriodInfoExpectedSuccess

  -- We check if the operational certificate is valid for the current KES period
  prop_op_cert_valid_kes_period nodeOperationalCertFp kesPeriodOutputSuccess

  H.cat $ work </> "kes-period-info-expected-success.json"


  H.note_ "Get updated UTxO"

  void $ execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-3.json"
      ]

  H.cat $ work </> "utxo-3.json"

  utxo3Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-3.json"
  UTxO utxo3 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo3Json
  _txin3 <- H.noteShow . head $ Map.keys utxo3


  H.note_ "Wait for the node to mint blocks. This will be in the following epoch so lets wait\
          \ until the END of the following epoch."

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

  H.note_ "Check to see if the node has minted blocks. This confirms that the operational\
           \ certificate is valid"

  -- TODO: Linking to the node log file like this is fragile.
  spoLogFile <- H.note $ tempAbsPath </> "logs/node-pool1.stdout.log"
  prop_node_minted_block spoLogFile
