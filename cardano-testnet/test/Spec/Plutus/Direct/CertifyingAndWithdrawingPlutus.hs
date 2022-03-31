{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Plutus.Direct.CertifyingAndWithdrawingPlutus
  ( hprop_plutus_certifying_withdrawing
  ) where


import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley
import           Control.Monad (void)
import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import           Data.Monoid (Last (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.Stack (callStack)
import qualified System.Directory as IO
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))
import           System.Info (os)

import           Cardano.CLI.Shelley.Output
import           Cardano.CLI.Shelley.Run.Query

import           Hedgehog (Property, (/==), (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Test.Base as H
import qualified Test.Process as H
import           Testnet.Cardano (TestnetOptions (..), TestnetRuntime (..), defaultTestnetOptions,
                   testnet)
import qualified Testnet.Cardano as TC
import qualified Testnet.Conf as H
import           Testnet.Utils (waitUntilEpoch)
import qualified System.Info as SYS


{-
The aim is to test a Plutus certifying and rewarding script. Certifying in the sense of validating a certificate
e.g in this case a delegating certificate and rewarding in the sense of validating a rewards withdrawal.
In this test, we delegate a Plutus script staking address to our stake pool. We must:
  1. Create a stake pool
  2. Delegate our Plutus script address to said staking pool
  3. Withdraw our rewards from our Plutus script staking address.
-}

isLinux :: Bool
isLinux = os == "linux"

hprop_plutus_certifying_withdrawing :: Property
hprop_plutus_certifying_withdrawing = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $
    H.mkConf (H.ProjectBase base) (H.YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  let fastTestnetOptions = defaultTestnetOptions
                             { epochLength = 500
                             , slotLength = 0.01
                             , activeSlotsCoeff = 0.1
                             }
  TC.TestnetRuntime { bftSprockets, testnetMagic } <- testnet fastTestnetOptions conf

  env <- H.evalIO getEnvironment

  execConfig <- H.noteShow H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName (head bftSprockets))
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

  utxoaddrwithstaking <- H.execCli [ "address", "build"
                                   , "--payment-verification-key-file", utxoVKeyFile2
                                   , "--stake-verification-key-file", utxoStakingVkey2
                                   , "--testnet-magic", show @Int testnetMagic
                                   ]

  utxostakingaddr <- filter (/= '\n')
                       <$> H.execCli
                             [ "stake-address", "build"
                             , "--stake-verification-key-file", utxoStakingVkey2
                             , "--testnet-magic", show @Int testnetMagic
                             ]

  -- Plutus related
  plutusStakingScript <- H.note $ base </> "scripts/plutus/scripts/guess-42-stake.plutus"
  plutusStakingScriptRedeemer <- H.note $ base </> "scripts/plutus/data/42.redeemer"
  scriptPaymentAddressWithStaking <- H.execCli [ "address", "build"
                                               , "--payment-verification-key-file", utxoVKeyFile
                                               , "--stake-script-file",  plutusStakingScript
                                               , "--testnet-magic", show @Int testnetMagic
                                               ]
  plutusStakingAddr <- filter (/= '\n') <$>
                         H.execCli [ "stake-address", "build"
                                   , "--testnet-magic", show @Int testnetMagic
                                   , "--stake-script-file",  plutusStakingScript
                                   ]
  -- Stake pool related
  poolownerstakekey <- H.note $ tempAbsPath </> "addresses/pool-owner1-stake.vkey"
  poolownerverkey <- H.note $ tempAbsPath </> "addresses/pool-owner1.vkey"
  poolownerstakeaddr <- filter (/= '\n')
                          <$> H.execCli
                                [ "stake-address", "build"
                                , "--stake-verification-key-file", poolownerstakekey
                                , "--testnet-magic", show @Int testnetMagic
                                ]

  poolowneraddresswstakecred <- H.execCli [ "address", "build"
                                          , "--payment-verification-key-file", poolownerverkey
                                          , "--stake-verification-key-file",  poolownerstakekey
                                          , "--testnet-magic", show @Int testnetMagic
                                          ]
  poolcoldVkey <- H.note $ tempAbsPath </> "node-pool1/shelley/operator.vkey"
  poolcoldSkey <- H.note $ tempAbsPath </> "node-pool1/shelley/operator.skey"

  stakePoolId <- filter ( /= '\n') <$>
                   H.execCli [ "stake-pool", "id"
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
    , "--tx-in", T.unpack $ renderTxIn txin
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Int 5000000
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
  if isLinux
  then H.threadDelay 5000000
  else H.threadDelay 10000000

  -- Check to see if pledge's stake address was registered

  void $ H.execCli' execConfig
    [ "query",  "stake-address-info"
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

  void $ H.execCli [ "stake-address", "registration-certificate"
                   , "--stake-verification-key-file", utxoStakingVkey2
                   , "--out-file", work </> "stakekey.regcert"
                   ]

  void $ H.execCli' execConfig
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

  void $ H.execCli [ "transaction", "sign"
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
    , "--tx-in", T.unpack $ renderTxIn txin2
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Int 5000000
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

  if isLinux
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
  T.unpack (serialiseToBech32 poolId) === stakePoolId

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
    Nothing -> H.failMessage callStack "Pledge was not delegated to pool"
    Just pledgerDelagator ->  T.unpack (serialiseToBech32 pledgerDelagator) === stakePoolId
  T.unpack (serialiseAddress pledgeSAddr) === poolownerstakeaddr

  H.note_ "We have a fully functioning stake pool at this point. We now want to test Plutus staking script withdrawals."

  H.note_ "We now create the Plutus script staking registration certificate"

  H.note_ "Get updated UTxO"

  void $ H.execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-3.json"
      ]

  H.cat $ work </> "utxo-3.json"

  utxo3Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-3.json"
  UTxO utxo3 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo3Json
  txin3 <- H.noteShow . head $ Map.keys utxo3

  void $ H.execCli
    [ "stake-address", "registration-certificate"
    , "--stake-script-file", plutusStakingScript
    , "--out-file", work </> "script.regcert"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin3
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Int 5000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "script.regcert"
    , "--out-file", work </> "register-plutus-staking-script.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "register-plutus-staking-script.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "register-plutus-staking-script.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "register-plutus-staking-script.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 10000000

  H.note_ "Check if Plutus staking script address was registered"

  void $ H.execCli' execConfig
      [ "query", "stake-address-info"
      , "--address", plutusStakingAddr
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "pledge-stake-address-info.json"
      ]

  plutusStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "pledge-stake-address-info.json"
  delegsAndRewardsMapPlutus <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards plutusStakeAddrInfoJSON
  let delegsAndRewardsPlutus = mergeDelegsAndRewards delegsAndRewardsMapPlutus
      plutusStakeAddrInfo = filter (\(sAddr,_,_) -> plutusStakingAddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsPlutus
      (plutusSAddr, _rewards, _poolId) = head plutusStakeAddrInfo

  H.note_ "Check if Plutus staking script has been registered"
  T.unpack (serialiseAddress plutusSAddr) === plutusStakingAddr

  H.note_ "Create delegation certificate for Plutus staking script to stake pool"

  void $ H.execCli
    [ "stake-address", "delegation-certificate"
    , "--stake-script-file", plutusStakingScript
    , "--cold-verification-key-file", poolcoldVkey
    , "--out-file", work </> "plutus-script.delegcert"
    ]

  H.note_ "Get updated UTxO"

  void $ H.execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-4.json"
      ]

  H.cat $ work </> "utxo-4.json"

  utxo4Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-4.json"
  UTxO utxo4 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo4Json
  txin4 <- H.noteShow . head $ Map.keys utxo4
  txinCollateral1 <- H.noteShow $ Map.keys utxo4 !! 1

  H.note_ "Delegate Plutus staking script to stake pool"

  void $ H.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin4
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral1
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Int 5000000
    , "--witness-override", show @Int 3
    , "--certificate-file", work </> "plutus-script.delegcert"
    , "--certificate-script-file", plutusStakingScript
    , "--certificate-redeemer-file", plutusStakingScriptRedeemer
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "delegate-staking-script.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "delegate-staking-script.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "delegate-staking-script.tx"
    ]

  void $ H.execCli' execConfig
               [ "transaction", "submit"
               , "--tx-file", work </> "delegate-staking-script.tx"
               , "--testnet-magic", show @Int testnetMagic
               ]

  -- Wait 5 seconds
  H.threadDelay 5000000

  H.note_ "Check to see if staking script was delegated"

  void $ H.execCli' execConfig
    [ "query",  "stake-address-info"
    , "--address", plutusStakingAddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "plutus-staking-script-delegation.json"
    ]

  stakingScriptAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "plutus-staking-script-delegation.json"
  delegsAndRewardsMapStakingScript <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards stakingScriptAddrInfoJSON
  let delegsAndRewardsStakingScript = mergeDelegsAndRewards delegsAndRewardsMapStakingScript
      stakingScriptAddrInfo = filter (\(sAddr,_,_) -> plutusStakingAddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsStakingScript
      (_stakingSAddr, _rewards, poolIdPlutusDeleg) = head stakingScriptAddrInfo

  H.note_ $ "Check plutus staking script: " <> (work </> "plutus-staking-script-delegation.json") <> " was delegated"
  case poolIdPlutusDeleg of
    Nothing -> H.failMessage callStack "Plutus script was not delegated to stake pool"
    Just plutusDelegPoolId ->
      T.unpack (serialiseToBech32 plutusDelegPoolId) === stakePoolId


  H.note_ "Checking plutus staking script has ada at its corresponding payment address"

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", scriptPaymentAddressWithStaking
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-plutus-staking-payment-address.json"
    ]

  H.cat $ work </> "utxo-plutus-staking-payment-address.json"

  utxoPlutusPaymentAddrJson <- H.leftFailM . H.readJsonFile $ work </> "utxo-plutus-staking-payment-address.json"
  UTxO utxoPlutus <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxoPlutusPaymentAddrJson

  utxoPlutus /== mempty

  H.note_ "Wait for rewards to be paid out. This will be current epoch + 4"

  void $ H.execCli' execConfig
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

  let rewardsEpoch = currEpoch + 4
  waitedEpoch <- waitUntilEpoch
                   (work </> "current-tip.json")
                   testnetMagic
                   execConfig
                   rewardsEpoch

  H.note_ "Check we have reached 4 epochs ahead"
  waitedEpoch === rewardsEpoch


  void $ H.execCli' execConfig
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

  H.note_ "Check rewards have been distributed to Plutus script staking address"

  void$ H.execCli' execConfig
    [ "query", "ledger-state"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "ledger-state.json"
    ]

  void $ H.execCli' execConfig
    [ "query",  "stake-address-info"
    , "--address", plutusStakingAddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "plutus-staking-script-delegation-rewards.json"
    ]

  stakingRewardsJSON <- H.leftFailM . H.readJsonFile $ work </> "plutus-staking-script-delegation-rewards.json"
  delegsAndRewardsMapScriptRewards <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards stakingRewardsJSON
  let delegsAndRewardsScriptRewards = mergeDelegsAndRewards delegsAndRewardsMapScriptRewards
      stakingScriptRewardsAddrInfo = filter (\(sAddr,_,_) -> plutusStakingAddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsScriptRewards
      (_, scriptRewards, _) = head stakingScriptRewardsAddrInfo

  pr@(Lovelace plutusRewards) <-
    case scriptRewards of
      Nothing -> H.failMessage callStack "Plutus staking script has no rewards"
      Just rwds -> H.assert (rwds > 0) >> return rwds

  H.note_ $ "We now withdraw the rewards from our Plutus staking address: " <> show @Integer plutusRewards

  H.note_ "Get updated UTxO"

  void $ H.execCli' execConfig
      [ "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-5.json"
      ]

  H.cat $ work </> "utxo-5.json"

  utxo5Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-5.json"
  UTxO utxo5 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxo5Json
  txin5 <- H.noteShow . head $ Map.keys utxo5
  txinCollateral2 <- H.noteShow $ Map.keys utxo5 !! 1

  let minrequtxo = 999978
  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack $ renderTxIn txin5
    , "--tx-in-collateral", T.unpack $ renderTxIn txinCollateral2
    , "--tx-out", scriptPaymentAddressWithStaking <> "+" <> show @Integer (plutusRewards + minrequtxo)
    , "--withdrawal", plutusStakingAddr <> "+" <> show @Integer plutusRewards
    , "--withdrawal-script-file", plutusStakingScript
    , "--withdrawal-redeemer-file", plutusStakingScriptRedeemer
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "staking-script-withdrawal.txbody"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "staking-script-withdrawal.txbody"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "staking-script-withdrawal.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "staking-script-withdrawal.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  -- Things take long on non-linux machines
  if isLinux
  then H.threadDelay 5000000
  else H.threadDelay 10000000

  H.note_ "Check UTxO at script staking address to see if withdrawal was successful"

  void $ H.execCli' execConfig
        [ "query", "utxo"
        , "--address", scriptPaymentAddressWithStaking
        , "--cardano-mode"
        , "--testnet-magic", show @Int testnetMagic
        , "--out-file", work </> "utxo-plutus-staking-payment-address-2.json"
        ]

  H.cat $ work </> "utxo-plutus-staking-payment-address-2.json"

  utxoPlutusPaymentAddrJson2 <- H.leftFailM . H.readJsonFile $ work </> "utxo-plutus-staking-payment-address-2.json"
  UTxO utxoPlutus2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(UTxO AlonzoEra) utxoPlutusPaymentAddrJson2
  -- Get total lovelace at plutus script address

  let lovelaceAtPlutusAddr = mconcat . map (\(TxOut _ v _) -> txOutValueToLovelace v) $ Map.elems utxoPlutus2

  H.note_ "Check that the withdrawal from the Plutus staking address was successful"
  lovelaceAtPlutusAddr === pr + 5000000 + 5000000 + 5000000 + 5000000 + Lovelace minrequtxo
