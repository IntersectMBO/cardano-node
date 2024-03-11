{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{- HLINT ignore "Use head" -}

module Cardano.Testnet.Test.Cli.Conway.DRepRetirement
  ( hprop_drep_retirement
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api

import           Cardano.Testnet

import           Prelude

import qualified Data.Text as Text
import           System.FilePath ((</>))

import           Testnet.Components.Query
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

-- | The era in which this test runs
sbe :: ShelleyBasedEra ConwayEra
sbe = ShelleyBasedEraConway

-- Execute this test with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/DRepRetirement/"'@
hprop_drep_retirement :: Property
hprop_drep_retirement = H.integrationRetryWorkspace 2 "drep-retirement" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let era = toCardanoEra sbe
      cardanoNodeEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 50 -- 50 * (1/10s) length, i.e. 5 seconds
        , cardanoSlotLength = 0.1  -- 1/10s slot (100ms)
        , cardanoNodeEra
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets
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

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  let drepVkeyFp :: Int -> FilePath
      drepVkeyFp n = tempAbsPath' </> "drep-keys" </> ("drep" <> show n) </> "drep.vkey"

      drepSKeyFp :: Int -> FilePath
      drepSKeyFp n = tempAbsPath' </> "drep-keys" </> ("drep" <> show n) </> "drep.skey"

  -- Create Drep registration certificates
  let drepCertFile :: Int -> FilePath
      drepCertFile n = gov </> "drep-keys" <>"drep" <> show n <> ".regcert"
  H.forConcurrently_ [1..3] $ \n -> do
    H.noteM_ $ H.execCli' execConfig
       [ "conway", "governance", "drep", "registration-certificate"
       , "--drep-verification-key-file", drepVkeyFp n
       , "--key-reg-deposit-amt", show @Int 1_000_000
       , "--out-file", drepCertFile n
       ]

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe $ wallets !! 0

  -- Submit registration certificates
  drepRegTxbodyFp <- H.note $ work </> "drep.registration.txbody"
  drepRegTxSignedFp <- H.note $ work </> "drep.registration.tx"

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
    , "--certificate-file", drepCertFile 1
    , "--certificate-file", drepCertFile 2
    , "--certificate-file", drepCertFile 3
    , "--witness-override", show @Int 4
    , "--out-file", drepRegTxbodyFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", drepRegTxbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 0
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", drepRegTxSignedFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", drepRegTxSignedFp
    ]

  let sizeBefore = 3
      configFile' = Api.File configurationFile
      socketPath' = Api.File socketPath

  checkDRepsNumber sbe configFile' socketPath' execConfig sizeBefore

  -- Deregister first DRep
  let dreprRetirementCertFile = gov </> "drep-keys" <> "drep1.retirementcert"

  H.noteM_ $ H.execCli' execConfig
     [ "conway", "governance", "drep", "retirement-certificate"
     , "--drep-verification-key-file", drepVkeyFp 1
     , "--deposit-amt", show @Int 1_000_000
     , "--out-file", dreprRetirementCertFile
     ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
    , "--cardano-mode"
    , "--out-file", work </> "utxo-11.json"
    ]

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe $ wallets !! 0

  drepRetirementRegTxbodyFp <- H.note $ work </> "drep.retirement.txbody"
  drepRetirementRegTxSignedFp <- H.note $ work </> "drep.retirement.tx"

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
    , "--certificate-file", dreprRetirementCertFile
    , "--witness-override", "2"
    , "--out-file", drepRetirementRegTxbodyFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", drepRetirementRegTxbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 0
    , "--signing-key-file", drepSKeyFp 1
    , "--out-file", drepRetirementRegTxSignedFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", drepRetirementRegTxSignedFp
    ]

  -- The important bit is that we pass (sizeBefore - 1) as the last argument,
  -- to witness that the number of dreps indeed decreased.
  checkDRepsNumber sbe configFile' socketPath' execConfig (sizeBefore - 1)
  H.success
