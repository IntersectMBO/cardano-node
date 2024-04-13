{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import           Testnet.Defaults
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
  H.threadDelay 1000
  H.writeFile "hprop_drep_retirement.start" "hprop_drep_retirement"

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
    , wallets=wallet0:_
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
  let sizeBefore = 3
      configFile' = Api.File configurationFile
      socketPath' = Api.File socketPath

  checkDRepsNumber sbe configFile' socketPath' execConfig sizeBefore

  -- Deregister first DRep
  let dreprRetirementCertFile = gov </> "drep-keys" <> "drep1.retirementcert"

  H.noteM_ $ H.execCli' execConfig
     [ "conway", "governance", "drep", "retirement-certificate"
     , "--drep-verification-key-file", defaultDRepVkeyFp 1
     , "--deposit-amt", show @Int 1_000_000
     , "--out-file", dreprRetirementCertFile
     ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--cardano-mode"
    , "--out-file", work </> "utxo-11.json"
    ]

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  drepRetirementRegTxbodyFp <- H.note $ work </> "drep.retirement.txbody"
  drepRetirementRegTxSignedFp <- H.note $ work </> "drep.retirement.tx"

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--certificate-file", dreprRetirementCertFile
    , "--witness-override", "2"
    , "--out-file", drepRetirementRegTxbodyFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", drepRetirementRegTxbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--signing-key-file", defaultDRepSkeyFp 1
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
  H.writeFile "hprop_drep_retirement.stop" "hprop_drep_retirement"
