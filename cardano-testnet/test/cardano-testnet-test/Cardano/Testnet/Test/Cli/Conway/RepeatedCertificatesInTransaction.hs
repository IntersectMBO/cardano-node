{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Conway.RepeatedCertificatesInTransaction where

import           Cardano.Api hiding (Value)

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as L
import           Data.String
import qualified Data.Text as Text
import           Lens.Micro
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Configuration (anyEraToString)
import           Testnet.Components.Query
import           Testnet.Components.SPO
import           Testnet.Process.Cli hiding (File)
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog (Property, (===))
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

hprop_repeated_certificates_in_transaction :: Property
hprop_repeated_certificates_in_transaction = H.integrationRetryWorkspace 0 "repeated-certificates-in-tx" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    sbe = ShelleyBasedEraBabbage
    era = AnyCardanoEra $ toCardanoEra sbe
    eraString = anyEraToString era

    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    options = cardanoDefaultTestnetOptions
      { cardanoNodeEra = era
      }

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:_
    } <- cardanoTestnetDefault options conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'
  epochStateView <- getEpochStateView (File configurationFile) (File socketPath)


  let testStakeDelegator = work </> "test-delegator"

  H.createDirectoryIfMissing_ testStakeDelegator
  let testDelegatorVkeyFp = testStakeDelegator </> "test-delegator.vkey"
      testDelegatorSKeyFp = testStakeDelegator </> "test-delegator.skey"
      testDelegatorPaymentVKeyFp = testStakeDelegator </> "test-delegator-payment.vkey"
      testDelegatorPaymentSKeyFp = testStakeDelegator </> "test-delegator-payment.skey"
      testDelegatorRegCertFp = testStakeDelegator </> "test-delegator.regcert"
      testDelegatorDeregCertFp = testStakeDelegator </> "test-delegator.deregcert"

  _ <- cliStakeAddressKeyGen work
    $ KeyNames testDelegatorVkeyFp testDelegatorSKeyFp
  _ <- cliAddressKeyGen work
    $ KeyNames testDelegatorPaymentVKeyFp testDelegatorPaymentSKeyFp

  testDelegatorPaymentAddr <- execCli
                [ "address", "build"
                , "--testnet-magic", show @Int testnetMagic
                , "--payment-verification-key-file", testDelegatorPaymentVKeyFp
                , "--stake-verification-key-file", testDelegatorVkeyFp
                ]

  -- Test stake address registration cert
  createStakeKeyRegistrationCertificate
    tempAbsPath
    era
    testDelegatorVkeyFp
    2_000_000
    testDelegatorRegCertFp

  createStakeKeyDeregistrationCertificate
    tempAbsPath
    era
    testDelegatorVkeyFp
    2_000_000
    testDelegatorDeregCertFp

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  multipleCertTxBodyFp <- H.note $ work </> "registration.txbody"
  void $ execCli' execConfig
    [ eraString
    , "transaction", "build"
    , "--change-address", testDelegatorPaymentAddr
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 5_000_000
    , "--witness-override", show @Int 2
    , "--certificate-file", testDelegatorRegCertFp
    , "--certificate-file", testDelegatorDeregCertFp
    , "--certificate-file", testDelegatorRegCertFp
    , "--certificate-file", testDelegatorDeregCertFp
    , "--certificate-file", testDelegatorRegCertFp
    , "--out-file", multipleCertTxBodyFp
    ]

  txJson <- (H.leftFail . A.eitherDecode @A.Value . fromString) =<< H.execCli
    ["transaction", "view"
    , "--tx-file", multipleCertTxBodyFp
    ]
  A.Success certificates <- fmap (A.fromJSON @[A.Value]) . H.nothingFail $ txJson ^? L.key "certificates"
  H.noteShow_ $ A.encode certificates
  length certificates === 5

