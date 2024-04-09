{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.DRepDeposits
  ( hprop_ledger_events_drep_deposits
  ) where

import           Cardano.Api (AnyCardanoEra (..), EpochNo (EpochNo), File (..),
                   ShelleyBasedEra (..), ToCardanoEra (..))

import           Cardano.Testnet
                   (CardanoTestnetOptions (cardanoEpochLength, cardanoNodeEra, cardanoNumDReps),
                   Conf (Conf, tempAbsPath), NodeRuntime (nodeSprocket),
                   TmpAbsolutePath (unTmpAbsPath), cardanoDefaultTestnetOptions,
                   cardanoTestnetDefault, makeTmpBaseAbsPath, mkConf)

import           Prelude

import           System.FilePath ((</>))

import           Testnet.Components.DReps (createDRepRegistrationTxBody, failToSubmitTx,
                   generateDRepKeyPair, generateRegistrationCertificate, getDRepDeposits, signTx,
                   submitTx)
import           Testnet.Components.Query (getEpochStateView, getMinDRepDeposit)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime (PaymentKeyInfo (paymentKeyInfoPair), PoolNode (poolRuntime),
                   TestnetRuntime (TestnetRuntime, configurationFile, poolNodes, testnetMagic, wallets))

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/DRep Deposits/"'@
hprop_ledger_events_drep_deposits :: Property
hprop_ledger_events_drep_deposits = H.integrationWorkspace "drep-deposits" $ \tempAbsBasePath' -> do

  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 0
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:_
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
  H.note_ $ "Foldblocks config file: " <> configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  minDRepDeposit <- getMinDRepDeposit execConfig

  -- DRep 1 (not enough deposit)

  drepDir1 <- H.createDirectoryIfMissing $ gov </> "drep1"

  drepKeyPair1 <- generateDRepKeyPair execConfig drepDir1 "keys"
  drepRegCert1 <- generateRegistrationCertificate execConfig drepDir1 "reg-cert"
                                                  drepKeyPair1 (minDRepDeposit - 1)
  drepRegTxBody1 <- createDRepRegistrationTxBody execConfig epochStateView sbe drepDir1 "reg-cert-txbody"
                                                 drepRegCert1 wallet0
  drepSignedRegTx1 <- signTx execConfig cEra drepDir1 "signed-reg-tx"
                             drepRegTxBody1 [drepKeyPair1, paymentKeyInfoPair wallet0]

  failToSubmitTx execConfig cEra drepSignedRegTx1

  -- DRep 2 (enough deposit)

  drepDir2 <- H.createDirectoryIfMissing $ gov </> "drep2"

  drepKeyPair2 <- generateDRepKeyPair execConfig drepDir2 "keys"
  drepRegCert2 <- generateRegistrationCertificate execConfig drepDir2 "reg-cert"
                                                  drepKeyPair2 minDRepDeposit
  drepRegTxBody2 <- createDRepRegistrationTxBody execConfig epochStateView sbe drepDir2 "reg-cert-txbody"
                                                 drepRegCert2 wallet1
  drepSignedRegTx2 <- signTx execConfig cEra drepDir2 "signed-reg-tx"
                             drepRegTxBody2 [drepKeyPair2, paymentKeyInfoPair wallet1]

  submitTx execConfig cEra drepSignedRegTx2

  deposits <- H.evalMaybeM $ getDRepDeposits sbe (File configurationFile) (File socketPath) (EpochNo 10) 1

  deposits H.=== [minDRepDeposit]


