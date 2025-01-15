{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Testnet.Test.Gov.DRepDeposit
  ( hprop_ledger_events_drep_deposits
  ) where

import           Cardano.Api
import           Cardano.Api.Experimental (Some (..))
import qualified Cardano.Api.Ledger as L

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class
import qualified Data.Map as Map
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Process.Cli.DRep
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog.Extras as H


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/DRep Deposits/"'@
hprop_ledger_events_drep_deposits :: Property
hprop_ledger_events_drep_deposits = integrationWorkspace "drep-deposits" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do


  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = def
        { cardanoNodeEra = AnyShelleyBasedEra sbe
        , cardanoNumDReps = 0
        }
      shelleyOptions = def { genesisEpochLength = 100 }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets=wallet0:wallet1:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo

  -- DRep 1 (not enough deposit)

  drepDir1 <- H.createDirectoryIfMissing $ gov </> "drep1"

  drepKeyPair1 <- generateDRepKeyPair execConfig drepDir1 "keys"
  drepRegCert1 <- generateRegistrationCertificate execConfig drepDir1 "reg-cert"
                                                  drepKeyPair1 (minDRepDeposit - 1)
  drepRegTxBody1 <- createCertificatePublicationTxBody execConfig epochStateView sbe drepDir1 "reg-cert-txbody"
                                                       drepRegCert1 wallet0
  drepSignedRegTx1 <- signTx execConfig cEra drepDir1 "signed-reg-tx"
                             drepRegTxBody1 [Some drepKeyPair1, Some $ paymentKeyInfoPair wallet0]

  failToSubmitTx execConfig cEra drepSignedRegTx1 "ConwayDRepIncorrectDeposit"

  -- DRep 2 (enough deposit)

  void $ registerDRep execConfig epochStateView ceo work "drep2" wallet1

  checkDRepState epochStateView sbe $ \m ->
    if map L.drepDeposit (Map.elems m) == [L.Coin minDRepDeposit]
       then Just ()
       else Nothing


