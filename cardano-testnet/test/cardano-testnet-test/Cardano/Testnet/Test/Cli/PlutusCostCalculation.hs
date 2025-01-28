{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Testnet.Test.Cli.PlutusCostCalculation (
    hprop_plutus_cost_calculation,
    -- | Execute tests in this module with:
    -- @DISABLE_RETRIES=1 cabal run cardano-testnet-test -- -p "/Spec.hs.Spec.CLI.included plutus/"@
) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Data.Default.Class
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Query (getEpochStateView)
import           Testnet.Process.Run (mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_plutus_cost_calculation :: Property
hprop_plutus_cost_calculation = integrationRetryWorkspace 2 "included plutus" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
    H.note_ SYS.os
    conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
    let tempAbsPath' = unTmpAbsPath tempAbsPath
    _work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

    let
        sbe = ShelleyBasedEraConway
        _txEra = AsConwayEra
        era = toCardanoEra sbe
        _cEra = AnyCardanoEra era
        tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
        options = def{cardanoNodeEra = AnyShelleyBasedEra sbe}

    TestnetRuntime
        { configurationFile
        , testnetMagic
        , testnetNodes
        , wallets = _wallet0 : _
        } <-
        cardanoTestnetDefault options def conf

    poolNode1 <- H.headM testnetNodes
    poolSprocket1 <- H.noteShow $ nodeSprocket poolNode1
    _execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
    _epochStateView <- getEpochStateView configurationFile (nodeSocketPath poolNode1)

    H.failure
