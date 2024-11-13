{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.HardFork
  ( hprop_hardfork
  ) where

import           Cardano.Api
import           Cardano.Api.Experimental
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Shelley.LedgerState as L
import qualified Cardano.Ledger.Shelley.UTxO as L
import           Cardano.Testnet

import           Prelude

import           Control.Lens ((^.))
import           Data.Default.Class
import           Data.Type.Equality
import           Data.Typeable
import           GHC.IO.Exception (IOException)
import           GHC.Stack

import           Testnet.Components.Query
import           Testnet.Process.Run
import           Testnet.Property.Assert (assertErasEqual)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types

import           Hedgehog
import qualified Hedgehog as H
import           Hedgehog.Extras (Integration)
import qualified Hedgehog.Extras as H
import           Hedgehog.Extras.Test (MonadAssertion)

hprop_hardfork :: Property
hprop_hardfork = integrationRetryWorkspace 2 "ledger-events-sanity-check" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) } <- mkConf tempAbsBasePath'

  let babbage = BabbageEraOnwardsBabbage
      conway = ShelleyBasedEraConway
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath
      fastTestnetOptions = def
        { cardanoNodeEra = AnyShelleyBasedEra (babbageEraOnwardsToShelleyBasedEra babbage)
        }

      shelleyOptions = def
        { genesisEpochLength = 100
        , genesisSlotLength = 0.1
        }


  TestnetRuntime{configurationFile, testnetNodes, testnetMagic}
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf
  nr@TestnetNode{nodeSprocket} <- H.headM testnetNodes
  execConfig <- mkExecConfig tempBaseAbsPath nodeSprocket testnetMagic
  let socketPath = nodeSocketPath nr

  esv <- getEpochStateView configurationFile socketPath

  getProtVer esv  >>= H.annotateShow
  H.noteShowM_ $ waitForEpochs esv (L.EpochInterval 4)

  getProtVer esv  >>= H.annotateShow
  -- getPP esv conway >>= H.annotateShow
  -- getEpochState esv >>= H.annotateShow
  H.noteM_ $ execCli' execConfig [ "conway", "query", "protocol-parameters" ]

  H.note_ $ "Sprocket: " <> show nodeSprocket
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath

  H.failure

getProtVer
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> m L.ProtVer
getProtVer epochStateView = withFrozenCallStack $ do
  AnyNewEpochState sbe newEpochState <- getEpochState epochStateView
  pure $ shelleyBasedEraConstraints sbe $ newEpochState ^. L.nesEsL . L.curPParamsEpochStateL . L.ppProtocolVersionL

getPP
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra era
  -> m (L.PParams (ShelleyLedgerEra era))
getPP epochStateView sbe' = withFrozenCallStack $ do
  AnyNewEpochState sbe newEpochState <- getEpochState epochStateView
  Refl <- H.leftFail $ assertErasEqual sbe sbe'
  pure $ shelleyBasedEraConstraints sbe $ newEpochState ^. L.nesEsL . L.curPParamsEpochStateL
