{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.DijkstraGenesis
  ( hprop_dijkstra_genesis
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (EpochInterval (..))

import           Cardano.Testnet

import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Lens as Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default.Class (def)
import qualified Data.List.NonEmpty as NEL
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Cardano (liftToIntegration)
import           Testnet.Start.Types

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Starts a testnet in Conway with a custom Dijkstra genesis, hard forks it to Dijkstra
-- at epoch 1, and checks that the Dijkstra protocol parameters come from that genesis.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Can start from a custom Dijkstra genesis/"'@
hprop_dijkstra_genesis :: H.Property
hprop_dijkstra_genesis = integrationRetryWorkspace 2 "dijkstra-genesis" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do
  -- Every value in this file differs from the default
  let dijkstraGenesisFp = "test/cardano-testnet-test/files/input/dijkstra-genesis.json"
  dijkstraGenesis <- H.readJsonFileOk dijkstraGenesisFp

  let creationOptions = def
        { creationGenesisOptions = def { genesisEpochLength = 200 }
        , creationDijkstraGenesis = UserProvidedData dijkstraGenesis
        }

  conf <- mkConf tmpDir
  liftToIntegration $ createTestnetEnv creationOptions conf

  -- Turn Dijkstra on and hard fork to it at epoch 1
  H.rewriteJsonFile (tmpDir </> "configuration.yaml") $ \(config :: Aeson.Value) ->
    config & Aeson._Object %~
      ( KeyMap.insert "ExperimentalHardForksEnabled" (Aeson.Bool True)
      . KeyMap.insert "TestDijkstraHardForkAtEpoch" (Aeson.Number 1)
      )

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , configurationFile
    } <- liftToIntegration $ cardanoTestnet (creationNodes creationOptions) def conf

  let node@TestnetNode{nodeSprocket} = NEL.head testnetNodes
  execConfig <- mkExecConfig tmpDir nodeSprocket testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node)

  H.note_ "Waiting for the Dijkstra era"
  let currentEra = do
        AnyNewEpochState sbe _ _ <- getEpochState epochStateView
        pure (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe :: AnyShelleyBasedEra)
  void $ retryUntilM epochStateView (WaitForEpochs (EpochInterval 3))
    currentEra (== AnyShelleyBasedEra ShelleyBasedEraDijkstra)

  H.note_ "The Dijkstra protocol parameters are the ones from the custom genesis"
  pparams :: Aeson.Value <- H.leftFail . Aeson.eitherDecode . B.pack =<< execCli' execConfig
    [ "query", "protocol-parameters"
    , "--output-json"
    ]
  expected :: Aeson.Object <- H.readJsonFileOk dijkstraGenesisFp
  let actual key = case key of
        "plutusV4CostModel" -> pparams ^? Aeson.key "costModels" . Aeson.key "PlutusV4"
        _ -> pparams ^? Aeson.key key
      mismatches =
        [ (key, value, actual key)
        | (key, value) <- KeyMap.toList expected
        , actual key /= Just value
        ]
  mismatches H.=== []
