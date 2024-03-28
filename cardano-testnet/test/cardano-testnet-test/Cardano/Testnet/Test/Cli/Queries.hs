{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use head" -}

module Cardano.Testnet.Test.Cli.Queries
  ( hprop_cli_queries
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api

import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput)
import           Cardano.Testnet

import           Prelude

import           Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Testnet.Components.Query (checkDRepsNumber)
import qualified Testnet.Process.Cli as H
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Golden as H

-- | Test CLI queries
-- Execute me with:
-- @cabal test cardano-testnet-test --test-options '-p "/CliQueries/"'@
-- If you want to recreate golden files, run the comment with
-- RECREATE_GOLDEN_FILES=1 as its prefix
hprop_cli_queries :: Property
hprop_cli_queries = H.integrationWorkspace "cli-queries" $ \tempAbsBasePath' -> do
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) }
    <- mkConf tempAbsBasePath'
  let tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        , cardanoNodeEra = cEra
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile

  -- TODO: we could wait less: waiting 1 block should suffice.
  checkDRepsNumber sbe (Api.File configurationFile) (Api.File socketPath) execConfig 3

  -- protocol-parameters to stdout
  protocolParametersOut <- H.execCli' execConfig
                             [ "conway", "query", "protocol-parameters" ]
  H.diffVsGoldenFile
    protocolParametersOut
    "test/cardano-testnet-test/files/golden/queries/protocolParametersOut.txt"
  -- protocol-parameters to a file
  let protocolParametersOutFile = work </> "protocol-parameters-out.json"
  H.noteM_ $ H.execCli' execConfig [ "conway", "query", "protocol-parameters"
                                   , "--out-file", protocolParametersOutFile]
  H.diffFileVsGoldenFile
    protocolParametersOutFile
    "test/cardano-testnet-test/files/golden/queries/protocolParametersFileOut.json"

  -- tip to stdout
  _ :: QueryTipLocalStateOutput <- H.noteShowM $ H.execCliStdoutToJson execConfig [ "conway", "query", "tip" ]
  -- tip to a file
  let tipOutFile = work </> "tip-out.json"
  H.noteM_ $ H.execCli' execConfig [ "conway", "query", "tip"
                                   , "--out-file", tipOutFile]
  _ :: QueryTipLocalStateOutput <- H.leftFailM . H.readJsonFile $ tipOutFile

  -- stake-pools to stdout
  stakePoolsOut <- H.execCli' execConfig [ "conway", "query", "stake-pools" ]
  length (lines stakePoolsOut) H.=== 3 -- Because, by default, 3 stake pools are created
  -- Light test of the query's answer, the ids should exist:
  forM_ (lines stakePoolsOut) $ \stakePoolId -> do
    H.execCli' execConfig [ "conway", "query", "pool-state"
                          , "--stake-pool-id", stakePoolId ]
  -- stake-pools to a file
  let stakePoolsOutFile = work </> "stake-pools-out.json"
  H.noteM_ $ H.execCli' execConfig [ "conway", "query", "stake-pools"
                                   , "--out-file", stakePoolsOutFile]

  -- query drep-state to stdout
  -- TODO: deserialize to a Haskell value when
  -- https://github.com/IntersectMBO/cardano-cli/issues/606 is tackled
  dreps :: Aeson.Value <- H.noteShowM $ H.execCliStdoutToJson execConfig [ "conway", "query", "drep-state", "--all-dreps"]
  assertArrayOfSize dreps 3
  -- query drep-state to a file
  let drepStateOutFile = work </> "drep-state-out.json"
  H.noteM_ $ H.execCli' execConfig [ "conway", "query", "drep-state", "--all-dreps"
                                   , "--out-file", drepStateOutFile]
  _ :: Aeson.Value <- H.leftFailM . H.readJsonFile $ drepStateOutFile

  H.success

-- | @assertArrayOfSize v n@ checks that the value is a JSON array of size @n@,
-- otherwise it fails the test.
assertArrayOfSize :: ()
  => HasCallStack
  => MonadTest m
  => Aeson.Value
  -> Int
  -> m ()
assertArrayOfSize v n =
  case v of
    Aeson.Array a -> do
      let actualLength = Vector.length a
      if actualLength == n
      then H.success
      else do
        H.note_ $ "Expected an array of length " <> show n <> ", but got: " <> show actualLength
        H.failure
    Aeson.Object _ -> failWrongType "object"
    Aeson.Bool _   -> failWrongType "bool"
    Aeson.Number _ -> failWrongType "number"
    Aeson.Null     -> failWrongType "null"
    Aeson.String _ -> failWrongType "string"
   where
     failWrongType got = do
       H.note_ $ "Expected a JSON object, but received: " <> got
       H.failure

