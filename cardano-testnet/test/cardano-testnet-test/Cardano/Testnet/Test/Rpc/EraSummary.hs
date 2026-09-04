{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.EraSummary
  ( hprop_rpc_read_era_summary
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp

import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c
import           Cardano.Testnet

import           Prelude

import           Control.Monad (forM_, void)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isNothing)
import qualified Data.Text as Text
import           Lens.Micro

import           Testnet.Property.Util (integrationRetryWorkspace)

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | The lowercase era names the UTxO RPC spec uses, in hard-fork order.
-- 'ReadEraSummary' only ever reports a prefix of this list: the eras the
-- ledger has actually seen so far.
eraNamesInOrder :: [Text.Text]
eraNamesInOrder = ["byron", "shelley", "allegra", "mary", "alonzo", "babbage", "conway", "dijkstra"]

-- | Run with:
-- @TASTY_PATTERN='/RPC ReadEraSummary/' cabal test cardano-testnet-test@
hprop_rpc_read_era_summary :: H.Property
hprop_rpc_read_era_summary = integrationRetryWorkspace 2 "rpc-read-era-summary" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'

  let era = Exp.ConwayEra
      sbe = convert era
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}

  TestnetRuntime
    { testnetNodes = node0 :| _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0
  let rpcServer = Rpc.ServerUnix rpcSocket

  response <-
    H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.QueryService "readEraSummary")) def

  cardanoSummaries <- H.nothingFail (response ^. U5c.maybe'cardano)
  let summaries = cardanoSummaries ^. U5c.summaries

  H.note_ "summaries is non-empty"
  H.assertWith summaries $ not . null
  nonEmptySummaries <- H.nothingFail (NonEmpty.nonEmpty summaries)
  let firstEntry = NonEmpty.head nonEmptySummaries
      lastEntry = NonEmpty.last nonEmptySummaries
      initEntries = NonEmpty.init nonEmptySummaries

  H.note_ "names are exactly a prefix of the canonical era order"
  map (^. U5c.name) summaries H.=== take (length summaries) eraNamesInOrder

  H.note_ "every entry except the last has its end set; the last entry's end is unset"
  forM_ initEntries $ \entry -> void $ H.nothingFail (entry ^. U5c.maybe'end)
  H.assertWith (lastEntry ^. U5c.maybe'end) isNothing

  H.note_ "consecutive boundaries line up: an era's end is the next era's start"
  forM_ (zip summaries (drop 1 summaries)) $ \(cur, next) -> do
    end <- H.nothingFail (cur ^. U5c.maybe'end)
    end ^. U5c.slot H.=== next ^. U5c.start . U5c.slot
    end ^. U5c.epoch H.=== next ^. U5c.start . U5c.epoch
    end ^. U5c.time H.=== next ^. U5c.start . U5c.time

  H.note_ "start boundaries are non-decreasing across eras (testnet hard-forks eagerly, so mostly equal)"
  H.assertWith (map (^. U5c.start . U5c.slot) summaries) isNonDecreasing
  H.assertWith (map (^. U5c.start . U5c.epoch) summaries) isNonDecreasing
  H.assertWith (map (^. U5c.start . U5c.time) summaries) isNonDecreasing

  H.note_ "the first era starts at slot 0, epoch 0"
  firstEntry ^. U5c.start . U5c.slot H.=== 0
  firstEntry ^. U5c.start . U5c.epoch H.=== 0

  H.note_ "protocolParams is unset for every era: only ReadParams carries current parameters"
  forM_ summaries $ \entry -> H.assertWith (entry ^. U5c.maybe'protocolParams) isNothing
 where
  isNonDecreasing :: Ord a => [a] -> Bool
  isNonDecreasing xs = and $ zipWith (<=) xs (drop 1 xs)
