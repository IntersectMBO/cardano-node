{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- Profiles using "chainsync" as scenario.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Scenario.Chainsync (
  profilesNoEraChainsync
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Scenario.Base as B
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

profilesNoEraChainsync :: [Types.Profile]
profilesNoEraChainsync =
  let chainsync =
          P.empty & B.base
        . P.chainsync
        -- TODO: Explorer node is needed ? Or a new property is needed ?
        . P.uniCircle . P.withExplorerNode . P.loopback
        . P.hostsChainsync 1 . P.withChaindbServer
        . V.genesisVariant300
        . V.timescaleMainnet
         -- TODO: "tracer-only" and "idle" have `P.delegators 6`.
         --       Remove and use `V.datasetEmpty` in module "Scenario.Base".
        . P.delegators 0
        . P.analysisPerformance
        . P.preset "mainnet"
        . P.desc "Mainnet chain syncing benchmark"
      {--
          │ mainnet_chunks:
          │ │ chaindb_server: 10
          │ │ explorer: 0
          │ ledger_snapshot:
          │ │ chaindb_server: 237599
          │ │ explorer: 0
          suffix: notrc
      --}
      byron  = chainsync & P.shutdownOnSlot   237599 . P.chaindb (  10,  0) (  237599,0)
      {--
          │ mainnet_chunks:
          │ │ chaindb_server: 1800
          │ │ explorer: 1799
          │ ledger_snapshot:
          │ │ chaindb_server: 3.8901589e+07
          │ │ explorer: 3.717365e+07
          suffix: notrc
      --}
      alonzo = chainsync & P.shutdownOnSlot 38901589 . P.chaindb (1800,1799) (38901589,37173650)
  in [
  -- Byron
    byron & P.name "chainsync-early-byron"               . P.traceForwardingOff . P.newTracing . P.p2pOff
  , byron & P.name "chainsync-early-byron-notracer"      . P.traceForwardingOff . P.newTracing . P.p2pOff
  , byron & P.name "chainsync-early-byron-oldtracing"    . P.traceForwardingOff . P.oldTracing . P.p2pOff
  -- Alonzo
  , alonzo  & P.name "chainsync-early-alonzo"            . P.traceForwardingOff . P.newTracing . P.p2pOff
  , alonzo  & P.name "chainsync-early-alonzo-notracer"   . P.traceForwardingOff . P.newTracing . P.p2pOff
  -- TODO: Remove and make `P.p2pOn` the default without adding a "-nop2p" profile.
  , alonzo  & P.name "chainsync-early-alonzo-p2p"        . P.traceForwardingOff . P.newTracing . P.p2pOn
  , alonzo  & P.name "chainsync-early-alonzo-oldtracing" . P.traceForwardingOff . P.oldTracing . P.p2pOff
  ]
