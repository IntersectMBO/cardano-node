{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------
{-

A function, `empty`, that creates a `Profile` as empty as possible (zeroes,
False, "" and Nothing when possible) and many other functions that receive a
`Profile` and return a `Profile`. All these other functions are the primitives,
everything needed to construct a profile and should be kept simple.

For example: empty & name "my local profile" . idle . hosts 2 . loopback

-}
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile (
    Types.Profile (Profile)

  , empty

  -- Name and description.
  , name, desc

  -- Scenario.
  , idle, tracerOnly, fixedLoaded, chainsync, latency

  -- Composition
  -- Composition topology.
  , uniCircle, torus, torusDense
  -- Composition location.
  , loopback, regions
  -- Composition size.
  , hosts, pools, hostsChainsync, withExplorerNode
  -- Etc.
  , withChaindbServer

 -- Genesis
  -- Set the epoch number from the "epoch-timeline".
  , pparamsEpoch
  -- Overlays to use.
  , v8Preview, v9Preview, stepHalf, doubleBudget
 -- Customize the "shelley", "alonzo" or "conway" properties.
  , shelley, alonzo, conway
  -- Time and block params.
  , slotDuration, epochLength, activeSlotsCoeff, parameterK
  -- Genesis size.
  , utxo, delegators, dreps
  -- If the genesis file is big more time is needed for deployment.
  , extraFutureOffset
  -- Funds for the generator.
  , poolBalance, funds

  -- Node
  -- LMDB True or False.
  , lmdb
  -- Node's p2p flag.
  , p2pOn, p2pOff
  -- Node's tracer flag.
  , tracerOn, tracerOff
  -- Node's tracer type.
  , newTracing, oldTracing
  -- Node's --shutdown-on-*-sync.
  , shutdownOnSlot, shutdownOnBlock, shutdownOnOff
  -- Node's RTS params.
  , rtsGcNonMoving, rtsGcAllocSize, rtsThreads, rtsHeapLimit
  , heapLimit

  -- Generator params.
  , tps, txIn, txOut, txFee, initCooldown
  , plutusType, plutusScript
  , redeemerInt, redeemerFields
  , generatorEpochs

  -- Tracer's params.
  , tracerRtview, tracerWithresources

  -- Cluster params.
  , clusterMinimunStorage, ssdDirectory, clusterKeepRunningOn
  , nomadNamespace, nomadClass, nomadResources, nomadHostVolume, nomadSSHLogsOn
  , awsInstanceTypes, usePublicRouting

  -- Analysis params.
  , analysisOff, analysisStandard, analysisPerformance
  , analysisSizeSmall, analysisSizeModerate, analysisSizeModerate2, analysisSizeFull
  , analysisUnitary, analysisEpoch3Plus
  , cBlockMinimumAdoptions

  , shelleyAlonzoConway, derive
  , cliArgs

  , preset

) where

import           Prelude hiding (id)
import           Data.List (sort)
import           Control.Monad (foldM)
import           System.IO.Unsafe (unsafePerformIO)

import           GHC.Stack (HasCallStack)

import qualified Data.Time as Time
import qualified Data.Scientific as Scientific
import qualified Data.Text            as Text
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Cardano.Benchmarking.Profile.Types as Types

import qualified Paths_cardano_profile as Paths

--------------------------------------------------------------------------------

-- This function is part of the profile building primitives module because
-- these functions rely on these values to throw an error if a value other than
-- ones here is being overwritten.
-- `Profile` itself and its property types have no sensible `mempty` instance.
empty :: Types.Profile
empty = Types.Profile {
    Types.name = ""
  , Types.desc = Nothing
  , Types.scenario = Types.Idle
  , Types.composition = Types.Composition {
      Types.locations = []
    , Types.n_bft_hosts = 0
    , Types.n_singular_hosts = 0
    , Types.n_dense_hosts = 0
    , Types.dense_pool_density = 0
    , Types.with_proxy = False
    , Types.with_explorer = False
    , Types.topology = Types.Line
    , Types.with_chaindb_server = Nothing
    , Types.n_hosts = 0
    , Types.n_pools = 0
    , Types.n_singular_pools = 0
    , Types.n_dense_pools = 0
    , Types.n_pool_hosts = 0
  }
  , Types.era = Types.Allegra
  , Types.genesis = Types.Genesis {
      Types.pparamsEpoch = 0
    , Types.pparamsOverlays = []
    , Types.shelley = mempty
    , Types.alonzo = mempty
    , Types.conway = Nothing
    , Types.slot_duration = 0
    , Types.epoch_length = 0
    , Types.active_slots_coeff = 0
    , Types.parameter_k = 0
    , Types.utxo = 0
    , Types.delegators = Nothing
    , Types.dreps = 0
    , Types.extra_future_offset = 0
    , Types.per_pool_balance = 0
    , Types.funds_balance = 0
    , Types.network_magic = 0
    , Types.pool_coin = 0
    , Types.delegator_coin = 0
    , Types.single_shot = False
  }
  , Types.node = Types.Node {
      Types.utxo_lmdb = False
    , Types.verbatim = Types.NodeVerbatim Nothing
    , Types.nodeTracer = False
    , Types.tracing_backend = ""
    , Types.rts_flags_override = []
    , Types.heap_limit = Nothing
    , Types.shutdown_on_slot_synced = Nothing
    , Types.shutdown_on_block_synced = Nothing
  }
  , Types.generator = Types.Generator {
      Types.tps = 0
    , Types.inputs_per_tx = 0
    , Types.outputs_per_tx = 0
    , Types.tx_fee = 0
    , Types.init_cooldown = 0
    , Types.plutus = Types.Plutus Nothing Nothing Nothing
    , Types.epochs = 0
    , Types.tx_count = Nothing
    , Types.add_tx_size = 0
  }
  , Types.tracer = Types.Tracer {
      Types.rtview = False
    , Types.ekg = False
    , Types.withresources = False
  }
  , Types.cluster = Types.Cluster {
      Types.nomad = Types.ClusterNomad {
        Types.namespace = "default"
      , Types.nomad_class = ""
      , Types.resources = Types.ByNodeType {
          Types.producer = Types.Resources 0 0 0
        , Types.explorer = Just $ Types.Resources 0 0 0
        }
      , Types.host_volumes = Nothing
      , Types.fetch_logs_ssh = False
      }
    , Types.aws = Types.ClusterAWS {
        Types.instance_type = Types.ByNodeType {
          Types.producer = ""
        , Types.explorer = Nothing
        }
      , Types.use_public_routing = False
      }
    , Types.minimun_storage = Just $ Types.ByNodeType {
        Types.producer = 0
      , Types.explorer = Nothing
      }
    , Types.ssd_directory = Nothing
    , Types.keep_running = False
  }
  , Types.analysis = Types.Analysis {
      Types.analysisType = Nothing
    , Types.cluster_base_startup_overhead_s = 0
    , Types.start_log_spread_s = 0
    , Types.last_log_spread_s = 0
    , Types.silence_since_last_block_s = 0
    , Types.tx_loss_ratio = Scientific.fromFloatDigits (0 :: Double)
    , Types.finish_patience = 0
    , Types.filters = []
    , Types.filter_exprs = []
    , Types.minimum_chain_density = Scientific.fromFloatDigits (0 :: Double)
    , Types.cluster_startup_overhead_s = 0
  }
--  , Types.overlay = mempty
  , Types.derived = Types.Derived {
      Types.epoch_duration = 0
    , Types.effective_epochs = 0
    , Types.dataset_measure = 0
    , Types.delegators_effective = 0
    , Types.genesis_future_offset = 0
    , Types.generator_duration = 0
    , Types.generator_tx_count = 0
    {-- TODO: Not used ???
      Types.dataset_induced_startup_delay_optimistic = 0
    , Types.dataset_induced_startup_delay_conservative = 0
    --}
    , Types.supply_total = 0
    , Types.supply_delegated = 0
    , Types.utxo_delegated = 0
    , Types.utxo_generated = 0
    , Types.utxo_stuffed = 0
    , Types.shutdown_time = Nothing
    , Types.default_value_tx_size_estimate = 0
    , Types.default_value_tx_per_block_estimate = 0
    , Types.generator_blocks_lower_bound = 0
    , Types.dataset_induced_startup_delay_optimistic = 0
    , Types.dataset_induced_startup_delay_conservative = 0
  }
  , Types.cli_args = Types.CliArgs {
      Types.createStakedArgs = []
    , Types.createTestnetDataArgs = []
    , Types.pools = []
  }
  , Types.preset = mempty
  , Types.overlay = mempty
}

-- Name and description.
--------------------------------------------------------------------------------

name :: HasCallStack => String -> Types.Profile -> Types.Profile
name str = \p ->
  if (Types.name p) /= ""
  then error "name: `name` already set (not empty)."
  else p {Types.name = str}

desc :: HasCallStack => String -> Types.Profile -> Types.Profile
desc str = \p ->
  if (Types.desc p) /= Nothing
  then error "desc: `desc` already set (not Nothing)."
  else p {Types.desc = Just str}

-- Scenario.
--------------------------------------------------------------------------------

idle :: Types.Profile -> Types.Profile
idle p = p {Types.scenario = Types.Idle}

tracerOnly :: Types.Profile -> Types.Profile
tracerOnly p = p {Types.scenario = Types.TracerOnly}

fixedLoaded :: Types.Profile -> Types.Profile
fixedLoaded p = p {Types.scenario = Types.FixedLoaded}

chainsync :: Types.Profile -> Types.Profile
chainsync p = p {Types.scenario = Types.Chainsync}

latency :: Types.Profile -> Types.Profile
latency p = p {Types.scenario = Types.Latency}

-- Composition.
--------------------------------------------------------------------------------

composition :: (Types.Composition -> Types.Composition) -> Types.Profile -> Types.Profile
composition f p = p {Types.composition = f (Types.composition p)}

-- Topology.
------------

uniCircle :: Types.Profile -> Types.Profile
uniCircle = composition (\c -> c {Types.topology = Types.UniCircle})

torus :: Types.Profile -> Types.Profile
torus = composition (\c -> c {Types.topology = Types.Torus})

torusDense :: Types.Profile -> Types.Profile
torusDense = composition (\c -> c {Types.topology = Types.TorusDense})

-- Location.
------------

loopback :: HasCallStack => Types.Profile -> Types.Profile
loopback = composition
  (\c ->
    if (Types.locations c) /= []
    then error "loopback: `locations` already set (not empty)."
    else c {Types.locations = [Types.Loopback]}
  )

regions :: HasCallStack => [Types.Location] -> Types.Profile -> Types.Profile
regions locs = composition
  (\c ->
    if (Types.locations c) /= []
    then error "regions: `locations` already set (not empty)."
    else c {Types.locations = locs}
  )

-- Size.
--------

hosts :: HasCallStack => Integer -> Types.Profile -> Types.Profile
hosts size = composition
  (\c ->
    -- "dense_pool_density" is set by `hosts`, `pools` and `hostsChainsync`.
    if (Types.dense_pool_density c) /= 0
    then error "hosts: size already set (not zero)."
    else
      c {
        Types.n_bft_hosts = 0
      , Types.n_singular_hosts = size
      , Types.n_dense_hosts = 0
      , Types.dense_pool_density = 1
      , Types.n_hosts = size
      , Types.n_pools = size
      , Types.n_singular_pools = size
      , Types.n_dense_pools = 0
      , Types.n_pool_hosts = size
      }
  )

pools :: HasCallStack => Integer -> Types.Profile -> Types.Profile
pools size = composition
  (\c ->
    -- "dense_pool_density" is set by `hosts`, `pools` and `hostsChainsync`.
    if (Types.dense_pool_density c) /= 0
    then error "pools: size already set (not zero)."
    else
      c {
        Types.n_bft_hosts = 0
      , Types.n_singular_hosts = 0
      , Types.n_dense_hosts = 1
      , Types.dense_pool_density = size
      , Types.n_hosts = 1
      , Types.n_pools = size
      , Types.n_singular_pools = 0
      , Types.n_dense_pools = size
      , Types.n_pool_hosts = 1
      }
  )

hostsChainsync :: HasCallStack => Integer -> Types.Profile -> Types.Profile
hostsChainsync size = composition
  (\c ->
    -- "dense_pool_density" is set by `hosts`, `pools` and `hostsChainsync`.
    if (Types.dense_pool_density c) /= 0
    then error "hostsChainsync: size already set (not zero)."
    else
      c {
        Types.n_bft_hosts = 0
      , Types.n_singular_hosts = 0
      , Types.n_dense_hosts = 0
      , Types.dense_pool_density = size
      , Types.n_hosts = 0
      , Types.n_pools = 0
      , Types.n_singular_pools = 0
      , Types.n_dense_pools = 0
      , Types.n_pool_hosts = 0
      }
  )

withExplorerNode :: Types.Profile -> Types.Profile
withExplorerNode = composition (\c -> c {Types.with_explorer = True})

-- Etc.
-------

withChaindbServer :: Types.Profile -> Types.Profile
withChaindbServer = composition (\c -> c {Types.with_chaindb_server = Just True})

-- Genesis.
--------------------------------------------------------------------------------

genesis :: (Types.Genesis -> Types.Genesis) -> Types.Profile -> Types.Profile
genesis f p = p {Types.genesis = f (Types.genesis p)}

-- Epoch timeline.
------------------

pparamsEpoch :: Int -> Types.Profile -> Types.Profile
pparamsEpoch epochNumber = genesis (\g -> g {Types.pparamsEpoch = toEnum epochNumber})

-- Overlays.
------------

v8Preview :: HasCallStack => Types.Profile -> Types.Profile
v8Preview = genesis
  (\g ->
    if any (== "v8-preview") (Types.pparamsOverlays g)
    then error "v8Preview: `pparamsOverlays` already has \"v8-preview\"."
    else g {Types.pparamsOverlays = Types.pparamsOverlays g ++ ["v8-preview"]}
  )

v9Preview :: HasCallStack => Types.Profile -> Types.Profile
v9Preview = genesis
  (\g ->
    if any (== "v9-preview") (Types.pparamsOverlays g)
    then error "v9Preview: `pparamsOverlays` already has \"v9-preview\"."
    else g {Types.pparamsOverlays = Types.pparamsOverlays g ++ ["v9-preview"]}
  )

stepHalf :: HasCallStack => Types.Profile -> Types.Profile
stepHalf = genesis
  (\g ->
    if any (== "stepshalf") (Types.pparamsOverlays g)
    then error "stepHalf: `pparamsOverlays` already has \"stepshalf\"."
    else g {Types.pparamsOverlays = Types.pparamsOverlays g ++ ["stepshalf"]}
  )

doubleBudget :: HasCallStack => Types.Profile -> Types.Profile
doubleBudget = genesis
  (\g ->
    if any (== "doublebudget") (Types.pparamsOverlays g)
    then error "doubleBudget: `pparamsOverlays` already has \"doublebudget\"."
    else g {Types.pparamsOverlays = Types.pparamsOverlays g ++ ["doublebudget"]}
  )

-- Customize genesis.
---------------------

shelley :: (Aeson.Object -> Aeson.Object) -> Types.Profile -> Types.Profile
shelley f = genesis (\g -> g {Types.shelley = f (Types.shelley g)})

alonzo :: (Aeson.Object -> Aeson.Object) -> Types.Profile -> Types.Profile
alonzo f = genesis (\g -> g {Types.shelley = f (Types.alonzo g)})

conway :: (Maybe Aeson.Object -> Maybe Aeson.Object) -> Types.Profile -> Types.Profile
conway f = genesis (\g -> g {Types.conway = f (Types.conway g)})

-- Time/block params.
---------------------

slotDuration :: HasCallStack => Time.NominalDiffTime -> Types.Profile -> Types.Profile
slotDuration i = genesis
  (\g ->
    if (Types.slot_duration g) /= 0
    then error "slotDuration: `slot_duration` already set (not zero)."
    else g {Types.slot_duration = i}
  )

epochLength :: HasCallStack => Integer -> Types.Profile -> Types.Profile
epochLength i = genesis
  (\g ->
    if (Types.epoch_length g) /= 0
    then error "epochLength: `epoch_length` already set (not zero)."
    else g {Types.epoch_length = i}
  )

activeSlotsCoeff :: HasCallStack => Scientific.Scientific -> Types.Profile -> Types.Profile
activeSlotsCoeff s = genesis
  (\g ->
    if (Types.active_slots_coeff g) /= 0
    then error "activeSlotsCoeff: `active_slots_coeff` already set (not zero)."
    else g {Types.active_slots_coeff = s}
  )

parameterK :: HasCallStack => Integer -> Types.Profile -> Types.Profile
parameterK i = genesis
  (\g ->
    if (Types.parameter_k g) /= 0
    then error "parameterK: `parameter_k` already set (not zero)."
    else g {Types.parameter_k = i}
  )

-- Genesis size.
----------------

utxo :: HasCallStack => Integer -> Types.Profile -> Types.Profile
utxo i = genesis
  (\g ->
    if (Types.utxo g) /= 0
    then error "utxo: `utxo` already set (not zero)."
    else g {Types.utxo = i}
  )

delegators :: HasCallStack => Integer -> Types.Profile -> Types.Profile
delegators i = genesis
  (\g ->
    if (Types.delegators g) /= Nothing
    then error "delegators: `delegators` already set (not Nothing)."
    else g {Types.delegators = (Just i)}
  )

dreps :: HasCallStack => Integer -> Types.Profile -> Types.Profile
dreps i = genesis
  (\g ->
    if (Types.dreps g) /= 0
    then error "dreps: `dreps` already set (not zero)."
    else g {Types.dreps = i}
  )

extraFutureOffset :: HasCallStack => Time.NominalDiffTime -> Types.Profile -> Types.Profile
extraFutureOffset i = genesis
  (\g ->
    if (Types.extra_future_offset g) /= 0
    then error "extraFutureOffset: `extra_future_offset` already set (not zero)."
    else g {Types.extra_future_offset = i}
  )

-- Generator funds.
-------------------

poolBalance :: HasCallStack => Integer -> Types.Profile -> Types.Profile
poolBalance i = genesis
  (\g ->
    if (Types.per_pool_balance g) /= 0
    then error "poolBalance: `per_pool_balance` already set (not zero)."
    else g {Types.per_pool_balance = i}
  )

funds :: HasCallStack => Integer -> Types.Profile -> Types.Profile
funds i = genesis
  (\g ->
    if (Types.funds_balance g) /= 0
    then error "funds: `funds_balance` already set (not zero)."
    else g {Types.funds_balance = i}
  )

-- Node.
--------------------------------------------------------------------------------

node :: (Types.Node -> Types.Node) -> Types.Profile -> Types.Profile
node f p = p {Types.node = f (Types.node p)}

lmdb :: Types.Profile -> Types.Profile
lmdb = node (\n -> n {Types.utxo_lmdb = True})

-- P2P.
-------

p2pOn :: HasCallStack => Types.Profile -> Types.Profile
p2pOn = node
  (\n ->
    if (Types.verbatim n) /= (Types.NodeVerbatim Nothing)
    then error "p2pOn: `verbatim` already set (not Nothing)."
    else n {Types.verbatim = Types.NodeVerbatim (Just True)}
  )

p2pOff :: HasCallStack => Types.Profile -> Types.Profile
p2pOff = node
  (\n ->
    if (Types.verbatim n) /= (Types.NodeVerbatim Nothing)
    then error "p2pOff: `verbatim` already set (not Nothing)."
    else n {Types.verbatim = Types.NodeVerbatim Nothing}
  )

-- Tracer.
----------

tracerOn :: Types.Profile -> Types.Profile
tracerOn = node (\n -> n {Types.nodeTracer = True})

tracerOff :: Types.Profile -> Types.Profile
tracerOff = node (\n -> n {Types.nodeTracer = False})

newTracing :: HasCallStack => Types.Profile -> Types.Profile
newTracing = node
  (\n ->
    if (Types.tracing_backend n) /= ""
    then error "newTracing: `tracing_backend` already set (not empty)."
    else n {Types.tracing_backend = "trace-dispatcher"}
  )

oldTracing :: HasCallStack => Types.Profile -> Types.Profile
oldTracing = node
  (\n ->
    if (Types.tracing_backend n) /= ""
    then error "oldTracing: `tracing_backend` already set (not empty)."
    else n {Types.tracing_backend = "iohk-monitoring"}
  )

-- "--shutdown-on-*".
---------------------

shutdownOnSlot :: HasCallStack => Integer -> Types.Profile -> Types.Profile
shutdownOnSlot slot = node
  (\n ->
    if
         (Types.shutdown_on_slot_synced  n) /= Nothing
      || (Types.shutdown_on_block_synced n) /= Nothing
    then error "shutdownOnSlot: `shutdown_on_slot_synced` or `shutdown_on_block_synced` already set."
    else n {Types.shutdown_on_slot_synced = Just slot}
  )

shutdownOnBlock :: HasCallStack => Integer -> Types.Profile -> Types.Profile
shutdownOnBlock block = node
  (\n ->
    if
         (Types.shutdown_on_slot_synced  n) /= Nothing
      || (Types.shutdown_on_block_synced n) /= Nothing
    then error "shutdownOnBlock: `shutdown_on_slot_synced` or `shutdown_on_block_synced` already set."
    else n {Types.shutdown_on_block_synced = Just block}
  )

shutdownOnOff :: HasCallStack => Types.Profile -> Types.Profile
shutdownOnOff = node
  (\n ->
    if
         (Types.shutdown_on_slot_synced  n) /= Nothing
      || (Types.shutdown_on_block_synced n) /= Nothing
    then error "shutdownOnOff: `shutdown_on_slot_synced` or `shutdown_on_block_synced` already set."
    else
      n {
        Types.shutdown_on_slot_synced = Nothing
      , Types.shutdown_on_block_synced = Nothing
      }
  )

-- RTS params.
--------------

rtsAppend :: String -> Types.Profile -> Types.Profile
rtsAppend str = node (\n -> n {Types.rts_flags_override = (Types.rts_flags_override n) ++ [str]})

rtsGcNonMoving :: Types.Profile -> Types.Profile
rtsGcNonMoving = rtsAppend "-xn"

rtsGcAllocSize :: Integer -> Types.Profile -> Types.Profile
rtsGcAllocSize size = rtsAppend $ "-A" ++ (show size) ++ "m"

rtsThreads :: Integer -> Types.Profile -> Types.Profile
rtsThreads n = rtsAppend $ "-N" ++ (show n)

rtsHeapLimit :: Integer -> Types.Profile -> Types.Profile
rtsHeapLimit n = rtsAppend $ "-M" ++ (show n) ++ "m"

heapLimit :: HasCallStack => Integer -> Types.Profile -> Types.Profile
heapLimit l = node
  (\n ->
    if (Types.heap_limit n) /= Nothing
    then error "heapLimit: `heap_limit` already set (not Nothing)."
    else n {Types.heap_limit = Just l}
  )

-- Generator.
--------------------------------------------------------------------------------

generator :: (Types.Generator -> Types.Generator) -> Types.Profile -> Types.Profile
generator f p = p {Types.generator = f (Types.generator p)}

tps :: HasCallStack => Scientific.Scientific -> Types.Profile -> Types.Profile
tps i = generator
  (\g ->
    if (Types.tps g) /= 0
    then error "tps: `tps` already set (not zero)."
    else g {Types.tps = i}
  )

txIn :: HasCallStack => Integer -> Types.Profile -> Types.Profile
txIn i = generator
  (\g ->
    if (Types.inputs_per_tx g) /= 0
    then error "txIn: `inputs_per_tx` already set (not zero)."
    else g {Types.inputs_per_tx = i}
  )

txOut :: HasCallStack => Integer -> Types.Profile -> Types.Profile
txOut i = generator
  (\g ->
    if (Types.outputs_per_tx g) /= 0
    then error "txOut: `outputs_per_tx` already set (not zero)."
    else g {Types.outputs_per_tx = i}
  )

txFee :: HasCallStack => Integer -> Types.Profile -> Types.Profile
txFee i = generator
  (\g ->
    if (Types.tx_fee g) /= 0
    then error "txFee: `tx_fee` already set (not zero)."
    else g {Types.tx_fee = i}
  )

initCooldown :: HasCallStack => Integer -> Types.Profile -> Types.Profile
initCooldown i = generator
  (\g ->
    if (Types.init_cooldown g) /= 0
    then error "initCooldown: `init_cooldown` already set (not zero)."
    else g {Types.init_cooldown = i}
  )

plutus :: (Types.Plutus -> Types.Plutus) -> Types.Profile -> Types.Profile
plutus f = generator (\g -> g {Types.plutus = f (Types.plutus g)})

plutusType :: String -> Types.Profile -> Types.Profile
plutusType s = plutus (\p -> p {Types.plutusType = Just s})

plutusScript :: String -> Types.Profile -> Types.Profile
plutusScript s = plutus (\p -> p {Types.plutusScript = Just s})

redeemerInt :: Integer -> Types.Profile -> Types.Profile
redeemerInt i = plutus
  (\p -> p {Types.redeemer = Just (
    Types.Redeemer (Just i) Nothing Nothing
  )})

redeemerFields :: [Aeson.Object] -> Types.Profile -> Types.Profile
redeemerFields objs = plutus
  (\p -> p {Types.redeemer = Just (
    Types.Redeemer Nothing (Just 0) (Just objs)
  )})

generatorEpochs :: HasCallStack => Integer -> Types.Profile -> Types.Profile
generatorEpochs i = generator
  (\g ->
    if (Types.epochs g) /= 0
    then error "generatorEpochs: `epochs` already set (not zero)."
    else g {Types.epochs = i}
  )

-- Tracer.
--------------------------------------------------------------------------------

tracer :: (Types.Tracer -> Types.Tracer) -> Types.Profile -> Types.Profile
tracer f p = p {Types.tracer = f (Types.tracer p)}

tracerRtview :: Types.Profile -> Types.Profile
tracerRtview = tracer (\t -> t {Types.rtview = True})

tracerWithresources :: Types.Profile -> Types.Profile
tracerWithresources = tracer (\t -> t {Types.withresources = True})

-- Cluster.
--------------------------------------------------------------------------------

cluster :: (Types.Cluster -> Types.Cluster) -> Types.Profile -> Types.Profile
cluster f p = p {Types.cluster = f (Types.cluster p)}

clusterMinimunStorage :: (Maybe (Types.ByNodeType Int)) -> Types.Profile -> Types.Profile
clusterMinimunStorage ms = cluster (\c -> c {Types.minimun_storage = ms})

ssdDirectory :: String -> Types.Profile -> Types.Profile
ssdDirectory str = cluster (\c -> c {Types.ssd_directory = Just str})

clusterKeepRunningOn :: Types.Profile -> Types.Profile
clusterKeepRunningOn = cluster (\c -> c {Types.keep_running = True})

nomad :: (Types.ClusterNomad -> Types.ClusterNomad) -> Types.Profile -> Types.Profile
nomad f p = cluster (\c -> c {Types.nomad = f (Types.nomad c)}) p

nomadNamespace :: String -> Types.Profile -> Types.Profile
nomadNamespace ns = nomad (\n -> n {Types.namespace = ns})

nomadClass :: String -> Types.Profile -> Types.Profile
nomadClass nc = nomad (\n -> n {Types.nomad_class = nc})

nomadResources :: (Types.ByNodeType Types.Resources) -> Types.Profile -> Types.Profile
nomadResources r = nomad (\n -> n {Types.resources = r})

nomadHostVolume :: Types.HostVolume -> Types.Profile -> Types.Profile
nomadHostVolume hv = nomad (\n ->
    let mhvs = case Types.host_volumes n of
                 Nothing -> Just [hv]
                 (Just hvs) -> Just $ hvs ++ [hv]
    in n {Types.host_volumes = mhvs}
  )

nomadSSHLogsOn :: Types.Profile -> Types.Profile
nomadSSHLogsOn = nomad (\n -> n {Types.fetch_logs_ssh = True})

aws :: (Types.ClusterAWS -> Types.ClusterAWS) -> Types.Profile -> Types.Profile
aws f p = cluster (\c -> c {Types.aws = f (Types.aws c)}) p

awsInstanceTypes :: (Types.ByNodeType String) -> Types.Profile -> Types.Profile
awsInstanceTypes i = aws (\n -> n {Types.instance_type = i})

usePublicRouting :: Types.Profile -> Types.Profile
usePublicRouting = aws (\n -> n {Types.use_public_routing = True})

-- Analysis.
--------------------------------------------------------------------------------

analysis :: (Types.Analysis -> Types.Analysis) -> Types.Profile -> Types.Profile
analysis f p = p {Types.analysis = f (Types.analysis p)}

-- Analysis type:
-----------------

analysisOff :: Types.Profile -> Types.Profile
analysisOff = analysis (\a -> a {Types.analysisType = Nothing})

analysisStandard :: Types.Profile -> Types.Profile
analysisStandard = analysis (\a -> a {Types.analysisType = Just "standard"})

analysisPerformance :: Types.Profile -> Types.Profile
analysisPerformance = analysis (\a -> a {Types.analysisType = Just "performance"})

-- Analysis filters:
--------------------

analysisFiltersAppend :: String -> Types.Profile -> Types.Profile
analysisFiltersAppend str = analysis (\a -> a {Types.filters = (Types.filters a) ++ [str]})

analysisSizeSmall :: Types.Profile -> Types.Profile
analysisSizeSmall = analysisFiltersAppend "size-small"

analysisSizeModerate :: Types.Profile -> Types.Profile
analysisSizeModerate = analysisFiltersAppend "size-moderate"

analysisSizeModerate2 :: Types.Profile -> Types.Profile
analysisSizeModerate2 = analysisFiltersAppend "size-moderate-2"

analysisSizeFull :: Types.Profile -> Types.Profile
analysisSizeFull = analysisFiltersAppend "size-full"

analysisUnitary :: Types.Profile -> Types.Profile
analysisUnitary = analysisFiltersAppend "unitary"

analysisEpoch3Plus :: Types.Profile -> Types.Profile
analysisEpoch3Plus = analysisFiltersAppend "epoch3+"

-- Analysis expressions:
------------------------

analysisExpressionAppend :: Types.AnalysisFilterExpression -> Types.Profile -> Types.Profile
analysisExpressionAppend expr = analysis (\a -> a {Types.filter_exprs = (Types.filter_exprs a) ++ [expr]})

cBlockMinimumAdoptions :: Integer -> Types.Profile -> Types.Profile
cBlockMinimumAdoptions i = analysisExpressionAppend
  (Types.AnalysisFilterExpression
    "CBlock"
    (Types.AnalysisFilterExpressionContent "BMinimumAdoptions" i)
  )

{-- Calculated when constructing the "derive" property:
clusterStartupOverhead :: HasCallStack => Time.NominalDiffTime -> Types.Profile -> Types.Profile
clusterStartupOverhead s = analysis
  (\a ->
    if (Types.cluster_startup_overhead_s a) /= 0
    then error "clusterStartupOverhead: `cluster_startup_overhead_s` already set (not zero)."
    else a {Types.cluster_startup_overhead_s = s}
  )

minimumChainDensity :: HasCallStack => Scientific.Scientific -> Types.Profile -> Types.Profile
minimumChainDensity s = analysis
  (\a ->
    if (Types.minimum_chain_density a) /= 0
    then error "minimumChainDensity: `minimum_chain_density` already set (not zero)."
    else a {Types.minimum_chain_density = s}
  )
--}

-- Derived.
--------------------------------------------------------------------------------

shelleyAlonzoConway :: Integer -> Types.Profile -> Types.Profile
shelleyAlonzoConway epochNumber p =
  let epochParams  = unsafePerformIO $ epochTimeline epochNumber
      epochParams' = unsafePerformIO $ foldM
        (\acc overlayName -> overlay overlayName acc)
        epochParams
        (Types.pparamsOverlays $ Types.genesis p)
  in genesis (\g -> g {
      Types.pparamsEpoch = epochNumber
    , Types.shelley =
        let shey' = KeyMap.fromList [
               ("slotLength", Aeson.Number $ realToFrac $ Types.slot_duration g)
             , ("epochLength", Aeson.Number $ fromInteger $ Types.epoch_length g)
             , ("securityParam", Aeson.Number $ fromInteger $ Types.parameter_k g)
             , ("activeSlotsCoeff", Aeson.Number $ Types.active_slots_coeff g)
             , ("protocolParams",
                 case KeyMap.lookup "shelley" epochParams' of
                   (Just shey) -> shey
                   _ -> error $ "Obtained no \"shelley\" from epoch-timeline.json"
               )
             ]
        -- Any property that was set before by the user takes precedence.
        in KeyMap.unionWithKey unionWithKey shey' (Types.shelley g)
    , Types.alonzo  =
        case KeyMap.lookup "alonzo" epochParams' of
          (Just (Aeson.Object alzo)) -> alzo
          _ -> error $ "Obtained no \"alonzo\" from epoch-timeline.json"
    , Types.conway  =
        case KeyMap.lookup "conway" epochParams' of
          (Just (Aeson.Object coay)) -> Just coay
          _ -> Nothing
    }) p

epochTimeline :: Integer -> IO (KeyMap.KeyMap Aeson.Value)
epochTimeline epochNumber = do
  fp <- Paths.getDataFileName "data/genesis/epoch-timeline.json"
  eitherValue <- Aeson.eitherDecodeFileStrict fp
  return $ case eitherValue of
    (Right (Aeson.Object keyMap)) -> foldl
      (\acc key ->
        -- TODO: It will fail if then number used as string key is above 999.
        if key <= read ("\"" ++ show epochNumber ++ "\"")
        then case KeyMap.lookup key keyMap of
          (Just (Aeson.Object obj)) ->
            -- Right-biased merge of both JSON objects at all depths.
            KeyMap.unionWithKey unionWithKey acc obj
          _ -> error "Key not an Aeson Object: \"data/epoch-timeline.json\""
        else acc
      )
      mempty
      (sort $ KeyMap.keys keyMap)
    _ -> error "Not an Aeson Object: \"data/epoch-timeline.json\""
  --_ <- error (show $ KeyMap.lookup "shelley" epochParams)

overlay :: String -> (KeyMap.KeyMap Aeson.Value) -> IO (KeyMap.KeyMap Aeson.Value)
overlay overlayName epochParams = do
  let dataFileName = "data/genesis/overlays/" ++ overlayName ++ ".json"
  fp <- Paths.getDataFileName dataFileName
  eitherValue <- Aeson.eitherDecodeFileStrict fp
  return $ case eitherValue of
    (Right (Aeson.Object keyMap)) ->
      -- Right-biased merge of both JSON objects at all depths.
      KeyMap.unionWithKey unionWithKey epochParams keyMap
    _ -> error $ "Not an Aeson Object: \"" ++ fp ++ "\""

-- Right-biased merge of both JSON objects at all depths.
unionWithKey :: KeyMap.Key -> Aeson.Value -> Aeson.Value -> Aeson.Value
-- Recurse if it's an object.
unionWithKey _ (Aeson.Object a) (Aeson.Object b) =
  Aeson.Object $ KeyMap.unionWithKey unionWithKey a b
-- If not an object prefer the right value.
unionWithKey _ _ b = b

derive :: Types.Profile -> Types.Profile
derive p@(Types.Profile _ _ _ comp _era gsis n gtor _ _ ana _ _ _ _) =
  let 
      -- Absolute/epoch durations:
      ----------------------------
      slot_duration      = Types.slot_duration gsis -- NominalDiffTime
      slots_per_epoch    = Types.epoch_length  gsis -- Integer
      -- Derived NominalDiffTime.
      epoch_duration     = (fromInteger slots_per_epoch) * slot_duration

      -- Block/probable durations:
      ----------------------------
      active_slots_coeff = Types.active_slots_coeff gsis -- Scientific
      -- Helper Scientific.
      block_per_epoch = (fromInteger slots_per_epoch) * active_slots_coeff
      -- Helper NominalDiffTime.
      _block_duration = slot_duration / (realToFrac active_slots_coeff)

      -- Effective durations:
      -----------------------
      -- If the nodes have a "--shutdown-on-*" parameter we use this to
      -- calculate the epochs of the run, if there is none, we use the epochs
      -- from the generator config.
      -- Force it to a Double or get an error:
      -- -- `fromRational` has been applied to a repeating decimal
      requested_epochs = -- A Double.
        case Types.shutdown_on_slot_synced n of
          (Just shutdown_slots) ->
            (fromInteger shutdown_slots) / (fromInteger slots_per_epoch)
          Nothing -> case Types.shutdown_on_block_synced n of
            (Just shutdown_blocks) ->
              (fromInteger shutdown_blocks) / (realToFrac block_per_epoch)
            -- If it does not come from the node, it comes from the generator.
            Nothing -> fromInteger $ Types.epochs gtor
      -- Derived Integer.
      -- 1.0001 epochs is a 2 epochs profile!
      requested_epochs_ceiling = ceiling $ (requested_epochs :: Double)

      -- Generator:
      -------------
      tps_ = Types.tps gtor -- Scientific
      generator_requested_duration = -- NominalDiffTime
        epoch_duration * (fromInteger requested_epochs_ceiling)
      -- Helpers NominalDiffTime.
      (generator_duration, maybe_shutdown_time) =
        case Types.shutdown_on_slot_synced n of
          (Just shutdown_slots) ->
            let shutdown_time = (fromInteger shutdown_slots) * slot_duration
            in (min generator_requested_duration shutdown_time, Just shutdown_time)
          Nothing -> (generator_requested_duration, Nothing)
      -- Integer.
      generator_tx_count = ceiling $ case Types.tx_count gtor of
        Just tx_count -> fromInteger tx_count
        Nothing -> (realToFrac generator_duration) * tps_

      -- UTxO:
      --------
      (effective_delegators, delegators_effective) =
        case Types.delegators gsis of
          (Just d) -> (d, max d (Types.n_pools comp))
          Nothing -> (Types.n_pools comp, Types.n_pools comp)
      utxo_generated = generator_tx_count * Types.inputs_per_tx gtor
      utxo_stuffed = max 0 (Types.utxo gsis)

      -- Dataset:
      -----------
      dataset_measure =
        if Types.utxo gsis == 0
        then 0
        else case Types.delegators gsis of
               (Just d) -> Types.utxo gsis + d
               Nothing -> Types.utxo gsis
      -- NominalDiffTime.
      dataset_induced_startup_delay_optimistic =
        if dataset_measure < 10000
        then Types.cluster_base_startup_overhead_s ana
        else (fromInteger dataset_measure) / 50000
      -- NominalDiffTime
      dataset_induced_startup_delay_conservative =
        if dataset_measure < 10000
        then Types.cluster_base_startup_overhead_s ana
        else (fromInteger dataset_measure) / 2500
      -- NominalDiffTime.
      genesis_future_offset =
          dataset_induced_startup_delay_optimistic
        + (Types.extra_future_offset gsis)

        -- Supply:
        ----------
      supply_delegated = Types.per_pool_balance gsis * Types.n_pools comp
      supply_total = supply_delegated + Types.funds_balance gsis

      -- Size:
      --------
      -- genesis.shelley.protocolParams.maxBlockBodySize
      maxBlockBodySize =
        case KeyMap.lookup "protocolParams" (Types.shelley gsis) of
          (Just (Aeson.Object pparams)) ->
            case KeyMap.lookup "maxBlockBodySize" pparams of
              (Just (Aeson.Number scientific)) -> scientific
              _ -> error "No \"maxBlockBodySize\" Number"
          _ -> error "No \"shelley.protocolParams\" object"
      -- XXX:  this is corruption at the highest levels, pure and simple.
      default_value_tx_size_estimate = (381 :: Integer) -- Bytes?
      default_value_tx_per_block_estimate = floor $
        (realToFrac maxBlockBodySize :: Double) / (fromInteger default_value_tx_size_estimate)
      generator_blocks_lower_bound = ceiling $
        (1.15 :: Double) * (fromInteger generator_tx_count) / (fromInteger default_value_tx_per_block_estimate)

  in analysis
      (\a -> a
        {
          Types.minimum_chain_density = active_slots_coeff * 0.5
        , Types.cluster_startup_overhead_s = dataset_induced_startup_delay_conservative
        }
      )
      (p
        {
          Types.derived = Types.Derived {
          -- Duration:
            Types.effective_epochs = requested_epochs_ceiling
          , Types.epoch_duration = epoch_duration
          -- Data set:
          , Types.dataset_measure = dataset_measure
          , Types.delegators_effective = delegators_effective
          , Types.genesis_future_offset = genesis_future_offset

          , Types.generator_duration = generator_duration
          , Types.generator_tx_count = generator_tx_count

          , Types.supply_total = supply_total
          , Types.supply_delegated = supply_delegated

          , Types.utxo_delegated = effective_delegators
          , Types.utxo_generated = utxo_generated
          , Types.utxo_stuffed = utxo_stuffed

          , Types.shutdown_time = maybe_shutdown_time
          -- XXX:  this is corruption at the highest levels, pure and simple.
          , Types.default_value_tx_size_estimate = default_value_tx_size_estimate
          , Types.default_value_tx_per_block_estimate = default_value_tx_per_block_estimate
          , Types.generator_blocks_lower_bound = generator_blocks_lower_bound

          , Types.dataset_induced_startup_delay_optimistic = dataset_induced_startup_delay_optimistic
          , Types.dataset_induced_startup_delay_conservative = dataset_induced_startup_delay_conservative
          }
        }
      )

{--
  { createStakedArgs:
    ([ "--testnet-magic",          $p.genesis.network_magic
     , "--supply",                 fmt_decimal_10_5($p.genesis.funds_balance)
     , "--gen-utxo-keys",          1
     , "--gen-genesis-keys",       $p.composition.n_bft_hosts
     , "--supply-delegated",       fmt_decimal_10_5($p.derived.supply_delegated)
     , "--gen-pools",              $p.composition.n_pools
     , "--gen-stake-delegs",       $p.derived.delegators_effective
     , "--num-stuffed-utxo",       fmt_decimal_10_5($p.derived.utxo_stuffed)
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [ "--bulk-pool-cred-files",   $p.composition.n_dense_hosts
     , "--bulk-pools-per-file",    $p.composition.dense_pool_density ]
     else [] end)
  , createTestnetDataArgs:
    ([ "--testnet-magic",          $p.genesis.network_magic
     , "--total-supply",           fmt_decimal_10_5($p.genesis.funds_balance + $p.derived.supply_delegated)
     , "--utxo-keys",              1
     , "--genesis-keys",           $p.composition.n_bft_hosts
     , "--delegated-supply",       fmt_decimal_10_5($p.derived.supply_delegated)
     , "--pools",                  $p.composition.n_pools
     , "--stake-delegators",       $p.derived.delegators_effective
     , "--drep-keys",              $p.genesis.dreps
     , "--stuffed-utxo",           fmt_decimal_10_5($p.derived.utxo_stuffed)
     ])
  , pools:
    [ "--argjson"
    , "initialPoolCoin",           fmt_decimal_10_5($p.genesis.pool_coin)
    ]
  }
--}

cliArgs :: Types.Profile -> Types.Profile
cliArgs p@(Types.Profile _ _ _ comp __ gsis _ _ _ _ _ dved _ _ _) =
  let --toJson = map (\(k,n) -> )
      fmtDecimal i =
           (Scientific.formatScientific Scientific.Fixed (Just 0) ((fromInteger i) / 100000))
        ++ "00000"
      createStakedArgs =
        [
          Aeson.String "--testnet-magic",    Aeson.Number $ fromInteger $ Types.network_magic gsis
        , Aeson.String "--supply",           Aeson.String $ Text.pack $ fmtDecimal $ Types.funds_balance gsis
        , Aeson.String "--gen-utxo-keys",    Aeson.Number $ fromInteger $ 1
        , Aeson.String "--gen-genesis-keys", Aeson.Number $ fromInteger $ Types.n_bft_hosts comp
        , Aeson.String "--supply-delegated", Aeson.String $ Text.pack $ fmtDecimal $ Types.supply_delegated dved
        , Aeson.String "--gen-pools",        Aeson.Number $ fromInteger $ Types.n_pools comp
        , Aeson.String "--gen-stake-delegs", Aeson.Number $ fromInteger $ Types.delegators_effective dved
        , Aeson.String "--num-stuffed-utxo", Aeson.String $ Text.pack $ fmtDecimal $ Types.utxo_stuffed dved
        ]
        ++
        if Types.dense_pool_density comp /= 1
        then
          [ 
            Aeson.String "--bulk-pool-cred-files", Aeson.Number $ fromInteger $ Types.n_dense_hosts comp
          , Aeson.String "--bulk-pools-per-file",  Aeson.Number $ fromInteger $ Types.dense_pool_density comp
          ]
        else []
      createTestnetDataArgs =
        [
          Aeson.String "--testnet-magic",    Aeson.Number $ fromInteger $ Types.network_magic gsis
        , Aeson.String "--total-supply",     Aeson.String $ Text.pack $ fmtDecimal $ (Types.funds_balance gsis) + (Types.supply_delegated dved)
        , Aeson.String "--utxo-keys",        Aeson.Number $ fromInteger $ 1
        , Aeson.String "--genesis-keys",     Aeson.Number $ fromInteger $ Types.n_bft_hosts comp
        , Aeson.String "--delegated-supply", Aeson.String $ Text.pack $ fmtDecimal $ Types.supply_delegated dved
        , Aeson.String "--pools",            Aeson.Number $ fromInteger $ Types.n_pools comp
        , Aeson.String "--stake-delegators", Aeson.Number $ fromInteger $ Types.delegators_effective dved
        -- TODO: Add the dreps profiles.
        , Aeson.String "--drep-keys",        Aeson.Number $ fromInteger $ Types.dreps gsis
        , Aeson.String "--stuffed-utxo",     Aeson.String $ Text.pack $ fmtDecimal $ Types.utxo_stuffed dved
        ]
      poolsArgs =
        [
          Aeson.String "--argjson"
        , Aeson.String "initialPoolCoin",    Aeson.String $ Text.pack $ fmtDecimal $ Types.pool_coin gsis
        ]
  in  p {Types.cli_args = Types.CliArgs {
            Types.createStakedArgs = createStakedArgs
          , Types.createTestnetDataArgs = createTestnetDataArgs
          , Types.pools = poolsArgs
        }}

--------------------------------------------------------------------------------

preset :: String -> Types.Profile -> Types.Profile
preset str = \p ->
  if (Types.preset p) /= Nothing
  then error "preset: `preset` already set (not Nothing)."
  else p {Types.preset = Just str}
