{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use camelCase" -}

--------------------------------------------------------------------------------
{-

A function, `empty`, that creates a `Profile` as empty as possible (zeroes,
False, "" and Nothing when possible) and many other functions that receive a
`Profile` and return a `Profile`. All these other functions are the primitives,
everything needed to construct a profile and should be kept simple.

For example: empty & name "my local profile" . idle . hosts 2 . loopback

Defaults and derived values are not part of this module.

-}
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Primitives (

    empty

  -- Name and description.
  , name, desc, descAdd

  -- Scenario.
  , idle, tracerOnly, fixedLoaded, chainsync

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
  , v8Preview, v9Preview, v10Preview
  -- Budget overlays:
  -- -- Block:
  -- -- -- Steps:
  , budgetBlockStepsOneAndAHalf, budgetBlockStepsDouble
  -- -- -- Memory:
  , budgetBlockMemoryOneAndAHalf, budgetBlockMemoryDouble
  -- -- TX:

  -- Others
  , blocksize64k
  , voting
 -- Customize the "shelley", "alonzo" or "conway" properties.
  , shelley, alonzo, conway
  -- Time and block params.
  , slotDuration, epochLength, activeSlotsCoeff, parameterK
  -- Genesis size.
  , utxo, delegators, dreps
  -- If the genesis file is big more time is needed for deployment.
  , extraFutureOffset
  -- Funds for the generator.
  , poolBalance, funds, utxoKeys
  -- Others.
  , maxBlockSize

  -- ChainDB
  , chaindb

  -- Node
  -- LMDB True or False.
  , lmdb
  -- Node's p2p flag.
  , p2pOn, p2pOff
  -- Node's tracer flag.
  , traceForwardingOn, traceForwardingOff
  -- Node's tracer type.
  , newTracing, oldTracing
  -- Node's --shutdown-on-*-sync.
  , shutdownOnSlot, shutdownOnBlock, shutdownOnOff
  -- Node's RTS params.
  , rtsGcNonMoving, rtsGcAllocSize, rtsGcParallel, rtsGcLoadBalance
  , rtsThreads, rtsHeapLimit, rtsEventlogged, rtsHeapProf
  , heapLimit

  -- Generator params.
  , tps, txIn, txOut, txFee, txFeeOverwrite, initCooldown
  , plutusType, plutusScript
  , redeemerInt, redeemerFields
  , generatorEpochs

  -- Workload params.
  , workloadAppend

  -- Tracer's params.
  , tracerRtview, tracerWithresources

  -- Cluster params.
  , clusterMinimunStorage, ssdDirectory, clusterKeepRunningOn
  , nomadNamespace, nomadClass, nomadResources, nomadHostVolume, nomadSSHLogsOn
  , awsInstanceTypes, usePublicRouting

  -- Analysis params.
  , analysisOff, analysisStandard, analysisPerformance
  , analysisSizeSmall, analysisSizeModerate, analysisSizeModerate2, analysisSizeFull
  , analysisUnitary, analysisEpoch3Plus, analysisEpoch4Plus, analysisEpoch5Plus
  , cBlockMinimumAdoptions

  , preset
  , overlay

) where

import           Prelude hiding (id)
import           Data.Maybe (isJust)
import           GHC.Stack (HasCallStack)
-- Package: aeson.
import qualified Data.Aeson           as Aeson
-- Package: scientific.
import qualified Data.Scientific as Scientific
-- Package: time.
import qualified Data.Time as Time
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types

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
  , Types.era = Types.Conway
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
    , Types.delegators = 0
    , Types.dreps = 0
    , Types.extra_future_offset = 0
    , Types.per_pool_balance = 0
    , Types.funds_balance = 0
    , Types.utxo_keys = 0
    , Types.network_magic = 0
    , Types.pool_coin = 0
    , Types.delegator_coin = 0
    , Types.single_shot = False
    , Types.max_block_size = Nothing
  }
  , Types.chaindb = Nothing
  , Types.node = Types.Node {
      Types.utxo_lmdb = False
    , Types.ssd_directory = Nothing
    , Types.verbatim = Types.NodeVerbatim Nothing
    , Types.trace_forwarding = False
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
  , Types.workloads = []
  , Types.tracer = Types.Tracer {
      Types.rtview = False
    , Types.ekg = False
    , Types.withresources = False
  }
  , Types.cluster = Nothing
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
name str p =
  if Types.name p /= ""
  then error "name: `name` already set (not empty)."
  else p {Types.name = str}

desc :: HasCallStack => String -> Types.Profile -> Types.Profile
desc str p =
  if isJust (Types.desc p)
  then error "desc: `desc` already set (not Nothing)."
  else p {Types.desc = Just str}

descAdd :: HasCallStack => String -> Types.Profile -> Types.Profile
descAdd str p =
  case Types.desc p of
    Just s  -> p {Types.desc = Just (s <> str) }
    Nothing -> error "descAdd: `desc` is not set."

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
    if Types.locations c /= []
    then error "loopback: `locations` already set (not empty)."
    else c {Types.locations = [Types.Loopback]}
  )

regions :: HasCallStack => [Types.Location] -> Types.Profile -> Types.Profile
regions locs = composition
  (\c ->
    if Types.locations c /= []
    then error "regions: `locations` already set (not empty)."
    else c {Types.locations = locs}
  )

-- Size.
--------

hosts :: HasCallStack => Integer -> Types.Profile -> Types.Profile
hosts size = composition
  (\c ->
    -- "dense_pool_density" is set by `hosts`, `pools` and `hostsChainsync`.
    if Types.dense_pool_density c /= 0
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
    if Types.dense_pool_density c /= 0
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
    if Types.dense_pool_density c /= 0
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

-- NB. these primitives should correspond to filenames data/genesis/overlays/*.json

v8Preview :: HasCallStack => Types.Profile -> Types.Profile
v8Preview = helper_addOverlayOrDie "v8-preview"

-- TODO: refactor definitions so that v9-preview always implies v8-preview,
--       meaning this can be defined as
--       v9Preview = helper_addOverlayOrDie "v9-preview" . v8Preview
v9Preview :: HasCallStack => Types.Profile -> Types.Profile
v9Preview = helper_addOverlayOrDie "v9-preview"

v10Preview :: HasCallStack => Types.Profile -> Types.Profile
v10Preview = helper_addOverlayOrDie "v10-preview"

-- Budget:

-- Steps:

budgetBlockStepsOneAndAHalf :: HasCallStack => Types.Profile -> Types.Profile
budgetBlockStepsOneAndAHalf = helper_addOverlayOrDie "budget/block/steps/oneandahalf"

budgetBlockStepsDouble :: HasCallStack => Types.Profile -> Types.Profile
budgetBlockStepsDouble = helper_addOverlayOrDie "budget/block/steps/double"

-- Memory

budgetBlockMemoryOneAndAHalf :: HasCallStack => Types.Profile -> Types.Profile
budgetBlockMemoryOneAndAHalf = helper_addOverlayOrDie "budget/block/memory/oneandahalf"

budgetBlockMemoryDouble :: HasCallStack => Types.Profile -> Types.Profile
budgetBlockMemoryDouble = helper_addOverlayOrDie "budget/block/memory/double"

-- used to manually reduce block size for e.g. Conway; has to be applied *AFTER* any v?-preview overlay.
blocksize64k :: HasCallStack => Types.Profile -> Types.Profile
blocksize64k = helper_addOverlayOrDie "blocksize64k"

voting :: HasCallStack => Types.Profile -> Types.Profile
voting = helper_addOverlayOrDie "voting"

-- ensures a specific overlay is added only once to the list; redudancies point to ill-formed profile specification and thus error out
helper_addOverlayOrDie :: HasCallStack => String -> Types.Profile -> Types.Profile
helper_addOverlayOrDie overlayName = genesis
  (\g ->
    if overlayName `elem` Types.pparamsOverlays g
    then error $ overlayName ++ ": `pparamsOverlays` already has \"" ++ overlayName ++ "\"."
    else g {Types.pparamsOverlays = Types.pparamsOverlays g ++ [overlayName]}
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
    if Types.slot_duration g /= 0
    then error "slotDuration: `slot_duration` already set (not zero)."
    else g {Types.slot_duration = i}
  )

epochLength :: HasCallStack => Integer -> Types.Profile -> Types.Profile
epochLength i = genesis
  (\g ->
    if Types.epoch_length g /= 0
    then error "epochLength: `epoch_length` already set (not zero)."
    else g {Types.epoch_length = i}
  )

activeSlotsCoeff :: HasCallStack => Scientific.Scientific -> Types.Profile -> Types.Profile
activeSlotsCoeff s = genesis
  (\g ->
    if Types.active_slots_coeff g /= 0
    then error "activeSlotsCoeff: `active_slots_coeff` already set (not zero)."
    else g {Types.active_slots_coeff = s}
  )

parameterK :: HasCallStack => Integer -> Types.Profile -> Types.Profile
parameterK i = genesis
  (\g ->
    if Types.parameter_k g /= 0
    then error "parameterK: `parameter_k` already set (not zero)."
    else g {Types.parameter_k = i}
  )

-- Genesis size.
----------------

utxo :: HasCallStack => Integer -> Types.Profile -> Types.Profile
utxo i = genesis
  (\g ->
    if Types.utxo g /= 0
    then error "utxo: `utxo` already set (not zero)."
    else g {Types.utxo = i}
  )

delegators :: HasCallStack => Integer -> Types.Profile -> Types.Profile
delegators i = genesis
  (\g ->
    if Types.delegators g /= 0
    then error "delegators: `delegators` already set (not zero)."
    else g {Types.delegators = i}
  )

dreps :: HasCallStack => Integer -> Types.Profile -> Types.Profile
dreps i = genesis
  (\g ->
    if Types.dreps g /= 0
    then error "dreps: `dreps` already set (not zero)."
    else g {Types.dreps = i}
  )

extraFutureOffset :: HasCallStack => Time.NominalDiffTime -> Types.Profile -> Types.Profile
extraFutureOffset i = genesis
  (\g ->
    if Types.extra_future_offset g /= 0
    then error "extraFutureOffset: `extra_future_offset` already set (not zero)."
    else g {Types.extra_future_offset = i}
  )

-- Genesis funds.
-----------------

poolBalance :: HasCallStack => Integer -> Types.Profile -> Types.Profile
poolBalance i = genesis
  (\g ->
    if Types.per_pool_balance g /= 0
    then error "poolBalance: `per_pool_balance` already set (not zero)."
    else g {Types.per_pool_balance = i}
  )

funds :: HasCallStack => Integer -> Types.Profile -> Types.Profile
funds i = genesis
  (\g ->
    if Types.funds_balance g /= 0
    then error "funds: `funds_balance` already set (not zero)."
    else g {Types.funds_balance = i}
  )

utxoKeys :: HasCallStack => Integer -> Types.Profile -> Types.Profile
utxoKeys i = genesis
  (\g ->
    if Types.utxo_keys g /= 0
    then error "funds: `utxo_keys` already set (not zero)."
    else g {Types.utxo_keys = i}
  )

-- Genesis others.
------------------

maxBlockSize :: HasCallStack => Integer -> Types.Profile -> Types.Profile
maxBlockSize i = genesis
  (\g ->
    if isJust (Types.max_block_size g)
    then error "funds: `max_block_size` already set (not Nothing)."
    else g {Types.max_block_size = Just i}
  )

-- ChainDB.
--------------------------------------------------------------------------------

chaindb :: (Integer, Integer) -> (Integer, Integer) -> Types.Profile -> Types.Profile
chaindb (mainnetServer, mainnetExplorer) (ledgerServer, ledgerExplorer) p =
  p {Types.chaindb = Just (
    Types.ChainDB
      (Types.Chunks mainnetServer mainnetExplorer)
      (Types.Chunks ledgerServer  ledgerExplorer )
  )}

-- Node.
--------------------------------------------------------------------------------

node :: (Types.Node -> Types.Node) -> Types.Profile -> Types.Profile
node f p = p {Types.node = f (Types.node p)}

lmdb :: Types.Profile -> Types.Profile
lmdb = node (\n -> n {Types.utxo_lmdb = True})

ssdDirectory :: String -> Types.Profile -> Types.Profile
ssdDirectory str = node (\n -> n {Types.ssd_directory = Just str})

-- P2P.
-------

p2pOn :: HasCallStack => Types.Profile -> Types.Profile
p2pOn = node
  (\n ->
    if Types.verbatim n /= Types.NodeVerbatim Nothing
    then error "p2pOn: `verbatim` already set (not Nothing)."
    else n {Types.verbatim = Types.NodeVerbatim (Just True)}
  )

p2pOff :: HasCallStack => Types.Profile -> Types.Profile
p2pOff = node
  (\n ->
    if Types.verbatim n /= Types.NodeVerbatim Nothing
    then error "p2pOff: `verbatim` already set (not Nothing)."
    else n {Types.verbatim = Types.NodeVerbatim Nothing}
  )

-- Tracer.
----------

traceForwardingOn :: Types.Profile -> Types.Profile
traceForwardingOn = node (\n -> n {Types.trace_forwarding = True})

traceForwardingOff :: Types.Profile -> Types.Profile
traceForwardingOff = node (\n -> n {Types.trace_forwarding = False})

newTracing :: HasCallStack => Types.Profile -> Types.Profile
newTracing = node
  (\n ->
    if Types.tracing_backend n /= ""
    then error "newTracing: `tracing_backend` already set (not empty)."
    else n {Types.tracing_backend = "trace-dispatcher"}
  )

oldTracing :: HasCallStack => Types.Profile -> Types.Profile
oldTracing = node
  (\n ->
    if Types.tracing_backend n /= ""
    then error "oldTracing: `tracing_backend` already set (not empty)."
    else n {Types.tracing_backend = "iohk-monitoring"}
  )

-- "--shutdown-on-*".
---------------------

shutdownOnSlot :: HasCallStack => Integer -> Types.Profile -> Types.Profile
shutdownOnSlot slot = node
  (\n ->
    if
         isJust (Types.shutdown_on_slot_synced  n)
      || isJust (Types.shutdown_on_block_synced n)
    then error "shutdownOnSlot: `shutdown_on_slot_synced` or `shutdown_on_block_synced` already set."
    else n {Types.shutdown_on_slot_synced = Just slot}
  )

shutdownOnBlock :: HasCallStack => Integer -> Types.Profile -> Types.Profile
shutdownOnBlock block = node
  (\n ->
    if
         isJust (Types.shutdown_on_slot_synced  n)
      || isJust (Types.shutdown_on_block_synced n)
    then error "shutdownOnBlock: `shutdown_on_slot_synced` or `shutdown_on_block_synced` already set."
    else n {Types.shutdown_on_block_synced = Just block}
  )

shutdownOnOff :: HasCallStack => Types.Profile -> Types.Profile
shutdownOnOff = node
  (\n ->
    if
         isJust (Types.shutdown_on_slot_synced  n)
      || isJust (Types.shutdown_on_block_synced n)
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
rtsAppend str = node (\n -> n {Types.rts_flags_override = Types.rts_flags_override n ++ [str]})

rtsGcNonMoving :: Types.Profile -> Types.Profile
rtsGcNonMoving = rtsAppend "-xn"

-- parallel GC for the old generation only
rtsGcParallel :: Types.Profile -> Types.Profile
rtsGcParallel = rtsAppend "-qg1"

-- load balancing, applies only to parallel GC
rtsGcLoadBalance :: Types.Profile -> Types.Profile
rtsGcLoadBalance = rtsAppend "-qb1"

rtsGcAllocSize :: Integer -> Types.Profile -> Types.Profile
rtsGcAllocSize size = rtsAppend $ "-A" ++ show size ++ "m"

rtsThreads :: Integer -> Types.Profile -> Types.Profile
rtsThreads n = rtsAppend $ "-N" ++ show n

rtsHeapLimit :: Integer -> Types.Profile -> Types.Profile
rtsHeapLimit n = rtsAppend $ "-M" ++ show n ++ "m"

rtsEventlogged :: Types.Profile -> Types.Profile
rtsEventlogged = rtsAppend "-l"

-- turns on heap profiling via workbench profile, as this does not require a -profiled build
rtsHeapProf :: Types.Profile -> Types.Profile
rtsHeapProf = rtsAppend "-hT"

heapLimit :: HasCallStack => Integer -> Types.Profile -> Types.Profile
heapLimit l = node
  (\n ->
    if isJust (Types.heap_limit n)
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
    if Types.tps g /= 0
    then error "tps: `tps` already set (not zero)."
    else g {Types.tps = i}
  )

txIn :: HasCallStack => Integer -> Types.Profile -> Types.Profile
txIn i = generator
  (\g ->
    if Types.inputs_per_tx g /= 0
    then error "txIn: `inputs_per_tx` already set (not zero)."
    else g {Types.inputs_per_tx = i}
  )

txOut :: HasCallStack => Integer -> Types.Profile -> Types.Profile
txOut i = generator
  (\g ->
    if Types.outputs_per_tx g /= 0
    then error "txOut: `outputs_per_tx` already set (not zero)."
    else g {Types.outputs_per_tx = i}
  )

txFee :: HasCallStack => Integer -> Types.Profile -> Types.Profile
txFee i = generator
  (\g ->
    if Types.tx_fee g /= 0
    then error "txFee: `tx_fee` already set (not zero)."
    else g {Types.tx_fee = i}
  )

txFeeOverwrite :: HasCallStack => Integer -> Types.Profile -> Types.Profile
txFeeOverwrite i = generator
  (\g ->
    if Types.tx_fee g == 0
    then error "txFeeOverwrite: `tx_fee` expected to be already set, but it isn't."
    else g {Types.tx_fee = i}
  )

initCooldown :: HasCallStack => Integer -> Types.Profile -> Types.Profile
initCooldown i = generator
  (\g ->
    if Types.init_cooldown g /= 0
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
    Types.RedeemerInt i
  )})

redeemerFields :: [Aeson.Object] -> Types.Profile -> Types.Profile
redeemerFields objs = plutus
  (\p -> p {Types.redeemer = Just (
    Types.RedeemerFields 0 objs
  )})

generatorEpochs :: HasCallStack => Integer -> Types.Profile -> Types.Profile
generatorEpochs i = generator
  (\g ->
    if Types.epochs g /= 0
    then error "generatorEpochs: `epochs` already set (not zero)."
    else g {Types.epochs = i}
  )

-- Workload.
--------------------------------------------------------------------------------

workloadAppend :: Types.Workload -> Types.Profile -> Types.Profile
workloadAppend w p = p {Types.workloads = Types.workloads p ++ [w]}

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

clusterEmpty :: Types.Cluster
clusterEmpty =
  Types.Cluster {
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
  , Types.keep_running = False
  }

cluster :: (Types.Cluster -> Types.Cluster) -> Types.Profile -> Types.Profile
cluster f p = p {Types.cluster = Just $ case Types.cluster p of
                                          Nothing -> f clusterEmpty
                                          (Just c) -> f c
              }

clusterMinimunStorage :: Maybe (Types.ByNodeType Int) -> Types.Profile -> Types.Profile
clusterMinimunStorage ms = cluster (\c -> c {Types.minimun_storage = ms})

clusterKeepRunningOn :: Types.Profile -> Types.Profile
clusterKeepRunningOn = cluster (\c -> c {Types.keep_running = True})

nomad :: (Types.ClusterNomad -> Types.ClusterNomad) -> Types.Profile -> Types.Profile
nomad f = cluster (\c -> c {Types.nomad = f (Types.nomad c)})

nomadNamespace :: String -> Types.Profile -> Types.Profile
nomadNamespace ns = nomad (\n -> n {Types.namespace = ns})

nomadClass :: String -> Types.Profile -> Types.Profile
nomadClass nc = nomad (\n -> n {Types.nomad_class = nc})

nomadResources :: Types.ByNodeType Types.Resources -> Types.Profile -> Types.Profile
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
aws f = cluster (\c -> c {Types.aws = f (Types.aws c)})

awsInstanceTypes :: Types.ByNodeType String -> Types.Profile -> Types.Profile
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

-- Ensures a specific filter is added only once to the list; redudancies point to ill-formed profile specification and thus error out
analysisFiltersAppend :: HasCallStack => String -> Types.Profile -> Types.Profile
analysisFiltersAppend str = analysis
  (\a ->
    if str `elem` Types.filters a
    then error $ "analysis: `filters` already has \"" ++ str ++ "\"."
    else a {Types.filters = Types.filters a ++ [str]}
  )

analysisSizeSmall :: HasCallStack => Types.Profile -> Types.Profile
analysisSizeSmall = analysisFiltersAppend "size-small"

analysisSizeModerate :: HasCallStack => Types.Profile -> Types.Profile
analysisSizeModerate = analysisFiltersAppend "size-moderate"

analysisSizeModerate2 :: HasCallStack => Types.Profile -> Types.Profile
analysisSizeModerate2 = analysisFiltersAppend "size-moderate-2"

analysisSizeFull :: HasCallStack => Types.Profile -> Types.Profile
analysisSizeFull = analysisFiltersAppend "size-full"

analysisUnitary :: HasCallStack => Types.Profile -> Types.Profile
analysisUnitary = analysisFiltersAppend "unitary"

analysisEpoch3Plus :: HasCallStack => Types.Profile -> Types.Profile
analysisEpoch3Plus = analysisFiltersAppend "epoch3+"

analysisEpoch4Plus :: HasCallStack => Types.Profile -> Types.Profile
analysisEpoch4Plus = analysisFiltersAppend "epoch4+"

analysisEpoch5Plus :: HasCallStack => Types.Profile -> Types.Profile
analysisEpoch5Plus = analysisFiltersAppend "epoch5+"

-- Analysis expressions:
------------------------

analysisExpressionAppend :: Types.AnalysisFilterExpression -> Types.Profile -> Types.Profile
analysisExpressionAppend expr = analysis (\a -> a {Types.filter_exprs = Types.filter_exprs a ++ [expr]})

cBlockMinimumAdoptions :: Integer -> Types.Profile -> Types.Profile
cBlockMinimumAdoptions i = analysisExpressionAppend
  (Types.AnalysisFilterExpression
    "CBlock"
    (Types.AnalysisFilterExpressionContent "BMinimumAdoptions" i)
  )

--------------------------------------------------------------------------------

preset :: HasCallStack => String -> Types.Profile -> Types.Profile
preset str p =
  if isJust (Types.preset p)
  then error "preset: `preset` already set (not Nothing)."
  else p {Types.preset = Just str}

overlay :: HasCallStack => Aeson.Object -> Types.Profile -> Types.Profile
overlay obj p =
  if Types.overlay p /= mempty
  then error "overlay: `overlay` already set (not an empty JSON object)."
  else p {Types.overlay = obj}
