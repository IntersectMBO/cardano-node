{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Types (

  Profile (..)

, Scenario (..)

, Composition (..)
, Topology (..), Topology.Location (..), Topology.AWSRegion (..)

, Era (..)
, Genesis (..)

, Node (..)
, NodeVerbatim (..)

, Generator (..)
, Plutus (..), Redeemer (..)

, Tracer (..)

, Cluster (..)
, ClusterNomad (..)
, HostVolume (..)
, ClusterAWS (..)
, ByNodeType (..)
, Resources (..)

, Analysis (..)
, AnalysisFilterExpression (..)
, AnalysisFilterExpressionContent (..)

, Derived (..)

, CliArgs (..)

) where

--------------------------------------------------------------------------------

import           Prelude
import           GHC.Generics

import qualified Data.Time as Time
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Scientific as Scientific

import qualified Cardano.Benchmarking.Topology.Types as Topology

--------------------------------------------------------------------------------

-- | A profile as it's used to define benchmarking profiles.
data Profile = Profile
  { name :: String
  -- TODO: Add ???
  -- , suffix :: Maybe String
  , desc :: Maybe String
  -- TODO: Add ???
  -- , extra_desc :: Maybe String
  , scenario :: Scenario

  , composition :: Composition

  , era :: Era
  , genesis :: Genesis

  -- TODO: Create en "effective_epochs" like property, now it's derived from
  --       nodes' "--shutdown-on-*-sync" property and generator's `epochs`
  --       simultaneously.
  , node :: Node

  , generator :: Generator

  , tracer :: Tracer
  , cluster :: Cluster
  , analysis :: Analysis
  , derived :: Derived
  , cli_args :: CliArgs

  -- TODO: Remove!
  , preset :: Maybe String
  , overlay :: Aeson.Object
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Profile

instance Aeson.FromJSON Profile

--------------------------------------------------------------------------------

-- TODO: Scenario "fixed" is actually not being used.
data Scenario = Idle | TracerOnly | Fixed | FixedLoaded | Chainsync | Latency
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Scenario where
  toJSON Idle        = Aeson.toJSON ("idle"         :: Text.Text)
  toJSON TracerOnly  = Aeson.toJSON ("tracer-only"  :: Text.Text)
  toJSON Fixed       = Aeson.toJSON ("fixed"        :: Text.Text)
  toJSON FixedLoaded = Aeson.toJSON ("fixed-loaded" :: Text.Text)
  toJSON Chainsync   = Aeson.toJSON ("chainsync"    :: Text.Text)
  toJSON Latency     = Aeson.toJSON ("latency"      :: Text.Text)

instance Aeson.FromJSON Scenario where
  parseJSON = Aeson.withText "Scenario" $ \t -> case t of
    "idle"         -> return Idle
    "tracer-only"  -> return TracerOnly
    "fixed"        -> return Fixed
    "fixed-loaded" -> return FixedLoaded
    "chainsync"    -> return Chainsync
    "latency"      -> return Latency
    _              -> fail $ "Unknown Scenario: \"" ++ Text.unpack t ++ "\""

--------------------------------------------------------------------------------

{--

TODO: Except "ci-test-dense10" fields "n_hosts", "n_pools", "n_singular_pools"
      and "n_pool_hosts" always have the same value!
      Simplify!
> wb profile all-profiles | jq 'map(select( .composition.n_hosts != .composition.n_pools ))' | jq 'map(.name)'
[
  "ci-test-dense10-shey",
  "ci-test-dense10-alra",
  "ci-test-dense10-mary",
  "ci-test-dense10-alzo",
  "ci-test-dense10-bage",
  "ci-test-dense10-coay"
]

> wb profile all-profiles | jq 'map(select( .composition.n_pools != .composition.n_singular_pools ))' | jq 'map(.name)'
[
  "ci-test-dense10-shey",
  "ci-test-dense10-alra",
  "ci-test-dense10-mary",
  "ci-test-dense10-alzo",
  "ci-test-dense10-bage",
  "ci-test-dense10-coay"
]

> wb profile all-profiles | jq 'map(select( .composition.n_singular_pools != .composition.n_pool_hosts ))' | jq 'map(.name)'
[
  "ci-test-dense10-shey",
  "ci-test-dense10-alra",
  "ci-test-dense10-mary",
  "ci-test-dense10-alzo",
  "ci-test-dense10-bage",
  "ci-test-dense10-coay"
]

TODO: Except "ci-test-dense10" field n_dense_pools is always zero.
> wb profile all-profiles | jq 'map(select( .composition.n_dense_pools != 0 ))' | jq 'map(.name)'
[
  "ci-test-dense10-shey",
  "ci-test-dense10-alra",
  "ci-test-dense10-mary",
  "ci-test-dense10-alzo",
  "ci-test-dense10-bage",
  "ci-test-dense10-coay"
]

--}
data Composition = Composition
  { locations :: [Topology.Location]
  -- TODO: Remove, all profile have a zero here!
  , n_bft_hosts :: Integer
  , n_singular_hosts :: Integer
  -- TODO: Zero for all profiles except "ci-test-dense10".
  , n_dense_hosts :: Integer
  -- TODO: One for all profiles except "ci-test-dense10".
  , dense_pool_density :: Integer
  -- TODO: Remove, all profile have a False here!
  , with_proxy :: Bool
  , with_explorer :: Bool
  , topology :: Topology
  , with_chaindb_server :: Maybe Bool
  , n_hosts :: Integer
  , n_pools :: Integer
  , n_singular_pools :: Integer
  -- TODO: Zero for all profiles except "ci-test-dense10".
  , n_dense_pools :: Integer
  , n_pool_hosts :: Integer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Composition

instance Aeson.FromJSON Composition

-- TODO: Scenario "line" is actually not being used.
data Topology = Line | UniCircle | Torus | TorusDense
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Topology where
  toJSON Line       = Aeson.toJSON ("line"        :: Text.Text)
  toJSON UniCircle  = Aeson.toJSON ("uni-circle"  :: Text.Text)
  toJSON Torus      = Aeson.toJSON ("torus"       :: Text.Text)
  toJSON TorusDense = Aeson.toJSON ("torus-dense" :: Text.Text)

instance Aeson.FromJSON Topology where
  parseJSON = Aeson.withText "Topology" $ \t -> case t of
    "line"        -> return Line
    "uni-circle"  -> return UniCircle
    "torus"       -> return Torus
    "torus-dense" -> return TorusDense
    _             -> fail $ "Unknown Topology: \"" ++ Text.unpack t ++ "\""

--------------------------------------------------------------------------------

{--
https://docs.cardano.org/explore-cardano/eras-and-phases/

> wb profile all-profiles | jq .[] | jq -r .era | sort | uniq
allegra
alonzo
babbage
conway
mary
shelley
--}
data Era = Allegra | Shelley | Mary | Alonzo | Babbage | Conway
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Era where
  toJSON Allegra = Aeson.toJSON ("allegra" :: Text.Text)
  toJSON Shelley = Aeson.toJSON ("shelley" :: Text.Text)
  toJSON Mary    = Aeson.toJSON ("mary"    :: Text.Text)
  toJSON Alonzo  = Aeson.toJSON ("alonzo"  :: Text.Text)
  toJSON Babbage = Aeson.toJSON ("babbage" :: Text.Text)
  toJSON Conway  = Aeson.toJSON ("conway"  :: Text.Text)

instance Aeson.FromJSON Era where
  parseJSON = Aeson.withText "Era" $ \t -> case t of
    "allegra" -> return Allegra
    "shelley" -> return Shelley
    "mary"    -> return Mary
    "alonzo"  -> return Alonzo
    "babbage" -> return Babbage
    "conway"  -> return Conway
    _         -> fail $ "Unknown Era: \"" ++ Text.unpack t ++ "\""

--------------------------------------------------------------------------------

data Genesis = Genesis
  {

  -- Genesis definition.
    pparamsEpoch :: Integer
  , pparamsOverlays :: [String]
  -- Genesis result.
    -- TODO: These three should be custom overlays and final objects be part of
    --       the derived properties.
  , shelley :: Aeson.Object
  , alonzo :: Aeson.Object
  , conway :: Maybe Aeson.Object -- TODO: Remove the null.

  -- Absolute durations:
  , slot_duration :: Time.NominalDiffTime
  , epoch_length :: Integer
  -- Block duration:
  , active_slots_coeff :: Scientific.Scientific
  -- Security parameter:
  , parameter_k :: Integer

  -- Size:
  , utxo :: Integer
  , delegators :: Maybe Integer
  , dreps :: Integer
  , extra_future_offset :: Time.NominalDiffTime -- Size dependent!

  -- And the others!
  , per_pool_balance :: Integer
  , funds_balance :: Integer

  -- Testnet:
  , network_magic :: Integer

  -- TODO: These two are built like derived properties. Move there?
  , pool_coin :: Integer
  , delegator_coin :: Integer
  -- TODO: Not used ?
  , single_shot :: Bool

  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Genesis

instance Aeson.FromJSON Genesis

--------------------------------------------------------------------------------

data Node = Node
  { 
    utxo_lmdb :: Bool

  -- TODO: Move up "EnableP2P". A new level only for this?
  , verbatim :: NodeVerbatim

  -- TODO: "tracing_backend" is null or has a backend name!
  , nodeTracer :: Bool -- -- TODO: Rename in workbench/bash to "node_tracer" (?)
  , tracing_backend :: String

  -- TODO: Create an RTS property.
  , rts_flags_override :: [String]
  , heap_limit :: Maybe Integer

  -- TODO: Either have this here or part of derived.
  , shutdown_on_slot_synced :: Maybe Integer
  , shutdown_on_block_synced :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Node where
  toJSON p@(Node _ _ _ _ _ _ _ _) =
    Aeson.object
      [ "utxo_lmdb"                Aeson..= utxo_lmdb p
      , "verbatim"                 Aeson..= verbatim p
      , "tracer"                   Aeson..= nodeTracer p
      , "tracing_backend"          Aeson..= tracing_backend p
      , "rts_flags_override"       Aeson..= rts_flags_override p
      , "heap_limit"               Aeson..= heap_limit p
      , "shutdown_on_slot_synced"  Aeson..= shutdown_on_slot_synced p
      , "shutdown_on_block_synced" Aeson..= shutdown_on_block_synced p
      ]

instance Aeson.FromJSON Node where
  parseJSON =
    Aeson.withObject "Node" $ \o -> do
      Node
        <$> o Aeson..: "utxo_lmdb"
        <*> o Aeson..: "verbatim"
        <*> o Aeson..: "tracer"
        <*> o Aeson..: "tracing_backend"
        <*> o Aeson..: "rts_flags_override"
        <*> o Aeson..: "heap_limit"
        <*> o Aeson..: "shutdown_on_slot_synced"
        <*> o Aeson..: "shutdown_on_block_synced"

data NodeVerbatim = NodeVerbatim
  { enableP2P :: Maybe Bool -- TODO: Make it lower case in the workbench.
  }
  deriving (Eq, Show, Generic)

-- TODO: Switch to lower-case in workbench/bash
instance Aeson.ToJSON NodeVerbatim where
  toJSON p@(NodeVerbatim _) =
    Aeson.object
      [ "EnableP2P"   Aeson..= enableP2P p
      ]

-- TODO: Switch to lower-case in workbench/bash
instance Aeson.FromJSON NodeVerbatim where
  parseJSON =
    Aeson.withObject "NodeVerbatim" $ \o -> do
      NodeVerbatim
        <$> o Aeson..:? "EnableP2P"

--------------------------------------------------------------------------------

data Generator = Generator
  { tps :: Scientific.Scientific

  , inputs_per_tx :: Integer
  , outputs_per_tx :: Integer
  , tx_fee :: Integer

  , init_cooldown :: Integer

  , plutus :: Plutus

  -- TODO: Remove from here, the "run-script.json" has no epoch knowledge!
  , epochs :: Integer
  -- TODO: Remove from here, it's populated like a derived property.
  --       ($gtor.tx_count // ($generator_duration * $gtor.tps) | ceil)
  , tx_count :: Maybe Integer
  -- TODO: Not used! ???
  , add_tx_size :: Integer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Generator

instance Aeson.FromJSON Generator

data Plutus = Plutus
  { plutusType :: Maybe String -- TODO: Rename in workbench/bash to "plutus_type"
  , plutusScript :: Maybe String
  , redeemer :: Maybe Redeemer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Plutus where
  toJSON p@(Plutus _ _ _) =
    Aeson.object
      [ "type"     Aeson..= plutusType p
      , "script"   Aeson..= plutusScript p
      , "redeemer" Aeson..= redeemer p
      ]

instance Aeson.FromJSON Plutus where
  parseJSON =
    Aeson.withObject "Plutus" $ \o -> do
      Plutus
        <$> o Aeson..:? "type"
        <*> o Aeson..:? "script"
        <*> o Aeson..:? "redeemer"

data Redeemer = Redeemer
  { redeemerInt :: Maybe Integer
  , constructor :: Maybe Integer
  , fields :: Maybe [Aeson.Object]
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Redeemer where
  toJSON p@(Redeemer _ _ _) =
    Aeson.object
      [ "int"         Aeson..= redeemerInt p
      , "constructor" Aeson..= constructor p
      , "fields"      Aeson..= fields p
      ]

instance Aeson.FromJSON Redeemer where
  parseJSON =
    Aeson.withObject "Redeemer" $ \o -> do
      Redeemer
        <$> o Aeson..:? "int"
        <*> o Aeson..:? "constructor"
        <*> o Aeson..:? "fields"

--------------------------------------------------------------------------------

data Tracer = Tracer
  { rtview :: Bool
  , ekg :: Bool
  , withresources :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Tracer

instance Aeson.FromJSON Tracer

--------------------------------------------------------------------------------

-- | The cluster properties (if used).
data Cluster = Cluster
  { nomad :: ClusterNomad
  , aws :: ClusterAWS
  , minimun_storage :: Maybe (ByNodeType Int)
  , ssd_directory :: Maybe String
  , keep_running :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Cluster

instance Aeson.FromJSON Cluster

data ClusterNomad = ClusterNomad
  { namespace :: String
  , nomad_class :: String
  , resources :: ByNodeType Resources
  , host_volumes :: Maybe [HostVolume]
  , fetch_logs_ssh :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ClusterNomad where
  toJSON p@(ClusterNomad _ _ _ _ _) =
    Aeson.object
      [ "namespace"      Aeson..= namespace p
      , "class"          Aeson..= nomad_class p
      , "resources"      Aeson..= resources p
      , "host_volumes"   Aeson..= host_volumes p
      , "fetch_logs_ssh" Aeson..= fetch_logs_ssh p
      ]

instance Aeson.FromJSON ClusterNomad where
  parseJSON =
    Aeson.withObject "ClusterNomad" $ \o -> do
      ClusterNomad
        <$> o Aeson..: "namespace"
        <*> o Aeson..: "class"
        <*> o Aeson..: "resources"
        <*> o Aeson..: "host_volumes"
        <*> o Aeson..: "fetch_logs_ssh"

data HostVolume = HostVolume
  { destination :: String
  , read_only :: Bool
  , source :: String
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON HostVolume

instance Aeson.FromJSON HostVolume

data ClusterAWS = ClusterAWS
  { instance_type :: ByNodeType String
  , use_public_routing :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ClusterAWS

instance Aeson.FromJSON ClusterAWS

data ByNodeType a = ByNodeType
  { producer :: a
  , explorer :: Maybe a
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (ByNodeType a)

instance Aeson.FromJSON a => Aeson.FromJSON (ByNodeType a)

data Resources = Resources
  { cores :: Integer
  , memory :: Integer
  , memory_max :: Integer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Resources

instance Aeson.FromJSON Resources

--------------------------------------------------------------------------------

data Analysis = Analysis
  { analysisType :: Maybe String -- TODO: Rename in workbench/bash to "analysis_type"
  , cluster_base_startup_overhead_s :: Time.NominalDiffTime
  , start_log_spread_s :: Integer
  , last_log_spread_s :: Integer
  , silence_since_last_block_s :: Integer
  , tx_loss_ratio :: Scientific.Scientific
  , finish_patience :: Integer
  , filters :: [String]
  , minimum_chain_density :: Scientific.Scientific
  , cluster_startup_overhead_s :: Time.NominalDiffTime
  , filter_exprs :: [AnalysisFilterExpression]
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Analysis where
  toJSON p@(Analysis _ _ _ _ _ _ _ _ _ _ _) =
    Aeson.object
      [ "type"                            Aeson..= analysisType p
      , "cluster_base_startup_overhead_s" Aeson..= cluster_base_startup_overhead_s p
      , "start_log_spread_s"              Aeson..= start_log_spread_s p
      , "last_log_spread_s"               Aeson..= last_log_spread_s p
      , "silence_since_last_block_s"      Aeson..= silence_since_last_block_s p
      , "tx_loss_ratio"                   Aeson..= tx_loss_ratio p
      , "finish_patience"                 Aeson..= finish_patience p
      , "filters"                         Aeson..= filters p
      , "minimum_chain_density"           Aeson..= minimum_chain_density p
      , "cluster_startup_overhead_s"      Aeson..= cluster_startup_overhead_s p
      , "filter_exprs"                    Aeson..= filter_exprs p
      ]

instance Aeson.FromJSON Analysis where
  parseJSON =
    Aeson.withObject "Analysis" $ \o -> do
      Analysis
        <$> o Aeson..: "type"
        <*> o Aeson..: "cluster_base_startup_overhead_s"
        <*> o Aeson..: "start_log_spread_s"
        <*> o Aeson..: "last_log_spread_s"
        <*> o Aeson..: "silence_since_last_block_s"
        <*> o Aeson..: "tx_loss_ratio"
        <*> o Aeson..: "finish_patience"
        <*> o Aeson..: "filters"
        <*> o Aeson..: "minimum_chain_density"
        <*> o Aeson..: "cluster_startup_overhead_s"
        <*> o Aeson..: "filter_exprs"

data AnalysisFilterExpression = AnalysisFilterExpression
  { tag :: String
  , contents :: AnalysisFilterExpressionContent
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON AnalysisFilterExpression

instance Aeson.FromJSON AnalysisFilterExpression

data AnalysisFilterExpressionContent = AnalysisFilterExpressionContent
  { innerTag :: String
  , innerContents :: Integer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON AnalysisFilterExpressionContent where
  toJSON p@(AnalysisFilterExpressionContent _ _) =
    Aeson.object
      [ "tag"      Aeson..= innerTag p
      , "contents" Aeson..= innerContents p
      ]

instance Aeson.FromJSON AnalysisFilterExpressionContent where
  parseJSON =
    Aeson.withObject "AnalysisFilterExpressionContent" $ \o -> do
      AnalysisFilterExpressionContent
        <$> o Aeson..: "tag"
        <*> o Aeson..: "contents"

--------------------------------------------------------------------------------

-- | Derived
data Derived = Derived
  { epoch_duration :: Time.NominalDiffTime
  , effective_epochs :: Integer
  , dataset_measure :: Integer
  , delegators_effective :: Integer
  , genesis_future_offset :: Time.NominalDiffTime
  , generator_duration :: Time.NominalDiffTime
  , generator_tx_count :: Integer

  -- Supply.
  , supply_total :: Integer
  , supply_delegated :: Integer
  -- UTxO set size:
  , utxo_delegated :: Integer
  , utxo_generated :: Integer
  , utxo_stuffed :: Integer

  -- Not sure!
  , shutdown_time :: Maybe Time.NominalDiffTime
  , default_value_tx_size_estimate :: Integer -- Number of Bytes.
  , default_value_tx_per_block_estimate :: Integer -- Number of Bytes.
  , generator_blocks_lower_bound :: Integer

  -- TODO: Not used ???
  , dataset_induced_startup_delay_optimistic :: Time.NominalDiffTime
  , dataset_induced_startup_delay_conservative :: Time.NominalDiffTime

  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Derived

instance Aeson.FromJSON Derived

--------------------------------------------------------------------------------

data CliArgs = CliArgs
  { createStakedArgs :: [Aeson.Value]
  , createTestnetDataArgs :: [Aeson.Value]
  , pools :: [Aeson.Value]
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON CliArgs

instance Aeson.FromJSON CliArgs
