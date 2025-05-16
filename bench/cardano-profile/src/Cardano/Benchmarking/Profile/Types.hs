{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Types (

  Profile (..)
, profileProtocolVersion

, Scenario (..)

, Composition (..)
, Topology (..), Topology.Location (..), Topology.AWSRegion (..)

, Era (..)
, firstEraForMajorVersion
, Genesis (..)

, ChainDB (..), Chunks (..)

, Node (..)
, NodeVerbatim (..)

, Generator (..)
, Plutus (..), Redeemer (..)

, Workload (..)
, Entrypoints (..)

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
import           Data.Maybe (isJust)
import           GHC.Generics
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
-- Package: cardano-topology.
import qualified Cardano.Benchmarking.Topology.Types as Topology
-- Package: scientific.
import qualified Data.Scientific as Scientific
-- Package: text.
import qualified Data.Text as Text
-- Package: time.
import qualified Data.Time as Time

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

  , chaindb :: Maybe ChainDB

  -- TODO: Create en "effective_epochs" like property, now it's derived from
  --       nodes' "--shutdown-on-*-sync" property and generator's `epochs`
  --       simultaneously.
  , node :: Node

  , generator :: Generator
  , workloads :: [Workload]

  , tracer :: Tracer
  , cluster :: Maybe Cluster
  , analysis :: Analysis
  , derived :: Derived
  , cli_args :: CliArgs

  -- TODO: Somehow merge these two!
  , preset :: Maybe String
  , overlay :: Aeson.Object --TODO: Add `Maybe`, empty object for compatibility.
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Profile where
  toJSON = Aeson.genericToJSON
    -- TODO: Remove after removing `jq` profiles.
    -- To compare JSONs without "desc", "chaindb" and "preset" properties.
    (Aeson.defaultOptions {Aeson.omitNothingFields = True})

instance Aeson.FromJSON Profile where
  parseJSON = Aeson.genericParseJSON
    -- TODO: Change to `True` after removing `jq` profiles.
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = False})

profileProtocolVersion :: Profile -> Maybe (Int, Int)
profileProtocolVersion Profile{genesis = Genesis{shelley = shey}} = do
  Aeson.Object pparams <- KM.lookup "protocolParams" shey
  Aeson.Object protver <- KM.lookup "protocolVersion" pparams
  Aeson.Number major   <- KM.lookup "major" protver
  Aeson.Number minor   <- KM.lookup "minor" protver
  (,) <$> Scientific.toBoundedInteger major <*> Scientific.toBoundedInteger minor

--------------------------------------------------------------------------------

-- Scenario "fixed" is actually not being used.
data Scenario = Idle | TracerOnly | Fixed | FixedLoaded | Chainsync
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Scenario where
  toJSON Idle        = Aeson.toJSON ("idle"         :: Text.Text)
  toJSON TracerOnly  = Aeson.toJSON ("tracer-only"  :: Text.Text)
  toJSON Fixed       = Aeson.toJSON ("fixed"        :: Text.Text)
  toJSON FixedLoaded = Aeson.toJSON ("fixed-loaded" :: Text.Text)
  toJSON Chainsync   = Aeson.toJSON ("chainsync"    :: Text.Text)

instance Aeson.FromJSON Scenario where
  parseJSON = Aeson.withText "Scenario" $ \t -> case t of
    "idle"         -> return Idle
    "tracer-only"  -> return TracerOnly
    "fixed"        -> return Fixed
    "fixed-loaded" -> return FixedLoaded
    "chainsync"    -> return Chainsync
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

instance Aeson.ToJSON Composition where
  toJSON = Aeson.genericToJSON
    -- TODO: Remove after removing `jq` profiles.
    -- To compare JSONs without the "with_chaindb_server" property.
    (Aeson.defaultOptions {Aeson.omitNothingFields = True})

instance Aeson.FromJSON Composition where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

-- Scenario "line" is actually not being used.
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

data Era = Shelley | Allegra | Mary | Alonzo | Babbage | Conway
  deriving (Show, Eq, Ord, Generic)

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

-- | Minimal major protocol version per era
firstEraForMajorVersion :: Int -> Era
firstEraForMajorVersion pv
  | pv >= 9   = Conway
  | pv >= 7   = Babbage
  | pv >= 5   = Alonzo
  | pv >= 4   = Mary
  | pv >= 3   = Allegra
  | pv >= 2   = Shelley
  | otherwise = error $ "firstEraForVersion: unsupported major protocol version " ++ show pv

{-
cf. https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md

| Date    | Phase    | Era     | Slot Number | Epoch Number | Protocol Version | Ledger Protocol | Consensus Mechanism     | Notes              |
|---------|----------|---------|------------:|-------------:|-----------------:|-----------------|-------------------------|--------------------|
| 2017/09 | Byron    | Byron   |           0 |            0 |              0,0 | -               | Ouroboros Classic       |                    |
| 2020/02 | Byron    | Byron   |     3801600 |          176 |              1,0 | -               | Ouroboros BFT           |                    |
| 2020/07 | Shelley  | Shelley |     4492800 |          208 |              2,0 | TPraos          | Ouroboros Praos         |                    |
| 2020/12 | Goguen   | Allegra |    16588800 |          236 |              3,0 | TPraos          | Ouroboros Praos         |                    |
| 2021/03 | Goguen   | Mary    |    23068800 |          251 |              4,0 | TPraos          | Ouroboros Praos         |                    |
| 2021/09 | Goguen   | Alonzo  |    39916975 |          290 |              5,0 | TPraos          | Ouroboros Praos         |                    |
| 2021/10 | Goguen   | Alonzo  |    43372972 |          298 |              6,0 | TPraos          | Ouroboros Praos         | intra-era hardfork |
| 2022/09 | Goguen   | Babbage |    72316896 |          365 |              7,0 | Praos           | Ouroboros Praos         | Vasil HF           |
| 2023/02 | Goguen   | Babbage |    84844885 |          394 |              8,0 | Praos           | Ouroboros Praos         | Valentine HF       |
| 2024/09 | Voltaire | Conway  |   133660855 |          507 |              9,0 | Praos           | Ouroboros Genesis/Praos | Chang HF           |
| 2025/01 | Voltaire | Conway  |   146620809 |          537 |             10,0 | Praos           | Ouroboros Genesis/Praos | Plomin HF          |
-}

--------------------------------------------------------------------------------

data Genesis = Genesis
  {

  -- Genesis definition.
    pparamsEpoch :: Integer
  , pparamsOverlays :: [String]
  -- Genesis result.
    -- TODO: These three could be custom overlays and final objects be part of
    --       the derived properties.
  , shelley :: KM.KeyMap Aeson.Value
  , alonzo :: KM.KeyMap Aeson.Value
  , conway :: Maybe (KM.KeyMap Aeson.Value) -- TODO: Remove the null.

  -- Absolute durations:
  , slot_duration :: Time.NominalDiffTime
  , epoch_length :: Integer
  -- Block duration:
  , active_slots_coeff :: Scientific.Scientific
  -- Security parameter:
  , parameter_k :: Integer

  -- Size:
  , utxo :: Integer
  , delegators :: Integer
  , dreps :: Integer
  , extra_future_offset :: Time.NominalDiffTime -- Size dependent!

  -- And the others!
  , per_pool_balance :: Integer
  , funds_balance :: Integer
  , utxo_keys :: Integer

  -- Testnet:
  , network_magic :: Integer

  -- TODO: These two are built like derived properties. Move there?
  , pool_coin :: Integer
  , delegator_coin :: Integer

  -- TODO: Not used ?
  , single_shot :: Bool
  , max_block_size :: Maybe Integer

  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Genesis

instance Aeson.FromJSON Genesis where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

--------------------------------------------------------------------------------

data ChainDB = ChainDB
  {
    mainnet_chunks :: Chunks
  , ledger_snapshot :: Chunks
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ChainDB

instance Aeson.FromJSON ChainDB where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

data Chunks = Chunks
  { 
    chaindb_server :: Integer
  , explorer_chunk :: Integer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Chunks where
  toJSON p@(Chunks _ _) =
    Aeson.object
      [ "chaindb_server" Aeson..= chaindb_server p
      , "explorer"       Aeson..= explorer_chunk p
      ]

instance Aeson.FromJSON Chunks where
  parseJSON =
    Aeson.withObject "Chunks" $ \o -> do
      Chunks
        <$> o Aeson..: "chaindb_server"
        <*> o Aeson..: "explorer"

--------------------------------------------------------------------------------

data Node = Node
  { 
    utxo_lmdb :: Bool
  , ssd_directory :: Maybe String

  -- TODO: Move up "EnableP2P". A new level only for this?
  , verbatim :: NodeVerbatim

  , trace_forwarding :: Bool
  , tracing_backend :: String

  -- TODO: Create an RTS property.
  , rts_flags_override :: [String]
  , heap_limit :: Maybe Integer

  -- TODO: Either have this here or part of derived.
  , shutdown_on_slot_synced :: Maybe Integer
  , shutdown_on_block_synced :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Node

instance Aeson.FromJSON Node where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

-- Properties passed directly to the node(s) "config.json" file.
newtype NodeVerbatim = NodeVerbatim
  { enableP2P :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

-- `Nothing` properties are not in the final "config.json", not even "null".
instance Aeson.ToJSON NodeVerbatim where
  -- If the "EnableP2P" JSON property is present in a Cardano node version that
  -- does not support P2P, the profile can fail to properly initiate a cluster.
  toJSON   (NodeVerbatim Nothing) = Aeson.object []
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

instance Aeson.FromJSON Generator where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

data Plutus = Plutus
  { plutusType :: Maybe String -- TODO: Rename in workbench/bash to "plutus_type"
  , plutusScript :: Maybe String
  , redeemer :: Maybe Redeemer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Plutus where
  toJSON p =
    Aeson.object $
      [ "type"     Aeson..= plutusType p
      , "script"   Aeson..= plutusScript p
      ]
      ++
      -- TODO: Needed to replicate the old "jq" JSON output.
      ["redeemer" Aeson..= redeemer p | isJust (redeemer p)]

instance Aeson.FromJSON Plutus where
  parseJSON =
    Aeson.withObject "Plutus" $ \o -> do
      Plutus
        <$> o Aeson..:? "type"
        <*> o Aeson..:? "script"
        <*> o Aeson..:? "redeemer"

data Redeemer =
    RedeemerInt
      { redeemerInt :: Integer
      }
  | RedeemerFields
      { constructor :: Integer
      , fields :: [Aeson.Object]
      }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Redeemer where
  toJSON r@(RedeemerInt _) =
    Aeson.object
      [ "int"         Aeson..= redeemerInt r
      ]
  toJSON r@(RedeemerFields _ _) =
    Aeson.object
      [ "constructor" Aeson..= constructor r
      , "fields"      Aeson..= fields r
      ]

instance Aeson.FromJSON Redeemer where
  parseJSON =
    Aeson.withObject "Redeemer" $ \o ->
      case KM.lookup "int" o of
        (Just _) -> do
          RedeemerInt
            <$> o Aeson..: "int"
        Nothing -> do
          RedeemerFields
            <$> o Aeson..: "constructor"
            <*> o Aeson..: "fields"

--------------------------------------------------------------------------------

data Workload = Workload
  { workloadName :: String
  , parameters :: Aeson.Object
  , entrypoints :: Entrypoints
  , wait_pools :: Bool
  }
  deriving (Eq, Show, Generic)

data Entrypoints = Entrypoints
  { pre_generator :: Maybe String
  , producers :: String
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Workload where
  toJSON p =
    Aeson.object
      [ "name"        Aeson..= workloadName p
      , "parameters"  Aeson..= parameters   p
      , "entrypoints" Aeson..= entrypoints  p
      , "wait_pools"  Aeson..= wait_pools   p
      ]

instance Aeson.FromJSON Workload where
  parseJSON =
    Aeson.withObject "Workload" $ \o -> do
      Workload
        <$> o Aeson..: "name"
        <*> o Aeson..: "parameters"
        <*> o Aeson..: "entrypoints"
        <*> o Aeson..: "wait_pools"

instance Aeson.ToJSON Entrypoints

instance Aeson.FromJSON Entrypoints where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

--------------------------------------------------------------------------------

data Tracer = Tracer
  { rtview :: Bool
  , ekg :: Bool
  , withresources :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Tracer

instance Aeson.FromJSON Tracer where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

--------------------------------------------------------------------------------

-- | The cluster properties (if used).
data Cluster = Cluster
  { nomad :: ClusterNomad
  , aws :: ClusterAWS
  , minimun_storage :: Maybe (ByNodeType Int)
  , keep_running :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Cluster

instance Aeson.FromJSON Cluster where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

data ClusterNomad = ClusterNomad
  { namespace :: String
  , nomad_class :: String
  , resources :: ByNodeType Resources
  , host_volumes :: Maybe [HostVolume]
  , fetch_logs_ssh :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ClusterNomad where
  toJSON cn =
    Aeson.object
      [ "namespace"      Aeson..= namespace cn
      , "class"          Aeson..= nomad_class cn
      , "resources"      Aeson..= resources cn
      , "host_volumes"   Aeson..= host_volumes cn
      , "fetch_logs_ssh" Aeson..= fetch_logs_ssh cn
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

instance Aeson.FromJSON HostVolume where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

data ClusterAWS = ClusterAWS
  { instance_type :: ByNodeType String
  , use_public_routing :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ClusterAWS

instance Aeson.FromJSON ClusterAWS where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

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

instance Aeson.FromJSON Resources where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

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
  toJSON a =
    Aeson.object
      [ "type"                            Aeson..= analysisType a
      , "cluster_base_startup_overhead_s" Aeson..= cluster_base_startup_overhead_s a
      , "start_log_spread_s"              Aeson..= start_log_spread_s a
      , "last_log_spread_s"               Aeson..= last_log_spread_s a
      , "silence_since_last_block_s"      Aeson..= silence_since_last_block_s a
      , "tx_loss_ratio"                   Aeson..= tx_loss_ratio a
      , "finish_patience"                 Aeson..= finish_patience a
      , "filters"                         Aeson..= filters a
      , "minimum_chain_density"           Aeson..= minimum_chain_density a
      , "cluster_startup_overhead_s"      Aeson..= cluster_startup_overhead_s a
      , "filter_exprs"                    Aeson..= filter_exprs a
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

instance Aeson.FromJSON AnalysisFilterExpression where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

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

instance Aeson.FromJSON Derived where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})

--------------------------------------------------------------------------------

data CliArgs = CliArgs
  { createStakedArgs :: [Aeson.Value]
  , createTestnetDataArgs :: [Aeson.Value]
  , pools :: [Aeson.Value]
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON CliArgs

instance Aeson.FromJSON CliArgs where
  parseJSON = Aeson.genericParseJSON
    (Aeson.defaultOptions {Aeson.rejectUnknownFields = True})
