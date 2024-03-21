{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Types (
  Profile (..)
, Composition (..), Topology (..), Topology.Location (..), Topology.AWSRegion (..)
, Era (..)
, Genesis (..)
, Scenario (..)
, Node (..), NodeVerbatim (..)
, Tracer (..)
, Generator (..), Plutus (..)
, Cluster (..), Nomad (..), ClusterAWS (..), ByNodeType (..), Resources (..)
, Analysis (..), AnalysisFilterExpression (..), AnalysisFilterExpressionContent (..)
) where

--------------------------------------------------------------------------------

import           Prelude
import           GHC.Generics

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Scientific as Scientific

import qualified Cardano.Benchmarking.Topology.Types as Topology

--------------------------------------------------------------------------------

-- | A profile as it's used to define benchmarking profiles.
data Profile = Profile
  { name :: String
  , desc :: Maybe String
  , composition :: Composition
  , era :: Era
  , genesis :: Genesis
  , scenario :: Scenario
  , node :: Node
  , tracer :: Tracer
  , generator :: Generator
  , cluster :: Cluster
  , analysis :: Analysis
  -- TODO
--  , cli_args :: Aeson.Object
--  , overlay :: Aeson.Object
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Profile

instance Aeson.FromJSON Profile

--------------------------------------------------------------------------------

-- | A topology as it's used to define benchmarking profiles.
{--

TODO: Except "ci-test-dense10" fields n_hosts, n_pools, n_singular_pools and
      n_pool_hosts always have the same same value!
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
  , n_bft_hosts :: Int
  , n_singular_hosts :: Int
  -- TODO: Zero for all profiles except "ci-test-dense10".
  , n_dense_hosts :: Int
  -- TODO: One for all profiles except "ci-test-dense10".
  , dense_pool_density :: Int
  -- TODO: Remove, all profile have a False here!
  , with_proxy :: Bool
  , with_explorer :: Bool
  , topology :: Topology
  , with_chaindb_server :: Maybe Bool
  , n_hosts :: Int
  , n_pools :: Int
  , n_singular_pools :: Int
  -- TODO: Zero for all profiles except "ci-test-dense10".
  , n_dense_pools :: Int
  , n_pool_hosts :: Int
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Composition

instance Aeson.FromJSON Composition

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

-- | A topology as it's used to define benchmarking profiles.
data Genesis = Genesis
  { network_magic :: Int
  , single_shot :: Bool
  , per_pool_balance :: Integer
  , funds_balance :: Integer
  , utxo :: Int
  , active_slots_coeff :: Scientific.Scientific
  , epoch_length :: Int
  , parameter_k :: Int
  , slot_duration :: Scientific.Scientific
  , extra_future_offset :: Int
  , pparamsEpoch :: Int
  , delegators :: Int
  , shelley :: Aeson.Object
  , alonzo :: Aeson.Object
  , pool_coin :: Integer
  , delegator_coin :: Int
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Genesis

instance Aeson.FromJSON Genesis

--------------------------------------------------------------------------------

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

-- | A topology as it's used to define benchmarking profiles.
data Node = Node
  { rts_flags_override :: [String]
  , shutdown_on_slot_synced :: Maybe Int
  , shutdown_on_block_synced :: Maybe Int
  , tracing_backend :: String
  , nodeTracer :: Bool -- -- TODO: Rename in workbench/bash to "node_tracer" (?)
  , verbatim :: NodeVerbatim
  }
  deriving (Eq, Show, Generic)

nodeOptions :: Aeson.Options
nodeOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = flm
  , Aeson.constructorTagModifier = ctm
  }
    where
      flm "nodeTracer" = "tracer"
      flm l = l
      ctm "nodeTracer" = "tracer"
      ctm t = t

instance Aeson.ToJSON Node where
    toEncoding = Aeson.genericToEncoding nodeOptions

instance Aeson.FromJSON Node where
    parseJSON = Aeson.genericParseJSON nodeOptions

data NodeVerbatim = NodeVerbatim
  { enableP2P :: Maybe Bool
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

-- | A topology as it's used to define benchmarking profiles.
data Tracer = Tracer
  { rtview :: Bool
  , ekg :: Bool
  , withresources :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Tracer

instance Aeson.FromJSON Tracer

--------------------------------------------------------------------------------

-- | A topology as it's used to define benchmarking profiles.
data Generator = Generator
  { add_tx_size :: Int
  , init_cooldown :: Int
  , inputs_per_tx :: Int
  , outputs_per_tx :: Int
  , tx_fee :: Int
  , epochs :: Int
  , tps :: Scientific.Scientific
  , plutus :: Maybe Plutus
  -- ($gtor.tx_count // ($generator_duration * $gtor.tps) | ceil)
  , tx_count :: Int
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Generator

instance Aeson.FromJSON Generator

-- | A topology as it's used to define benchmarking profiles.
data Plutus = Plutus
  { plutusType :: Maybe String -- TODO: Rename in workbench/bash to "plutus_type"
  , plutusScript :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Plutus where
  toJSON p@(Plutus _ _) =
    Aeson.object
      [ "type"   Aeson..= plutusType p
      , "script" Aeson..= plutusScript p
      ]

instance Aeson.FromJSON Plutus where
  parseJSON =
    Aeson.withObject "Plutus" $ \o -> do
      Plutus
        <$> o Aeson..:? "type"
        <*> o Aeson..:? "script"

--------------------------------------------------------------------------------

-- | The cluster properties (if used).
data Cluster = Cluster
  { nomad :: Nomad
  , aws :: ClusterAWS
  , minimun_storage :: Maybe (ByNodeType Int)
  , keep_running :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Cluster

instance Aeson.FromJSON Cluster

data Nomad = Nomad
  { namespace :: String
  , nomad_class :: String
  , resources :: ByNodeType Resources
  , fetch_logs_ssh :: Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Nomad where
  toJSON p@(Nomad _ _ _ _) =
    Aeson.object
      [ "namespace"      Aeson..= namespace p
      , "class"          Aeson..= nomad_class p
      , "resources"      Aeson..= resources p
      , "fetch_logs_ssh" Aeson..= fetch_logs_ssh p
      ]

instance Aeson.FromJSON Nomad where
  parseJSON =
    Aeson.withObject "Nomad" $ \o -> do
      Nomad
        <$> o Aeson..: "namespace"
        <*> o Aeson..: "class"
        <*> o Aeson..: "resources"
        <*> o Aeson..: "fetch_logs_ssh"

data ClusterAWS = ClusterAWS
  { instance_type :: ByNodeType String
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
  { cores :: Int
  , memory :: Int
  , memory_max :: Int
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Resources

instance Aeson.FromJSON Resources

--------------------------------------------------------------------------------

-- | A topology as it's used to define benchmarking profiles.
data Analysis = Analysis
  { analysisType :: Maybe String -- TODO: Rename in workbench/bash to "analysis_type"
  , cluster_base_startup_overhead_s :: Int
  , start_log_spread_s :: Int
  , last_log_spread_s :: Int
  , silence_since_last_block_s :: Int
  , tx_loss_ratio :: Scientific.Scientific
  , finish_patience :: Int
  , filters :: [String]
  , filter_exprs :: [AnalysisFilterExpression]
  , minimum_chain_density :: Scientific.Scientific
  , cluster_startup_overhead_s :: Int
  }
  deriving (Eq, Show, Generic)

analysisOptions :: Aeson.Options
analysisOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = flm
  , Aeson.constructorTagModifier = ctm
  }
    where
      flm "analysisType" = "type"
      flm l = l
      ctm "analysisType" = "type"
      ctm t = t

instance Aeson.ToJSON Analysis where
    toEncoding = Aeson.genericToEncoding analysisOptions

instance Aeson.FromJSON Analysis where
    parseJSON = Aeson.genericParseJSON analysisOptions

data AnalysisFilterExpression = AnalysisFilterExpression
  { tag :: String
  , contents :: AnalysisFilterExpressionContent
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON AnalysisFilterExpression

instance Aeson.FromJSON AnalysisFilterExpression

data AnalysisFilterExpressionContent = AnalysisFilterExpressionContent
  { innerTag :: String
  , innerContents :: Int
  }
  deriving (Eq, Show, Generic)

analysisFilterExpressionContentOptions :: Aeson.Options
analysisFilterExpressionContentOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = flm
  , Aeson.constructorTagModifier = ctm
  }
    where
      flm "innerTag" = "tag"
      flm "innerContents" = "contents"
      flm l = l
      ctm "innerTag" = "tag"
      ctm "innerContents" = "contents"
      ctm t = t

instance Aeson.ToJSON AnalysisFilterExpressionContent where
    toEncoding = Aeson.genericToEncoding analysisFilterExpressionContentOptions

instance Aeson.FromJSON AnalysisFilterExpressionContent where
    parseJSON = Aeson.genericParseJSON analysisFilterExpressionContentOptions
