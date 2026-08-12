{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- | Raw load-generator configuration parsed from JSON.
--
-- A plain Aeson parser with no extra logic. Each 'FromJSON' instance is a
-- direct transformation from JSON values to Haskell base types ('String',
-- 'Natural', 'Double', 'Int', etc.); optional fields are 'Maybe' and named
-- collections are @'Map' 'String'@. No defaults are applied, no business rules
-- are checked, and no cross-field relationships are enforced.
-- All of that is the responsibility of
-- "Cardano.Benchmarking.PullFiction.Config.Validated".
--
-- All data constructors and fields are exported so that test code can build
-- configuration values directly without going through JSON.
--
-- __Import qualified.__ Field names clash with
-- "Cardano.Benchmarking.PullFiction.Config.Validated" and
-- "Cardano.Benchmarking.PullFiction.Config.Runtime".
module Cardano.Benchmarking.PullFiction.Config.Raw
  (
    -- * Config.
    Config (..)

    -- * Inputs.
  , InitialInputs (..)
  , InputSource (..)

    -- * Observer.
  , Observer (..)

    -- * Builder.
  , Builder (..)
  , Recovery (..)

    -- * Recycle strategy.
  , RecycleStrategy (..)

    -- * RateLimit.
  , RateLimit (..)
    -- ** TopLevelScope.
  , TopLevelScope (..)
    -- ** WorkloadScope.
  , WorkloadScope (..)

    -- * OnExhaustion.
  , OnExhaustion (..)

    -- * Workload.
  , Workload (..)

    -- * Target.
  , Target (..)

  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Monad (when)
import Numeric.Natural (Natural)
-----------
-- aeson --
-----------
import Data.Aeson qualified as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson.Types
----------------
-- containers --
----------------
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

-- | Fail if the JSON object contains fields not in the given list.
-- Catches typos in configuration files early.
noUnknownFields :: String -> Aeson.Object -> [String] -> Aeson.Types.Parser ()
noUnknownFields name obj known =
  let unknown = filter (`notElem` map Key.fromString known) (KeyMap.keys obj)
  in  when (not (null unknown)) $
        fail $ name ++ ": unrecognized field(s): "
             ++ unwords (map (show . Key.toString) unknown)

--------------------------------------------------------------------------------

-- | Top-level configuration as parsed from JSON.
--
-- No invariants are enforced. Use 'validate' from
-- "Cardano.Benchmarking.PullFiction.Config.Validated" to apply business
-- rules and cascading defaults.
data Config = Config
  { -- | Which 'InputSource' loads the initial inputs, plus optional opaque
    -- use-site params. Interpretation of the params is the caller's
    -- responsibility (e.g. @Main.hs@).
    initialInputs :: !InitialInputs
    -- | Optional @\"input_sources\"@ map (keyed by name).
    -- Because Aeson decodes JSON objects into a 'Map', duplicate source names
    -- are silently discarded (last value wins).
  , maybeInputSources :: !(Maybe (Map String InputSource))
    -- | Optional @\"observers\"@ map (keyed by name).
    -- Because Aeson decodes JSON objects into a 'Map', duplicate observer names
    -- are silently discarded (last value wins).
  , maybeObservers :: !(Maybe (Map String Observer))
    -- | Optional top level @\"builder\"@.
  , maybeTopLevelBuilder :: !(Maybe Builder)
    -- | Optional top-level @\"rate_limit\"@.
  , maybeTopLevelRateLimit :: !(Maybe (Maybe TopLevelScope, RateLimit))
    -- | Optional top-level @\"max_batch_size\"@.
  , maybeTopLevelMaxBatchSize :: !(Maybe Natural)
    -- | Optional top-level @\"on_exhaustion\"@.
  , maybeTopLevelOnExhaustion :: !(Maybe OnExhaustion)
    -- | Optional top-level @\"startup_delay_seconds\"@.
  , maybeStartupDelaySeconds :: !(Maybe Natural)
    -- | Optional generator workloads keyed by name.
    -- Because Aeson decodes JSON objects into a 'Map', duplicate workload names
    -- are silently discarded (last value wins).
  , maybeWorkloads :: !(Maybe (Map String Workload))
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Config where
  -- No noUnknownFields: the top-level JSON carries additional fields
  -- parsed by the caller (tracing, nodeConfig, protocol_parameters, etc.).
  parseJSON = Aeson.withObject "Config" $ \o ->
    Config
      <$> o .:  "initial_inputs"
      <*> o .:? "input_sources"
      <*> o .:? "observers"
      <*> o .:? "builder"
      <*> Aeson.Types.explicitParseFieldMaybe parseTopLevelRateLimit o
                "rate_limit"
      <*> o .:? "max_batch_size"
      <*> o .:? "on_exhaustion"
      <*> o .:? "startup_delay_seconds"
      <*> o .:? "workloads"

--------------------------------------------------------------------------------

-- | The @initial_inputs@ reference: which 'InputSource' loads the initial
-- inputs, plus optional use-site params. The params are opaque like
-- 'builderParams', their shape depends on the source's type and interpretation
-- is the caller's responsibility.
data InitialInputs = InitialInputs
  { -- | Name of the 'InputSource' to load from.
    initialInputsSource :: !String
    -- | Optional opaque params for the load (e.g. which signing keys).
  , initialInputsParams :: !(Maybe Aeson.Value)
  }
  deriving (Show, Eq)

instance Aeson.FromJSON InitialInputs where
  parseJSON = Aeson.withObject "InitialInputs" $ \o -> do
    noUnknownFields "InitialInputs" o ["source", "params"]
    InitialInputs
      <$> o .:  "source"
      <*> o .:? "params"

-- | Opaque input source configuration: a way to obtain inputs, referenced by
-- @initial_inputs@ (the startup load) and by builder recoveries (rebuilding
-- the queued inputs after a reset).
--
-- Carries a @\"type\"@ discriminator and an opaque @\"params\"@ object.
-- Interpretation of the params is the caller's responsibility (see @Main.hs@),
-- like 'Observer' and 'Builder'.
data InputSource = InputSource
  { -- | Source variant (e.g. @\"utxo_query\"@ @\"genesis_utxo_keys\"@).
    -- Non-empty.
    inputSourceType :: !String
    -- | Opaque params object for the variant.
  , inputSourceParams :: !Aeson.Value
  }
  deriving (Show, Eq)

instance Aeson.FromJSON InputSource where
  parseJSON = Aeson.withObject "InputSource" $ \o -> do
    noUnknownFields "InputSource" o ["type", "params"]
    ty <- o .: "type" :: Aeson.Types.Parser String
    when (null ty) $ fail "InputSource: \"type\" must be non-empty"
    InputSource ty <$> o .: "params"

--------------------------------------------------------------------------------

-- | Opaque observer configuration.
--
-- Carries a @\"type\"@ discriminator and an opaque @\"params\"@ object.
-- Interpretation of the params is the caller's responsibility (see @Main.hs@),
-- like 'initialInputs' and 'Builder'.
data Observer = Observer
  { -- | Observer variant (e.g. @\"nodetonode\"@ @\"nodetoclient\"@). Non-empty.
    observerType :: !String
    -- | Opaque params object for the variant.
  , observerParams :: !Aeson.Value
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Observer where
  parseJSON = Aeson.withObject "Observer" $ \o -> do
    noUnknownFields "Observer" o ["type", "params"]
    ty <- o .: "type" :: Aeson.Types.Parser String
    when (null ty) $ fail "Observer: \"type\" must be non-empty"
    Observer ty <$> o .: "params"

--------------------------------------------------------------------------------

-- | Opaque builder configuration.
--
-- Carries a @\"type\"@ discriminator and an opaque @\"params\"@ object.
-- Interpretation of the params is the caller's responsibility (see @Main.hs@),
-- like 'observer' and 'initialInputs'.
data Builder = Builder
  { -- | Builder variant name (e.g. @\"value\"@). Non-empty.
    builderType :: !String
    -- | Opaque params object for the variant.
  , builderParams :: !Aeson.Value
    -- | Optional recycle strategy. 'Nothing' means no recycling.
  , builderRecycle :: !(Maybe RecycleStrategy)
    -- | Optional rollback recovery. 'Nothing' means no recovery.
  , builderRecovery :: !(Maybe Recovery)
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Builder where
  parseJSON = Aeson.withObject "Builder" $ \o -> do
    noUnknownFields "Builder" o ["type", "params", "recycle", "recovery"]
    ty <- o .: "type" :: Aeson.Types.Parser String
    when (null ty) $ fail "Builder: \"type\" must be non-empty"
    Builder ty <$> o .:  "params"
               <*> o .:? "recycle"
               <*> o .:? "recovery"

--------------------------------------------------------------------------------

-- | A builder's rollback recovery: when one of its payloads is orphaned,
-- discard the workload's queued inputs and reseed them from the named
-- 'InputSource'. Observers are independent entities: several builders may name
-- the same observer, each choosing its own recovery.
data Recovery = Recovery
  { -- | Observer whose orphan events trigger the recovery. Optional for
    -- @on_confirm@ (defaults to the confirm observer), required for @on_build@
    -- and @on_pull@.
    recoveryObserver :: !(Maybe String)
    -- | Name of the 'InputSource' that rebuilds the queued inputs.
  , recoverySource :: !String
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Recovery where
  parseJSON = Aeson.withObject "Recovery" $ \o -> do
    noUnknownFields "Recovery" o ["observer", "source"]
    Recovery
      <$> o .:? "observer"
      <*> o .:  "source"

--------------------------------------------------------------------------------

-- | When to recycle transaction outputs back to the input queue.
data RecycleStrategy
  -- | Recycle immediately after building, before entering the payload queue.
  = RecycleOnBuild
  -- | Recycle when a worker dequeues the payload from the queue.
  | RecycleOnDequeue
  -- | Recycle when an observer confirms the payload. Carries the observer
  -- name.
  | RecycleOnConfirm !String
  deriving (Show, Eq)

instance Aeson.FromJSON RecycleStrategy where
  parseJSON = Aeson.withObject "RecycleStrategy" $ \o -> do
    noUnknownFields "RecycleStrategy" o ["type", "params"]
    ty <- o .: "type" :: Aeson.Types.Parser String
    mParams <- o .:? "params" :: Aeson.Types.Parser (Maybe Aeson.Value)
    case (ty, mParams) of
      -- on_build and on_pull take no params, fail instead of silently
      -- ignoring them.
      ("on_build",   Nothing) -> pure RecycleOnBuild
      ("on_build",   Just _)  ->
        fail "RecycleStrategy on_build: takes no \"params\""
      -- TODO: rename the JSON value "on_pull" to "on_dequeue", the strategy
      -- recycles when the payload is DEQUEUED from the pipe, not on a
      -- TxSubmission "pull".
      -- Kept as "on_pull" for backward compatibility with existing configs.
      ("on_pull",    Nothing) -> pure RecycleOnDequeue
      ("on_pull",    Just _)  ->
        fail "RecycleStrategy on_pull: takes no \"params\""
      -- on_confirm params: the observer name.
      ("on_confirm", Just v)  -> RecycleOnConfirm <$> Aeson.Types.parseJSON v
      ("on_confirm", Nothing) ->
        fail "RecycleStrategy on_confirm: missing \"params\""
      _ -> fail $ "RecycleStrategy: unknown \"type\" " ++ show ty
                ++ ", expected \"on_build\", \"on_pull\", or \"on_confirm\""

--------------------------------------------------------------------------------

-- | Scope of a top-level rate limiter.
--
-- There is no @Distributed@ scope. A \"distributed\" mode would be equivalent
-- to 'TopPerWorkload' or 'TopPerTarget' but with the TPS divided internally by
-- the number of sub-entities. We avoid that: the config should state the
-- per-entity TPS directly so the value is explicit and auditable.
data TopLevelScope
  -- | One rate limiter shared by all targets across all workloads.
  = TopShared
  -- | Each workload gets its own rate limiter at the full configured TPS.
  | TopPerWorkload
  -- | Each target gets its own rate limiter at the full configured TPS.
  | TopPerTarget
  deriving (Show, Eq)

-- | Scope of a workload-level rate limiter.
--
-- 'TopPerWorkload' is not valid here (we are already at the workload level).
data WorkloadScope
  -- | One rate limiter shared by all targets in the workload.
  = WorkloadShared
  -- | Each target gets its own rate limiter at the full configured TPS.
  | WorkloadPerTarget
  deriving (Show, Eq)

-- | Rate limit configuration.
--
-- Scope is not part of the rate limit itself; it is carried alongside the
-- 'RateLimit' in the enclosing tuple (e.g. @(TopLevelScope, RateLimit)@).
--
-- The JSON representation uses @\"type\"@ + @\"params\"@ at the same level;
-- the parser flattens the nested @\"params\"@ object into the constructor.
data RateLimit
  = TokenBucket
    { -- | Target tokens per second.
      tps :: !Double
    }
  deriving (Show, Eq)

-- | Parse a rate limit from JSON using a context-specific scope parser.
--
-- Scope is optional (defaults to @\"shared\"@ at validation time) and parsed
-- first; it is not part of 'RateLimit'.
--
-- At the top level, use 'parseTopLevelRateLimit' (accepts @\"shared\"@,
-- @\"per_workload\"@, @\"per_target\"@).
-- At the workload level, use 'parseWorkloadRateLimit' (accepts @\"shared\"@,
-- @\"per_target\"@).
parseRateLimit
  :: (String -> Aeson.Types.Parser scope)
  -> Aeson.Value
  -> Aeson.Types.Parser (Maybe scope, RateLimit)
parseRateLimit scopeParser = Aeson.withObject "RateLimit" $ \o -> do
  noUnknownFields "RateLimit" o ["type", "params", "scope"]
  maybeScopeStr <- o .:? "scope"
  maybeScope <- case maybeScopeStr of
    Nothing -> pure Nothing
    Just s  -> Just <$> scopeParser s
  ty <- o .: "type" :: Aeson.Types.Parser String
  case ty of
    "token_bucket" -> do
      op <- o .: "params"
      noUnknownFields "RateLimit.params" op ["tps"]
      rl <- TokenBucket <$> op .: "tps"
      pure (maybeScope, rl)
    _ -> fail $
      "RateLimit: unknown \"type\" " ++ show ty ++ ", expected \"token_bucket\""

parseTopLevelRateLimit :: Aeson.Value
                       -> Aeson.Types.Parser (Maybe TopLevelScope, RateLimit)
parseTopLevelRateLimit = parseRateLimit parseTopLevelScope

parseWorkloadRateLimit :: Aeson.Value
                       -> Aeson.Types.Parser (Maybe WorkloadScope, RateLimit)
parseWorkloadRateLimit = parseRateLimit parseWorkloadScope

parseTopLevelScope :: String -> Aeson.Types.Parser TopLevelScope
parseTopLevelScope "shared"       = pure TopShared
parseTopLevelScope "per_workload" = pure TopPerWorkload
parseTopLevelScope "per_target"   = pure TopPerTarget
parseTopLevelScope s              = fail $ "RateLimit: unknown scope " ++ show s

parseWorkloadScope :: String -> Aeson.Types.Parser WorkloadScope
parseWorkloadScope "shared"     = pure WorkloadShared
parseWorkloadScope "per_target" = pure WorkloadPerTarget
parseWorkloadScope s            = fail $
  "RateLimit: unknown scope " ++ show s
    ++ "; at workload level, only \"shared\" and \"per_target\" are valid"

--------------------------------------------------------------------------------

-- | What to do when the payload queue, the output of the builder stage, is
-- exhausted.
data OnExhaustion
  -- | Block / wait.
  = Block
  -- | Fail immediately with an error.
  | Error
  deriving (Show, Eq)

instance Aeson.FromJSON OnExhaustion where
  parseJSON = Aeson.withText "OnExhaustion" $ \t -> case t of
    "block" -> pure Block
    "error" -> pure Error
    _       -> fail $
      "OnExhaustion: expected \"block\" or \"error\", got " ++ show t

--------------------------------------------------------------------------------

-- | Configuration for a single workload as parsed from JSON.
--
-- The workload name is the 'Map' key in the parent 'Config'; it is not stored
-- inside the record.
data Workload = Workload
  { -- | Optional builder for this workload.
    maybeBuilder :: !(Maybe Builder)
    -- | Optional rate limit for this workload.
  , maybeRateLimit :: !(Maybe (Maybe WorkloadScope, RateLimit))
    -- | Optional max tokens per request.
  , maybeMaxBatchSize :: !(Maybe Natural)
    -- | Optional on-exhaustion behaviour.
  , maybeOnExhaustion :: !(Maybe OnExhaustion)
    -- | Targets keyed by name.
    -- Because Aeson decodes JSON objects into a 'Map', duplicate target names
    -- are silently discarded (last value wins).
  , targets :: !(Map String Target)
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Workload where
  parseJSON = Aeson.withObject "Workload" $ \o -> do
    noUnknownFields "Workload" o
      ["builder", "rate_limit", "max_batch_size", "on_exhaustion", "targets"]
    Workload
      <$> o .:? "builder"
      <*> Aeson.Types.explicitParseFieldMaybe parseWorkloadRateLimit o
                "rate_limit"
      <*> o .:? "max_batch_size"
      <*> o .:? "on_exhaustion"
      <*> o .:  "targets"

--------------------------------------------------------------------------------

-- | A target endpoint to connect to.
--
-- The target name is the 'Map' key in the parent 'Workload'; it is not stored
-- inside the record.
data Target = Target
  { -- | Optional per-target @\"max_batch_size\"@ override.
    maybeTargetMaxBatchSize :: !(Maybe Natural)
    -- | Optional per-target @\"on_exhaustion\"@ override.
  , maybeTargetOnExhaustion :: !(Maybe OnExhaustion)
  , addr :: !String
  , port :: !Int
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Target where
  parseJSON = Aeson.withObject "Target" $ \o -> do
    noUnknownFields "Target" o
      ["max_batch_size", "on_exhaustion", "addr", "port"]
    Target
      <$> o .:? "max_batch_size"
      <*> o .:? "on_exhaustion"
      <*> o .:  "addr"
      <*> o .:  "port"
