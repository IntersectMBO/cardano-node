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

    -- * Builder.
  , Builder (..)

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
import Data.Aeson.Types qualified as Aeson.Types
----------------
-- containers --
----------------
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

-- | Top-level configuration as parsed from JSON.
--
-- No invariants are enforced. Use 'validate' from
-- "Cardano.Benchmarking.PullFiction.Config.Validated" to apply business
-- rules and cascading defaults.
data Config = Config
  { -- | Raw JSON value describing how to load initial inputs.
    -- Interpretation is left to the caller (e.g. @Main.hs@).
    initialInputs :: !Aeson.Value
    -- | Optional top level @\"builder\"@.
  , maybeTopLevelBuilder :: !(Maybe Builder)
    -- | Optional top-level @\"rate_limit\"@.
  , maybeTopLevelRateLimit :: !(Maybe (Maybe TopLevelScope, RateLimit))
    -- | Optional top-level @\"max_batch_size\"@.
  , maybeTopLevelMaxBatchSize :: !(Maybe Natural)
    -- | Optional top-level @\"on_exhaustion\"@.
  , maybeTopLevelOnExhaustion :: !(Maybe OnExhaustion)
    -- | Generator workloads keyed by name. Because Aeson decodes JSON objects
    -- into a 'Map', duplicate workload names are silently discarded (last
    -- value wins).
  , workloads :: !(Map String Workload)
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \o ->
    Config
      <$> o .:  "initial_inputs"
      <*> o .:? "builder"
      <*> Aeson.Types.explicitParseFieldMaybe parseTopLevelRateLimit o "rate_limit"
      <*> o .:? "max_batch_size"
      <*> o .:? "on_exhaustion"
      <*> o .:  "workloads"

--------------------------------------------------------------------------------

-- | Opaque builder configuration.
--
-- Carries a @\"type\"@ discriminator and an opaque @\"params\"@ object.
-- Interpretation of the params is the caller's responsibility (see @Main.hs@),
-- just like 'initialInputs'.
data Builder = Builder
  { -- | Builder variant name (e.g. @\"value\"@). Non-empty.
    builderType :: !String
    -- | Opaque params object for the variant.
  , builderParams :: !Aeson.Value
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Builder where
  parseJSON = Aeson.withObject "Builder" $ \o -> do
    ty <- o .: "type" :: Aeson.Types.Parser String
    when (null ty) $ fail "Builder: type must be non-empty"
    Builder ty <$> o .: "params"

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
  maybeScopeStr <- o .:? "scope"
  maybeScope <- case maybeScopeStr of
    Nothing -> pure Nothing
    Just s  -> Just <$> scopeParser s
  ty <- o .: "type" :: Aeson.Types.Parser String
  case ty of
    "token_bucket" -> do
      p <- o .: "params"
      rl <- TokenBucket <$> p .: "tps"
      pure (maybeScope, rl)
    _ -> fail $
      "RateLimit: unknown type " ++ show ty ++ ", expected \"token_bucket\""

parseTopLevelRateLimit :: Aeson.Value
                       -> Aeson.Types.Parser (Maybe TopLevelScope, RateLimit)
parseTopLevelRateLimit = parseRateLimit topLevelScopeParser

parseWorkloadRateLimit :: Aeson.Value
                       -> Aeson.Types.Parser (Maybe WorkloadScope, RateLimit)
parseWorkloadRateLimit = parseRateLimit workloadScopeParser

topLevelScopeParser :: String -> Aeson.Types.Parser TopLevelScope
topLevelScopeParser "shared"       = pure TopShared
topLevelScopeParser "per_workload" = pure TopPerWorkload
topLevelScopeParser "per_target"   = pure TopPerTarget
topLevelScopeParser s              = fail $ "RateLimit: unknown scope " ++ show s

workloadScopeParser :: String -> Aeson.Types.Parser WorkloadScope
workloadScopeParser "shared"     = pure WorkloadShared
workloadScopeParser "per_target" = pure WorkloadPerTarget
workloadScopeParser s            = fail $
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
    -- | Targets keyed by name. Duplicate target names are silently discarded
    -- (last value wins) because Aeson decodes JSON objects into a 'Map'.
  , targets :: !(Map String Target)
  }
  deriving (Show, Eq)

instance Aeson.FromJSON Workload where
  parseJSON = Aeson.withObject "Workload" $ \o ->
    Workload
      <$> o .:? "builder"
      <*> Aeson.Types.explicitParseFieldMaybe parseWorkloadRateLimit o "rate_limit"
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
  parseJSON = Aeson.withObject "Target" $ \o ->
    Target
      <$> o .:? "max_batch_size"
      <*> o .:? "on_exhaustion"
      <*> o .:  "addr"
      <*> o .:  "port"
