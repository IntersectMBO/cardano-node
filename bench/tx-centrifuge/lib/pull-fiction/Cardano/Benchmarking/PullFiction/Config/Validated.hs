{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | Validated load-generator configuration with cascading defaults applied.
--
-- Types mirror "Cardano.Benchmarking.PullFiction.Config.Raw" but with
-- hidden data constructors. The only way to obtain values is through
-- 'validate', which guarantees that every value has passed validation (e.g.
-- @tps > 0@, @max_batch_size >= 1@, valid names).
--
-- Cascading defaults are resolved here:
--
-- * @builder@: setting it at both the top level and the workload level is an
--   error; otherwise workload value > top level value > error.
-- * @rate_limit@: setting it at both the top level and the workload level is an
--   error; otherwise the workload inherits the top-level value (or 'Nothing'
--   for unlimited).
-- * @max_batch_size@: target value > workload value > top-level value >
--   default (1).
-- * @on_exhaustion@: target value > workload value > top-level value >
--   default (@\"block\"@).
--
-- After 'validate', every 'Target' has a concrete @targetMaxBatchSize@ and
-- every 'Workload' has a concrete @builder@ (no 'Maybe').
--
-- 'Workload' and 'Config' store their children in 'Map's keyed by name
-- (alphabetical order; JSON object key order is not preserved).
--
-- __Import qualified.__ Field names clash with
-- "Cardano.Benchmarking.PullFiction.Config.Raw" and
-- "Cardano.Benchmarking.PullFiction.Config.Runtime".
module Cardano.Benchmarking.PullFiction.Config.Validated
  (
    -- * Config.
    Config
  , initialInputs, workloads

    -- * RateLimitSource.
  , RateLimitSource (..)

    -- * Workload.
  , Workload
  , workloadName, builder, targets

    -- * Target.
  , Target
  , targetName, rateLimitSource, targetMaxBatchSize, onExhaustion
  , addr, port

    -- * Validation.
  , validate

  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
----------------
-- containers --
----------------
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
---------------------
-- pull-fiction --
---------------------
import Cardano.Benchmarking.PullFiction.Config.Raw qualified as Raw

--------------------------------------------------------------------------------
-- Defaults.
--------------------------------------------------------------------------------

-- | Default scope for a top-level rate limiter when not specified in JSON.
defaultTopLevelScope :: Raw.TopLevelScope
defaultTopLevelScope = Raw.TopShared

-- | Default scope for a workload-level rate limiter when not specified in JSON.
defaultWorkloadScope :: Raw.WorkloadScope
defaultWorkloadScope = Raw.WorkloadShared

-- | Default maximum batch size when neither the workload nor the top-level
-- config specifies one.
defaultMaxBatchSize :: Natural
defaultMaxBatchSize = 1

-- | Default on-exhaustion behaviour when not specified at any level.
defaultOnExhaustion :: Raw.OnExhaustion
defaultOnExhaustion = Raw.Block

--------------------------------------------------------------------------------

-- | Top-level configuration.
--
-- See 'Raw.Config' for field semantics. All invariants have been checked and
-- cascading defaults applied by 'validate'.
data Config input = Config
  { -- | Initial inputs provided by the caller and stored by 'validate'.
    initialInputs :: !(NonEmpty input)
    -- | Workloads keyed by name. Iteration order is alphabetical (Map order).
  , workloads :: !(Map String Workload)
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | Resolved rate limit for a target, with its sharing key pre-computed.
--
-- The 'rateLimitKey' encodes the sharing boundary using fully qualified names:
--
-- * @\@global@: one limiter shared by all targets across all workloads.
-- * @workloadName@: one limiter per workload (each shared by all its targets).
-- * @workloadName.targetName@: one limiter per target.
--
-- Because workload and target names may not start with @\@@ or contain @.@
-- (enforced at validation time), these keys are guaranteed to be unique.
data RateLimitSource = RateLimitSource
  { -- | Cache key for limiter sharing (the fully qualified name).
    rateLimitKey :: !String
    -- | Validated rate limit parameters.
  , rateLimit    :: !Raw.RateLimit
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | A single workload with cascading defaults applied.
--
-- 'builder' is always concrete (no 'Maybe'); cascading from the top level
-- config is performed by 'validate'.
data Workload = Workload
  { workloadName :: !String
    -- | Resolved builder: workload value > top level value.
    -- Opaque; interpretation is the caller's responsibility.
  , builder :: !Raw.Builder
    -- | Targets keyed by name. Iteration order is alphabetical.
  , targets :: !(Map String Target)
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | A target endpoint to connect to.
--
-- 'targetMaxBatchSize' and 'onExhaustion' are concrete (no 'Maybe'). Cascading
-- defaults have been applied by 'validate'.
data Target = Target
  { targetName :: !String
    -- | Resolved rate limit source ('Nothing' means unlimited).
  , rateLimitSource :: !(Maybe RateLimitSource)
    -- | Resolved max batch size: target value > workload value > top-level value
    -- > default.
  , targetMaxBatchSize :: !Natural
    -- | Resolved on-exhaustion behaviour: target value > workload value >
    -- top-level value > 'Raw.Block'.
  , onExhaustion :: !Raw.OnExhaustion
  , addr :: !String
  , port :: !Int
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Validation.
--------------------------------------------------------------------------------

-- | Validate a 'Raw.Config', enforce all business rules, and cascade top-level
-- defaults into workloads.
--
-- Input loading is the caller's responsibility; passes the already-loaded
-- inputs directly. This keeps the validation layer pure and decouples it from
-- IO concerns like key loading and network magic interpretation.
--
-- Returns 'Left' with a descriptive error message on the first violation.
validate
  -- | Raw configuration as parsed from JSON.
  :: Raw.Config
  -- | Initial inputs (already loaded by the caller).
  -> NonEmpty input
  -> Either String (Config input)
validate raw inputs = do
  -- Builder (opaque; passed through without interpretation).
  let maybeTopBuilder = Raw.maybeTopLevelBuilder raw
  -- Rate limit.
  maybeTopRateLimit <- case Raw.maybeTopLevelRateLimit raw of
    Nothing                     -> pure Nothing
    Just (maybeTopScope, rawRL) -> do
      validatedRL <- validateRateLimit rawRL
      let topScope = fromMaybe defaultTopLevelScope maybeTopScope
      pure (Just (topScope, validatedRL))
  -- Max batch size.
  let topMaxBatchSize = fromMaybe
                          defaultMaxBatchSize
                          (Raw.maybeTopLevelMaxBatchSize raw)
  when (topMaxBatchSize == 0) $
    Left "Config: max_batch_size must be >= 1"
  -- On-exhaustion behaviour.
  let topOnExhaustion = fromMaybe
                          defaultOnExhaustion
                          (Raw.maybeTopLevelOnExhaustion raw)
  -- Workloads.
  when (Map.null (Raw.workloads raw)) $
    Left "Config: workloads must not be empty"
  workloadsMap <- Map.traverseWithKey
    (\wName workload -> validateWorkload
      wName maybeTopBuilder maybeTopRateLimit topMaxBatchSize topOnExhaustion workload
    )
    (Raw.workloads raw)
  -- Inputs must cover all workloads: Runtime.partitionInputs splits them into
  -- contiguous chunks, so fewer inputs than workloads leaves some with zero.
  let inputCount = length inputs
  when (inputCount < Map.size workloadsMap) $
    Left $ "Config: not enough initial inputs (" ++ show inputCount
        ++ ") for " ++ show (Map.size workloadsMap) ++ " workload(s)"
  -- Final validated config.
  pure Config
    { initialInputs = inputs
    , workloads    = workloadsMap
    }

--------------------------------------------------------------------------------

validateWorkload
  -- | Workload name (from Map key).
  :: String
  -- | Top level builder (opaque).
  -> Maybe Raw.Builder
  -- | Validated top-level scope / rate limit.
  -> Maybe (Raw.TopLevelScope, Raw.RateLimit)
  -- | Resolved top-level max batch size.
  -> Natural
  -- | Resolved top-level on-exhaustion behaviour.
  -> Raw.OnExhaustion
  -> Raw.Workload
  -> Either String Workload
validateWorkload name maybeTopBuilder maybeTopRateLimit topMaxBatchSize topOnExhaustion rawWorkload = do
  -- Name.
  validateName "Workload" name
  -- Builder conflict: setting at both levels is ambiguous.
  case (maybeTopBuilder, Raw.maybeBuilder rawWorkload) of
    (Just _, Just _) ->
      Left $ "builder set at both the top level and in workload: " ++ show name
    _ -> pure ()
  -- Resolve builder: workload level > top level > error.
  resolvedBuilder <- case Raw.maybeBuilder rawWorkload of
    Just parsedBuilder  -> pure parsedBuilder
    Nothing -> case maybeTopBuilder of
      Just topLevelBuilder  -> pure topLevelBuilder
      Nothing -> Left $
        "Workload " ++ show name
          ++ ": builder is required (no workload or top level default)"
  -- Rate-limit conflict: setting at both levels is ambiguous.
  case (maybeTopRateLimit, Raw.maybeRateLimit rawWorkload) of
    (Just _, Just _) ->
      Left $
        "rate_limit is set at both the top level and in workload: " ++ show name
    _ -> pure ()
  -- Resolve effective rate limit: workload-level > top-level > unlimited.
  -- The scope and validated rate limit are cascaded to validateTarget, which
  -- computes the final RateLimitSource (including the cache key).
  effectiveRateLimit <- case Raw.maybeRateLimit rawWorkload of
    Just (maybeWlScope, rawRL) -> do
      validatedRL <- validateRateLimit rawRL
      let wlScope = fromMaybe defaultWorkloadScope maybeWlScope
      pure (Just (Right wlScope, validatedRL))
    Nothing -> case maybeTopRateLimit of
      Just (topScope, topRL) ->
        pure (Just (Left topScope, topRL))
      Nothing ->
        pure Nothing
  -- Cascade max_batch_size: workload > top-level (always concrete).
  -- The per-target override is applied inside validateTarget.
  case Raw.maybeMaxBatchSize rawWorkload of
    Just 0 -> Left "Workload: max_batch_size must be >= 1"
    _      -> pure ()
  let workloadBatchSize = fromMaybe
                                    topMaxBatchSize
                                    (Raw.maybeMaxBatchSize rawWorkload)
  -- Cascade on_exhaustion: workload > top-level.
  let workloadOnExhaustion = fromMaybe
                               topOnExhaustion
                               (Raw.maybeOnExhaustion rawWorkload)
  -- Targets.
  when (Map.null (Raw.targets rawWorkload)) $
    Left $ "Workload " ++ show name ++ ": targets must not be empty"
  targetsMap <- Map.traverseWithKey
    (\tName target -> validateTarget
      name tName effectiveRateLimit workloadBatchSize workloadOnExhaustion target
    )
    (Raw.targets rawWorkload)
  -- Final validated workload.
  pure Workload
    { workloadName = name
    , builder      = resolvedBuilder
    , targets      = targetsMap
    }

validateTarget
  :: String   -- ^ Workload name (for cache key computation).
  -> String   -- ^ Target name (from Map key).
  -> Maybe (Either Raw.TopLevelScope Raw.WorkloadScope, Raw.RateLimit)
  -> Natural  -- ^ Resolved max batch size.
  -> Raw.OnExhaustion -- ^ Resolved on-exhaustion behaviour.
  -> Raw.Target
  -> Either String Target
validateTarget wlName tgtName effectiveRateLimit workloadBatchSize workloadOnExhaustion rawTarget = do
  -- Name.
  validateName "Target" tgtName
  -- Resolve rate limit source with pre-computed cache key.
  -- The key scheme uses fully-qualified names:
  --   @global              → one limiter for everything
  --   workloadName         → one per workload
  --   workloadName.target  → one per target
  let maybeRateLimitSource = case effectiveRateLimit of
        Nothing -> Nothing
        Just (scope, rl) -> Just $ case scope of
          Left  Raw.TopShared         -> RateLimitSource "@global" rl
          Left  Raw.TopPerWorkload    -> RateLimitSource wlName rl
          Left  Raw.TopPerTarget      -> RateLimitSource (wlName ++ "." ++ tgtName) rl
          Right Raw.WorkloadShared    -> RateLimitSource wlName rl
          Right Raw.WorkloadPerTarget -> RateLimitSource (wlName ++ "." ++ tgtName) rl
  -- Cascade max_batch_size: target > workload (always concrete).
  case Raw.maybeTargetMaxBatchSize rawTarget of
    Just 0 -> Left $
      "Target " ++ show tgtName
        ++ ": max_batch_size must be >= 1"
    _ -> pure ()
  let resolvedBatchSize = fromMaybe
                            workloadBatchSize
                            (Raw.maybeTargetMaxBatchSize rawTarget)
  -- Cascade on_exhaustion: target > workload (always concrete).
  let resolvedOnExhaustion = fromMaybe
                               workloadOnExhaustion
                               (Raw.maybeTargetOnExhaustion rawTarget)
  -- Final validated target.
  pure Target
    { targetName         = tgtName
    , rateLimitSource    = maybeRateLimitSource
    , targetMaxBatchSize = resolvedBatchSize
    , onExhaustion       = resolvedOnExhaustion
    , addr               = Raw.addr rawTarget
    , port               = Raw.port rawTarget
    }

--------------------------------------------------------------------------------

validateRateLimit :: Raw.RateLimit -> Either String Raw.RateLimit
validateRateLimit rl@(Raw.TokenBucket rawTps) = do
  when (isNaN rawTps) $
    Left "RateLimit: tps must be a number, got NaN"
  when (isInfinite rawTps) $
    Left "RateLimit: tps must be finite"
  when (rawTps <= 0) $
    Left "RateLimit: tps must be > 0"
  pure rl

-- | Validate that a name does not start with @\'@\'@ or contain @\'.\'@.
--
-- These characters are reserved for the rate-limit cache key scheme
-- (see 'RateLimitSource').
validateName :: String -> String -> Either String ()
validateName context name = do
  case name of
    [] ->
      Left $ context ++ ": name must be non-empty"
    ('@':_) ->
      Left $ context ++ ": name must not start with '@', got " ++ show name
    _ -> pure ()
  when ('.' `elem` name) $
    Left $ context ++ ": name must not contain '.', got " ++ show name

