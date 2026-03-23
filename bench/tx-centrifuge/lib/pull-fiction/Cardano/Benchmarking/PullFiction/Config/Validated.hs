{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | Validated load-generator configuration with cascading defaults applied.
--
-- Types mirror "Cardano.Benchmarking.PullFiction.Config.Raw" but with hidden
-- data constructors. The only way to obtain values is through 'validate', which
-- guarantees that every value has passed validation (e.g. @tps > 0@, 
-- @max_batch_size >= 1@, valid names).
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
-- After 'validate', every 'Target' has a concrete @maxBatchSize@ and every
-- 'Workload' has a concrete @builder@ (no 'Maybe').
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
  , initialInputs, observers, workloads

    -- * Workload.
  , Workload
  , workloadName, builder, targets

    -- * RateLimitSource.
  , RateLimitSource (..)

    -- * Target.
  , Target
  , targetName
  , rateLimitSource
  , maxBatchSize, onExhaustion
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
------------------
-- pull-fiction --
------------------
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
    -- | Observers (keyed by name).
    -- Opaque; interpretation is the caller's responsibility.
  , observers     :: !(Map String Raw.Observer)
    -- | Workloads keyed by name. Iteration order is alphabetical (Map order).
  , workloads     :: !(Map String Workload)
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | A single workload with cascading defaults applied.
--
-- 'builder' is always concrete (no 'Maybe'); cascading from the top level
-- config is performed by 'validate'.
data Workload = Workload
  { -- | User provided name.
    workloadName :: !String
    -- | Resolved builder: workload value > top level value.
    -- Opaque; interpretation is the caller's responsibility.
  , builder      :: !Raw.Builder
    -- | Targets keyed by name. Iteration order is alphabetical.
  , targets      :: !(Map String Target)
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

-- | A target endpoint to connect to.
--
-- 'maxBatchSize' and 'onExhaustion' are concrete (no 'Maybe'). Cascading
-- defaults have been applied by 'validate'.
data Target = Target
  { -- | User provided name.
    targetName      :: !String
    -- | Resolved rate limit source ('Nothing' means unlimited).
  , rateLimitSource :: !(Maybe RateLimitSource)
    -- | Resolved max batch size.
    -- target value > workload value > top-level value > default (1).
  , maxBatchSize    :: !Natural
    -- | Resolved on-exhaustion behaviour.
    -- target value > workload value > top-level value > default (block).
  , onExhaustion    :: !Raw.OnExhaustion
  -- How to connect to the target.
  , addr            :: !String
  , port            :: !Int
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
  -- Observers. Always top-level and by name.
  -- (opaque; passed through without interpretation).
  let resolvedObservers = fromMaybe
                            Map.empty
                            (Raw.maybeObservers raw)
  -- Top level builder. Future iterations will have builders by name.
  -- (opaque; passed through without interpretation).
  let maybeTopBuilder = Raw.maybeTopLevelBuilder raw
  -- Top level rate limit.
  maybeTopRateLimit <-
    case Raw.maybeTopLevelRateLimit raw of
      Nothing                     -> pure Nothing
      Just (maybeTopScope, rawRL) -> do
        let topScope = fromMaybe
                         defaultTopLevelScope
                         maybeTopScope
        validatedRL <- validateRateLimit rawRL
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
  let rawWorkloads = fromMaybe
                       Map.empty
                       (Raw.maybeWorkloads raw)
  when (Map.null rawWorkloads) $
    Left "Config: at least one workload is required"
  workloadsMap <- Map.traverseWithKey
    (\name workload ->
      validateWorkload
        name
        maybeTopBuilder
        maybeTopRateLimit
        topMaxBatchSize
        topOnExhaustion
        workload
    )
    rawWorkloads
  -- Inputs must cover all workloads: Runtime.partitionInputs splits them into
  -- contiguous chunks, so fewer inputs than workloads leaves some with zero.
  let inputCount = length inputs
  when (inputCount < Map.size workloadsMap) $
    Left $ "Config: not enough initial inputs (" ++ show inputCount
        ++ ") for " ++ show (Map.size workloadsMap) ++ " workload(s)"
  -- Final validated config.
  pure Config
    { initialInputs = inputs
    , observers     = resolvedObservers
    , workloads     = workloadsMap
    }

--------------------------------------------------------------------------------

-- Returns 'Left' with a descriptive error message on the first violation.
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
  -- | The parsed workload from JSON.
  -> Raw.Workload
  -> Either String Workload
validateWorkload name
                 maybeTopBuilder
                 maybeTopRateLimit
                 topMaxBatchSize
                 topOnExhaustion
                 rawWorkload = do
  -- Name.
  validateName "Workload" name
  -- Builder conflict: setting at both levels is ambiguous.
  case (maybeTopBuilder, Raw.maybeBuilder rawWorkload) of
    (Just _, Just _) ->
      Left $ "builder set at both the top level and in workload: " ++ show name
    _ -> pure ()
  -- Resolve builder: workload level > top level > error.
  resolvedBuilder <- do
    case Raw.maybeBuilder rawWorkload of
      Just parsedBuilder -> do
        -- The top level builder gets ignored in favor of the workload builder.
        pure parsedBuilder
      Nothing -> do
        case maybeTopBuilder of
          Just topLevelBuilder -> pure topLevelBuilder
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
  effectiveRateLimit <- do
    case Raw.maybeRateLimit rawWorkload of
      -- There is a rate limit at the workload level.
      Just (maybeWlScope, rawRL) -> do
        validatedRL <- validateRateLimit rawRL
        let wlScope = fromMaybe
                        defaultWorkloadScope
                        maybeWlScope
        -- `Right` workload scope.
        pure (Just (Right wlScope, validatedRL))
      -- There is no rate limit at the workload level.
      Nothing -> do
        case maybeTopRateLimit of
          Just (topScope, validatedTopRL) -> do
            -- `Left` top level scope.
            pure (Just (Left topScope, validatedTopRL))
          Nothing -> do
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

-- Returns 'Left' with a descriptive error message on the first violation.
validateTarget
  -- | Workload name (for cache key computation).
  :: String
  -- | Target name (from Map key).
  -> String
  -- | If 'Just': 'Left' is top level scope, 'Right' is workload scope.
  -> Maybe (Either Raw.TopLevelScope Raw.WorkloadScope, Raw.RateLimit)
  -- | Resolved max batch size.
  -> Natural
  -- | Resolved on-exhaustion behaviour.
  -> Raw.OnExhaustion
  -- The target parsed from JSON.
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
  let maybeRateLimitSource =
        case effectiveRateLimit of
          Nothing -> Nothing
          Just (scope, rl) -> Just $ case scope of
            -- Using the scope set at the top level rate limit.
            Left  Raw.TopShared         -> RateLimitSource "@global" rl
            Left  Raw.TopPerWorkload    -> RateLimitSource wlName rl
            Left  Raw.TopPerTarget      -> RateLimitSource (wlName++"."++tgtName) rl
            -- Using scope set at the workload level rate limit.
            Right Raw.WorkloadShared    -> RateLimitSource wlName rl
            Right Raw.WorkloadPerTarget -> RateLimitSource (wlName++"."++tgtName) rl
  -- Cascade max_batch_size: target > workload (always concrete).
  case Raw.maybeTargetMaxBatchSize rawTarget of
    Just 0 -> Left $
      "Target " ++ show tgtName
        ++ ": max_batch_size must be >= 1"
    _ -> pure ()
  -- Cascade max_batch_size: target > workload (always concrete).
  let resolvedMaxBatchSize = fromMaybe
                               workloadBatchSize
                               (Raw.maybeTargetMaxBatchSize rawTarget)
  -- Cascade on_exhaustion: target > workload (always concrete).
  let resolvedOnExhaustion = fromMaybe
                               workloadOnExhaustion
                               (Raw.maybeTargetOnExhaustion rawTarget)
  -- Final validated target.
  pure Target
    { targetName      = tgtName
    , rateLimitSource = maybeRateLimitSource
    , maxBatchSize    = resolvedMaxBatchSize
    , onExhaustion    = resolvedOnExhaustion
    , addr            = Raw.addr rawTarget
    , port            = Raw.port rawTarget
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

