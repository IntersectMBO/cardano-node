{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}


--------------------------------------------------------------------------------

module Test.PullFiction.Harness
  ( -- * Test results
    TestResult(..)
    -- * Naming helpers
  , targetName
  , nodeName
    -- * Running tests
  , resolveConfig
  , loadConfig
  , runTest
  , runTpsTest
  , runPipelineIsolationTest
    -- * Metrics & formatting
  , getDuration
  , formatMetrics
  , formatDuration
    -- * Assertions (pure)
  , checkElapsedTolerance
  , checkTpsTolerance
  , checkTargetFairness
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (threadDelay)
import Control.Exception (onException, throwIO)
import Control.Monad (forever, when)
import Data.IORef qualified as IORef
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
-----------
-- aeson --
-----------
import Data.Aeson qualified as Aeson
-----------
-- async --
-----------
import Control.Concurrent.Async qualified as Async
-----------
-- clock --
-----------
-- NOTE: System.Clock is used directly here (rather than PullFiction.Clock)
-- intentionally. The harness measures overall test wall-clock time, which is
-- independent of the rate-limiter's internal clock. Keeping them separate
-- ensures that test timing cannot be influenced by any future changes to
-- PullFiction.Clock.
import System.Clock qualified as Clock
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
---------------------
-- pull-fiction --
---------------------
import Cardano.Benchmarking.PullFiction.Config.Runtime qualified as Runtime
import Cardano.Benchmarking.PullFiction.Config.Validated qualified as Validated
import Cardano.Benchmarking.PullFiction.WorkloadRunner (runWorkload)

--------------------------------------------------------------------------------

-- | Aggregate results from a TPS test run.
data TestResult = TestResult
  { -- | Wall-clock time the test actually ran.
    elapsedSeconds :: !Double
    -- | Actual token count per target, keyed by target name.
  , targetCounts   :: !(Map.Map String Int)
  } deriving (Show)

--------------------------------------------------------------------------------
-- Naming helpers
--------------------------------------------------------------------------------

-- | Qualified target name: @\"workload\/target\"@. This is the key format used
-- by 'runTpsTest' for per-target counters.
targetName :: String -> String -> String
targetName workload target = workload ++ "/" ++ target

-- | Node name matching the config's @\"node-NN\"@ zero-padded naming scheme
-- (e.g. @\"node-01\"@, @\"node-50\"@).
nodeName :: Int -> String
nodeName i = "node-" ++ (if i < 10 then "0" else "") ++ show i

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

-- | Decode a JSON config with pre-built inputs and resolve into a
-- 'Runtime.Runtime'.
--
-- This is the common entry point for tests that need a resolved pipeline.
-- Uses a trivial builder (1 input per batch; the input itself is the payload)
-- so the pipeline exercises rate limiting and recycling without real
-- transaction building.  'loadConfig' is a thin wrapper for the common case
-- of @()@ inputs.
resolveConfig :: FilePath -> NE.NonEmpty input -> IO (Runtime.Runtime input input)
resolveConfig path inputs = do
  raw <- Aeson.eitherDecodeFileStrict path >>= either fail pure
  validated <- either fail pure $ Validated.validate raw inputs
  Runtime.resolve
    -- mkBuilder: 1 input per batch; input IS the payload; recycle the same
    -- input as output.
    (\_ _ _ -> pure Runtime.BuilderHandle
      { Runtime.bhInputsPerBatch = 1
      , Runtime.bhBuildPayload   = \is -> pure ((), head is, is)
      })
    -- mkObserver: no test config uses observers.
    (\_ name _ -> fail $ "resolveConfig: unexpected observer: " ++ name)
    -- onRecycle: no-op.
    (\_ _ -> pure ())
    validated

-- | Load a generator config from a JSON file with dummy inputs and resolve into
-- a 'Runtime.Runtime'.
--
-- Useful for tests that only need config metadata (rate limits, targets) and do
-- not use the input pipeline.
loadConfig :: FilePath -> IO (Runtime.Runtime () ())
loadConfig path = resolveConfig path (() NE.:| [])

-- | Run the pipeline scaffolding shared by all test runners.
--
-- 'Runtime.resolve' has already spawned a builder async per workload (each
-- reads from the input queue, produces payloads, and enqueues them) and loaded
-- initial inputs.  This function spawns workers via 'runWorkload', races
-- them against the configured duration, then cancels all asyncs (builders and
-- workers).
--
-- @payload = input@ — the builder treats the input itself as the payload.
runTest
  :: Runtime.Runtime input input
  -> Double                           -- ^ Duration in seconds.
  -> (Runtime.Workload input input   -- ^ Workload the worker belongs to.
       -> Runtime.Target input input -- ^ Target the worker serves.
       -> IO input                    -- ^ Blocking fetch (rate-limited).
       -> IO (Maybe input)            -- ^ Non-blocking fetch.
       -> IO ()                       -- ^ Worker body.
     )
  -> IO Double                        -- ^ Elapsed wall-clock seconds.
runTest runtime durationSecs workerBody = do
  let allWorkloads = Map.elems (Runtime.workloads runtime)
  -- Start time.
  start <- Clock.getTime Clock.MonotonicRaw
  -- Spawn workers via runWorkload, passing the caller-supplied callbacks.
  -- Runtime asyncs (builders, recyclers) are already running.
  workers <- concat <$> mapM
    (\workload -> runWorkload workload $
      \target fetchPayload tryFetchPayload -> workerBody workload target fetchPayload tryFetchPayload
    )
    allWorkloads
  -- Race the test duration against any async dying. Exceptions are thrown
  -- synchronously (not via Async.link) so Tasty's withResource can properly
  -- cache and propagate them to all test cases in the group.
  let allAsyncs = Runtime.asyncs runtime ++ workers
      cancelAll = mapM_ Async.cancel allAsyncs
  winner <- Async.race
    (threadDelay (round (durationSecs * 1_000_000)))
    (Async.waitAnyCatch allAsyncs)
    `onException` cancelAll
  -- End time.
  end <- Clock.getTime Clock.MonotonicRaw
  cancelAll
  case winner of
    Right (_, Left ex) -> throwIO ex
    _                  -> pure ()
  -- Return with the elapsed time.
  pure $ fromIntegral (Clock.toNanoSecs (end - start)) / 1e9

-- | Decode a JSON config, create @()@ inputs, resolve into a
-- 'Runtime.Runtime', then run the pipeline, collecting per-target token
-- counts.
--
-- The pipeline is trivial: a builder thread reads @()@ from the input queue
-- and writes @((), [()])@ to the payload queue; 'runWorkload' handles rate
-- limiting and input recycling; the worker callback just increments a
-- per-target counter.
--
-- The caller is responsible for checking the returned 'TestResult' against its
-- own expected TPS map via 'checkTpsTolerance', 'checkTargetFairness', etc.
runTpsTest
  -- | Path to the JSON config file.
  :: FilePath
  -- | Test duration in seconds.
  -> Double
  -> IO TestResult
runTpsTest configPath durationSecs = do
  runtime <- resolveConfig configPath (() NE.:| replicate 99_999 ())
  -- Per-target counters keyed by "workloadName/targetName".
  let allTargets = concatMap
        (\wl -> map
                 (\rt ->
                   targetName (Runtime.workloadName wl) (Runtime.targetName rt)
                 )
          (Map.elems (Runtime.targets wl))
        )
        (Map.elems (Runtime.workloads runtime))
  counters <- Map.fromList <$> mapM
    (\key -> do
      ref <- IORef.newIORef (0 :: Int)
      pure (key, ref)
    )
    allTargets
  -- Each worker calls fetchPayload in a loop, increments its counter, and
  -- recycles the input back to the pipe for the builder to reuse.
  elapsed <- runTest runtime durationSecs $
    \workload target fetchPayload _tryFetchPayload -> do
      let key = targetName (Runtime.workloadName workload)
                           (Runtime.targetName target)
          ref     = counters Map.! key
      forever $ do
        _ <- fetchPayload
        IORef.atomicModifyIORef' ref (\c -> (c + 1, ()))
  -- Collect results.
  perTarget <- Map.fromList <$> mapM
    (\(key, ref) -> do
      c <- IORef.readIORef ref
      pure (key, c)
    )
    (Map.toList counters)
  -- Returns the map with the tokens per target.
  pure TestResult
    { elapsedSeconds = elapsed
    , targetCounts   = perTarget
    }

-- | Run a pipeline isolation test that verifies each workload's input recycling
-- loop is closed: inputs tagged for workload N are only ever observed by
-- workload N's workers, never by another workload.
--
-- Inputs are @(Int, Int)@ tuples where the first element is the workload index
-- and the second is an input identifier within that workload.
-- 'Runtime.resolve' partitions inputs in ascending workload-key order, so
-- workload @i@ (0-based by key order) receives only inputs whose first element
-- is @i@. This also tests the partition logic itself.
--
-- If any worker observes an input with a foreign workload tag, the test fails
-- immediately. Both 'fetchPayload' (blocking) and 'tryFetchPayload'
-- (non-blocking) paths are exercised on every iteration.
runPipelineIsolationTest
  -- | Path to the JSON config file.
  :: FilePath
  -- | Number of workloads (must match config).
  -> Int
  -- | Test duration in seconds.
  -> Double
  -> IO ()
runPipelineIsolationTest configPath nWorkloads durationSecs = do
  let inputsPerWorkload = 2000
      taggedInputs =
        [ (i, j)
        | i <- [0 :: Int .. nWorkloads - 1]
        , j <- [0 :: Int .. inputsPerWorkload - 1]
        ]
  inputs <- case taggedInputs of
    (t:ts) -> pure (t NE.:| ts)
    []     -> fail "runPipelineIsolationTest: nWorkloads must be >= 1"
  runtime <- resolveConfig configPath inputs
  -- Workloads are stored in a Map, so keys are ascending.
  -- resolve partitions contiguous chunks in the same order.
  let nameToTag = Map.fromList $
        zip (Map.keys (Runtime.workloads runtime)) [0 :: Int ..]
  -- Workers: fetch payload (= input tag), assert it matches the workload.
  -- fetchPayload and tryFetchPayload recycle consumed inputs automatically
  -- (see 'TargetWorker'); the worker only checks the tag. Both blocking and
  -- non-blocking paths are exercised on every iteration, verifying closed-loop
  -- recycling in both code paths.
  _ <- runTest runtime durationSecs $
    \workload _target fetchPayload tryFetchPayload -> do
      let wlName      = Runtime.workloadName workload
          expectedTag = nameToTag Map.! wlName
          check (wlIdx, _) =
            when (wlIdx /= expectedTag) $
              fail $ "Input leakage: workload " ++ wlName
                  ++ " (tag " ++ show expectedTag
                  ++ ") received input tagged " ++ show wlIdx
      forever $ do
        tag <- fetchPayload
        check tag
        mTag <- tryFetchPayload
        case mTag of
          Nothing   -> pure ()
          Just tag' -> check tag'
  pure ()

--------------------------------------------------------------------------------
-- Metrics & formatting
--------------------------------------------------------------------------------

-- | Default test duration in seconds
-- (overridable via PULL_FICTION_TEST_DURATION_SECS).
defaultDuration :: Double
defaultDuration = 60.0

-- | Read test duration from the @PULL_FICTION_TEST_DURATION_SECS@ environment
-- variable, falling back to 'defaultDuration' (60 s).
getDuration :: IO Double
getDuration = do
  env <- lookupEnv "PULL_FICTION_TEST_DURATION_SECS"
  pure $ maybe defaultDuration
    (\s -> maybe defaultDuration id (readMaybe s)) env

-- | Format a duration as a compact string for test group titles (e.g. @60.0@
-- becomes @\"60s\"@, @5.0@ becomes @\"5s\"@).
formatDuration :: Double -> String
formatDuration d = show (round d :: Int) ++ "s"

-- | Format a full metrics summary as a string.
-- Suitable for use as the result description in 'testCaseInfo'.
formatMetrics
  :: Double                -- ^ Configured test duration in seconds.
  -> Map.Map String Double -- ^ Expected TPS per target.
  -> TestResult -> String
formatMetrics cfgDuration expectedTps r = intercalate "\n"
    [ "Global"
    , "  targets:          " ++ show (Map.size (targetCounts r))
    , "  duration:         " ++ formatFixed 2 dur ++ " s"
                             ++ " (target " ++ formatFixed 0 cfgDuration
                             ++ " s, " ++ formatSignedPct durErr ++ "%)"
    , "  configured TPS:   " ++ show (round cfgTps :: Int)
    , "  actual TPS:       " ++ show (round actualTps :: Int)
                             ++ " (" ++ formatSignedPct tpsErr ++ "%)"
    , "  total tokens:     " ++ show totalTokens
                             ++ " (expected " ++ show expected
                             ++ ", " ++ formatSignedPct tokenErr ++ "%)"
    , "Per-target tokens"
    , "  mean:             " ++ show (round tMean :: Int)
                             ++ biasT (round tMean :: Int)
    , "  min:              " ++ show tMin ++ biasT tMin
    , "  max:              " ++ show tMax ++ biasT tMax
    , "  spread (max-min): " ++ show tSpread
                             ++ " (" ++ formatFixed 1 tSpreadPct ++ "% of ideal)"
    , "  worst deviation:  " ++ formatFixed 1 tWorstDev ++ "% from mean"
    , "  std deviation:    " ++ show (round tStddev :: Int)
    , "  CV:               " ++ formatFixed 2 tCv ++ "%"
    , "Per-target TPS"
    , "  mean:             " ++ formatFixed tpsDp sMean ++ biasS sMean
    , "  min:              " ++ formatFixed tpsDp sMin ++ biasS sMin
    , "  max:              " ++ formatFixed tpsDp sMax ++ biasS sMax
    , "  spread (max-min): " ++ formatFixed tpsDp sSpread
                             ++ " (" ++ formatFixed 1 sSpreadPct ++ "% of ideal)"
    , "  worst deviation:  " ++ formatFixed 1 sWorstDev ++ "% from mean"
    , "  std deviation:    " ++ formatFixed tpsDp sStddev
    , "  CV:               " ++ formatFixed 2 sCv ++ "%"
    ]
  where
    durErr      = (elapsedSeconds r - cfgDuration)
                / cfgDuration * 100
    totalTokens = sum (Map.elems (targetCounts r))
    cfgTps      = sum (Map.elems expectedTps)
    actualTps   = fromIntegral totalTokens / elapsedSeconds r
    tpsErr      = (actualTps - cfgTps) / cfgTps * 100
    expected    = round (cfgTps * elapsedSeconds r) :: Int
    tokenErr    = (fromIntegral totalTokens - fromIntegral expected)
                / fromIntegral expected * 100 :: Double
    counts      = Map.elems (targetCounts r)
    dur         = elapsedSeconds r
    n           = fromIntegral (length counts) :: Double
    -- Ideal per-target values (mean of expectedTps).
    idealTps    = sum (Map.elems expectedTps)
                / fromIntegral (Map.size expectedTps)
    idealTokens = idealTps * dur
    -- Token stats
    tMean       = fromIntegral (sum counts) / n
    tMin        = minimum counts
    tMax        = maximum counts
    tSpread     = tMax - tMin
    tSpreadPct  = fromIntegral tSpread / idealTokens * 100
    tWorstDev   = maximum
      (map (\c -> abs (fromIntegral c - tMean)
                / tMean) counts) * 100
    tVariance   = sum (map (\c -> (fromIntegral c - tMean) ** 2) counts) / n
    tStddev     = sqrt tVariance
    tCv         = tStddev / tMean * 100
    biasT v     = let d = fromIntegral v - idealTokens
                      p' = d / idealTokens * 100
                  in  " (ideal " ++ show (round idealTokens :: Int)
                      ++ ", " ++ formatSignedPct p' ++ "%)"
    -- TPS stats (tokens / duration per target)
    tpsList     = map (\c -> fromIntegral c / dur) counts :: [Double]
    sMean       = sum tpsList / n
    sMin        = minimum tpsList
    sMax        = maximum tpsList
    sSpread     = sMax - sMin
    sSpreadPct  = sSpread / idealTps * 100
    sWorstDev   = maximum (map (\s -> abs (s - sMean) / sMean) tpsList) * 100
    sVariance   = sum (map (\s -> (s - sMean) ** 2) tpsList) / n
    sStddev     = sqrt sVariance
    sCv         = sStddev / sMean * 100
    -- Decimal places: use 0 when per-target TPS >= 1, otherwise enough to show
    -- the leading significant digit plus one extra for resolution (e.g. 0.2
    -- TPS -> 2 dp so min/max/spread are distinguishable).
    tpsDp       = if idealTps >= 1 then 0
                  else max 1 (ceiling (negate (logBase 10 idealTps)) + 1 :: Int)
    biasS v     = let d = v - idealTps
                      p' = d / idealTps * 100
                  in  " (ideal " ++ formatFixed tpsDp idealTps
                      ++ ", " ++ formatSignedPct p' ++ "%)"

-- | Format a 'Double' with exactly @n@ decimal places, rounding half-up.
--
-- >>> formatFixed 2 3.1415
-- "3.14"
-- >>> formatFixed 0 99.7
-- "100"
formatFixed :: Int -> Double -> String
formatFixed 0 x = show (round x :: Int)
formatFixed decimals x =
  let factor = 10 ^ decimals :: Int
      scaled = round (x * fromIntegral factor) :: Int
      (whole, frac) = scaled `quotRem` factor
      fracStr = let s = show (abs frac)
                in  replicate (decimals - length s) '0' ++ s
  in  (if x < 0 && whole == 0 then "-" else "") ++ show whole ++ "." ++ fracStr

-- | Format a percentage value with a leading sign (@+@ or @-@) and one decimal
-- place. Used in metrics output to show relative deviations.
--
-- >>> formatSignedPct 3.14
-- "+3.1"
-- >>> formatSignedPct (-0.5)
-- "-0.5"
formatSignedPct :: Double -> String
formatSignedPct x = (if x >= 0 then "+" else "") ++ formatFixed 1 x

--------------------------------------------------------------------------------
-- Assertions (pure)
--------------------------------------------------------------------------------

-- | Check that the elapsed wall-clock time is within the given relative
-- tolerance of the configured duration. Returns 'Nothing' on success, or 'Just'
-- an error message on failure.
--
-- A test that overshoots significantly (e.g. 231s vs 60s) indicates that the
-- rate-limiting mechanism cannot keep up: the feeder loop overhead exceeds the
-- target inter-tick delay.
checkElapsedTolerance
  :: Double  -- ^ Tolerance (e.g. 0.05 for 5%).
  -> Double  -- ^ Configured test duration in seconds.
  -> TestResult -> Maybe String
checkElapsedTolerance tolerance cfgDuration result
  | abs pctErr / 100 <= tolerance = Nothing
  | otherwise = Just $
      "elapsed " ++ formatFixed 1 actual ++ " s ("
      ++ (if pctErr >= 0 then "+" else "") ++ show (round pctErr :: Int)
      ++ "%) vs target " ++ formatFixed 0 cfgDuration ++ " s"
  where
    actual = elapsedSeconds result
    pctErr = (actual - cfgDuration) / cfgDuration * 100

-- | Check that actual TPS is within the given relative tolerance of configured
-- TPS. Returns 'Nothing' on success, or 'Just' an error message on failure.
checkTpsTolerance
  :: Double                 -- ^ Tolerance (e.g. 0.05 for 5%).
  -> Map.Map String Double  -- ^ Expected TPS per target.
  -> TestResult -> Maybe String
checkTpsTolerance tolerance expectedTps result
  | abs pctErr / 100 <= tolerance = Nothing
  | otherwise = Just $
      "actual " ++ show (round actualTps :: Int) ++ " TPS ("
      ++ (if pctErr >= 0 then "+" else "") ++ show (round pctErr :: Int)
      ++ "%) vs target " ++ show (round cfgTps :: Int)
  where
    totalTokens = sum (Map.elems (targetCounts result))
    cfgTps      = sum (Map.elems expectedTps)
    actualTps   = fromIntegral totalTokens / elapsedSeconds result
    pctErr      = (actualTps - cfgTps) / cfgTps * 100

-- | Check a single target's token count against its expected TPS. Returns
-- 'Nothing' on success, or 'Just' an error message on failure.
--
-- Applies a per-target discrete-distribution continuity correction: the actual
-- token count is an integer, so even a perfect system deviates from a
-- non-integer expected count by at least the rounding distance. We subtract
-- this /quantization floor/ so that the tolerance measures only the /excess/
-- deviation attributable to the scheduling algorithm, not to integer
-- arithmetic.
checkTargetFairness
  :: Double                 -- ^ Tolerance (e.g. 0.10 for 10%).
  -> Map.Map String Double  -- ^ Expected TPS per target.
  -> TestResult -> String -> Maybe String
checkTargetFairness tolerance expectedTps result name
  | excessDev <= tolerance = Nothing
  | otherwise = Just $
      show (round (dev * 100) :: Int) ++ "% from expected "
      ++ show (round expectedCount :: Int)
      ++ " (actual " ++ show actual ++ ")"
  where
    actual        = Map.findWithDefault 0 name (targetCounts result)
    elapsed       = elapsedSeconds result
    eTps          = Map.findWithDefault 0 name expectedTps
    expectedCount = eTps * elapsed
    dev           = abs (fromIntegral actual - expectedCount) / expectedCount
    frac          = expectedCount - fromIntegral (floor expectedCount :: Int)
    qFloor
      | frac == 0 = 0
      | otherwise = min frac (1 - frac) / expectedCount
    excessDev     = max 0 (dev - qFloor)
